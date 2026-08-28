context("reconstructionRisk")

make_dat <- function(n = 300, seed = 1) {
  set.seed(seed)
  z <- sample(1:6, n, replace = TRUE)                       # latent group -> dependence
  jit <- function(p) ifelse(runif(n) < p, z, sample(1:6, n, replace = TRUE))
  data.frame(
    age = jit(0.7), sex = sample(1:2, n, replace = TRUE),
    reg = jit(0.7), occ = jit(0.5)
  )
}

test_that("output structure and bounds bracket the reference risk", {
  dat <- make_dat()
  dat$occ[sample(nrow(dat), 40)] <- NA
  kv <- c("age", "sex", "reg", "occ")
  rr <- reconstructionRisk(dat, keyVars = kv, survey = FALSE)

  expect_s3_class(rr, "reconstructionRisk")
  expect_length(rr$risk, nrow(dat))
  expect_named(rr$accuracy, kv)
  expect_true(all(rr$accuracy >= 0 & rr$accuracy <= 1))
  # bounds bracket the reference, and are themselves ordered
  expect_true(all(rr$risk_lower <= rr$risk + 1e-9))
  expect_true(all(rr$risk <= rr$risk_upper + 1e-9))
  expect_true(all(rr$risk_lower <= rr$risk_upper + 1e-9))
  expect_true(all(rr$risk >= 0 & rr$risk <= 1 + 1e-9))
})

test_that("no missing values => reference == both bounds == standard risk", {
  dat <- make_dat()
  kv <- 1:4
  rr <- reconstructionRisk(dat, keyVars = kv, survey = FALSE)
  expect_equal(rr$n_missing, 0L)
  expect_equal(rr$risk, rr$risk_lower)
  expect_equal(rr$risk, rr$risk_upper)
  # matches a direct freqCalc + indivRisk(survey = FALSE) = 1/fk
  fc <- freqCalc(dat, keyVars = kv, alpha = 1)
  expect_equal(rr$risk, 1 / fc$fk)
})

test_that("accuracy = 0 sends missing records to the optimistic bound", {
  dat <- make_dat()
  miss <- sample(nrow(dat), 50); dat$occ[miss] <- NA
  rr <- reconstructionRisk(dat, keyVars = 1:4, accuracy = 0, survey = FALSE)
  m <- is.na(dat$occ)
  expect_equal(rr$risk[m], rr$risk_lower[m])
})

test_that("accuracy = 1 sends missing records to the conservative bound", {
  dat <- make_dat()
  dat$occ[sample(nrow(dat), 50)] <- NA
  rr <- reconstructionRisk(dat, keyVars = 1:4, accuracy = 1, survey = FALSE)
  m <- is.na(dat$occ)
  expect_equal(rr$risk[m], rr$risk_upper[m])
})

test_that("reference risk is monotone non-decreasing in reconstruction accuracy", {
  dat <- make_dat()
  dat$occ[sample(nrow(dat), 60)] <- NA
  m <- is.na(dat$occ)
  agg <- function(a) sum(reconstructionRisk(dat, keyVars = 1:4, accuracy = a,
                                            survey = FALSE)$risk[m])
  vals <- vapply(seq(0, 1, 0.25), agg, numeric(1))
  expect_true(all(diff(vals) >= -1e-9))
})

test_that("invalid accuracy is rejected", {
  dat <- make_dat()
  expect_error(reconstructionRisk(dat, keyVars = 1:4, accuracy = c(0.2, 0.3)),
               "one value per key")
  expect_error(reconstructionRisk(dat, keyVars = 1:4, accuracy = 1.5), "\\[0, 1\\]")
})

test_that("default accuracy is higher for a well-predicted key than a random one", {
  set.seed(7); n <- 600
  z <- sample(1:5, n, replace = TRUE)
  dat <- data.frame(
    a = z,                                   # perfectly determined by b
    b = z,
    rnd = sample(1:5, n, replace = TRUE)     # independent noise
  )
  rr <- reconstructionRisk(dat, keyVars = 1:3, survey = FALSE)
  expect_gt(rr$accuracy["a"], rr$accuracy["rnd"])
})

test_that("sdcMicroObj method uses the suppressed manipulated keys + weight", {
  data(testdata, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water", "sex")
  sdc <- createSdcObj(testdata, keyVars = kv, w = "sampling_weight")
  sdc <- localSuppression(sdc)
  rr <- reconstructionRisk(sdc)

  expect_s3_class(rr, "reconstructionRisk")
  expect_true(rr$n_missing > 0)                    # localSuppression introduced NAs
  expect_length(rr$risk, nrow(testdata))
  expect_true(all(rr$risk_lower <= rr$risk + 1e-9))
  expect_true(all(rr$risk <= rr$risk_upper + 1e-9))

  # scenario B: the object passes the original key values on automatically
  expect_equal(rr$scenario, "B (known truth)")
  # identical to the data.frame method on the manipulated keys + weight + original
  manip <- sdc@manipKeyVars
  manip$.w <- testdata$sampling_weight
  rr2 <- reconstructionRisk(manip, keyVars = seq_along(kv), w = ncol(manip),
                            original = testdata[, kv])
  expect_equal(rr$risk, rr2$risk)

  expect_warning(reconstructionRisk(sdc, keyVars = 1:2), "ignored")
})

test_that("scenario B weight is the per-record probability of the true value", {
  set.seed(3); n <- 600
  o <- sample(1:3, n, replace = TRUE)
  a <- ifelse(runif(n) < 0.85, 1L, 2L)            # value 1 common (~85%), value 2 rare
  orig <- data.frame(a = a, o = o)
  rel <- orig; rel$a <- NA_integer_               # suppress 'a' everywhere
  rr <- reconstructionRisk(rel, keyVars = 1:2, original = orig, survey = FALSE)

  expect_equal(rr$scenario, "B (known truth)")
  # records whose true value is the common one are far easier to reconstruct
  expect_gt(mean(rr$reconstruction_prob[a == 1]), mean(rr$reconstruction_prob[a == 2]))
  expect_true(all(rr$reconstruction_prob >= 0 & rr$reconstruction_prob <= 1))
  expect_true(all(rr$risk_lower <= rr$risk + 1e-9) && all(rr$risk <= rr$risk_upper + 1e-9))
})

test_that("measure_risk(reconstruction=TRUE) adds the field; default is unchanged", {
  data(testdata, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water", "sex")
  sdc <- createSdcObj(testdata, keyVars = kv, w = "sampling_weight")
  sdc <- localSuppression(sdc)

  sdc0 <- measure_risk(sdc)                         # default: no reconstruction field
  expect_null(sdc0@risk$reconstruction)

  sdc1 <- measure_risk(sdc, reconstruction = TRUE)  # opt-in: additive field
  rc <- sdc1@risk$reconstruction
  expect_false(is.null(rc))
  expect_length(rc$risk, nrow(testdata))
  expect_true(all(rc$lower <= rc$risk + 1e-9))
  expect_true(all(rc$risk  <= rc$upper + 1e-9))
  # the existing individual-risk output is byte-identical with/without the option
  expect_equal(sdc0@risk$individual, sdc1@risk$individual)
})

test_that("measure_risk data.frame method supports reconstruction", {
  data(testdata, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water", "sex")
  td <- testdata; td$roof[1:30] <- NA              # induce missings in a key
  mr <- measure_risk(td, keyVars = kv, w = "sampling_weight", reconstruction = TRUE)
  expect_false(is.null(mr$reconstruction))
  expect_length(mr$reconstruction$risk, nrow(td))
})

test_that("measure_risk(reconstruction=TRUE) on an sdcMicroObj takes the scenario-B path", {
  data(testdata, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water", "sex")
  sdc <- createSdcObj(testdata, keyVars = kv, w = "sampling_weight")
  sdc <- localSuppression(sdc)
  rc <- measure_risk(sdc, reconstruction = TRUE)@risk$reconstruction
  rr <- reconstructionRisk(sdc)
  expect_equal(rc$scenario, "B (known truth)")
  expect_equal(rc$risk, rr$risk)
  expect_equal(rc$reconstruction_prob, rr$reconstruction_prob)
})

## the ten-record toy population of the paper (Sections 3-4): records 4 and 5 have Q3 blanked;
## their true values are 2 (common: 6 of 10) and 3 (rare: 1 of 10)
toy_example <- function() {
  toy  <- data.frame(Q1 = c(1,1,1,1,1,2,2,2,2,2), Q2 = c(1,1,1,1,1,2,2,2,2,2),
                     Q3 = c(1,1,1,NA,NA,2,2,2,2,2))
  orig <- toy; orig$Q3[4:5] <- c(2, 3)
  list(toy = toy, orig = orig)
}

test_that("default model is the envelope over {marginal, conditional} (paper Table 4.1)", {
  te <- toy_example()
  rr <- reconstructionRisk(te$toy, keyVars = 1:3, survey = FALSE, original = te$orig)
  expect_equal(rr$model, "envelope")
  # the file marginal recovers Q3 = 2 with 0.6 and Q3 = 3 with 0.1; the own-cell
  # conditional gives 1/5 for both, so the envelope is 0.6 and 0.2
  expect_equal(unname(rr$reconstruction_prob[4:5]), c(0.6, 0.2))
  expect_equal(unname(rr$risk), c(rep(1/3, 3), 0.6, 0.2, rep(0.2, 5)))
})

test_that("model = 'conditional' / 'marginal' select one library member; the envelope dominates both", {
  te <- toy_example()
  args <- list(te$toy, keyVars = 1:3, survey = FALSE, original = te$orig)
  cond <- do.call(reconstructionRisk, c(args, model = "conditional"))
  marg <- do.call(reconstructionRisk, c(args, model = "marginal"))
  env  <- do.call(reconstructionRisk, c(args, model = "envelope"))
  expect_equal(unname(cond$reconstruction_prob[4:5]), c(0.2, 0.2))  # 1 of 5 in cell (1, 1)
  expect_equal(unname(marg$reconstruction_prob[4:5]), c(0.6, 0.1))
  expect_equal(unname(env$reconstruction_prob),
               pmax(unname(cond$reconstruction_prob), unname(marg$reconstruction_prob)))
  expect_equal(unname(cond$risk[4:5]), c(0.2, 0.2))                 # the Section 4 degeneracy
  expect_error(do.call(reconstructionRisk, c(args, model = "oracle")))
})

test_that("measure_risk(reconstruction=TRUE) passes 'model' through and reports it", {
  data(testdata, package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water", "sex")
  sdc <- createSdcObj(testdata, keyVars = kv, w = "sampling_weight")
  sdc <- localSuppression(sdc)
  rc_default <- measure_risk(sdc, reconstruction = TRUE)@risk$reconstruction
  expect_equal(rc_default$model, "envelope")
  rc <- measure_risk(sdc, reconstruction = TRUE, model = "conditional")@risk$reconstruction
  rr <- reconstructionRisk(sdc, model = "conditional")
  expect_equal(rc$model, "conditional")
  expect_equal(rc$risk, rr$risk)
})

