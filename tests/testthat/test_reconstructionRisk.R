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

  # identical to calling the data.frame method on the manipulated keys + weight
  manip <- sdc@manipKeyVars
  manip$.w <- testdata$sampling_weight
  rr2 <- reconstructionRisk(manip, keyVars = seq_along(kv), w = ncol(manip))
  expect_equal(rr$risk, rr2$risk)

  expect_warning(reconstructionRisk(sdc, keyVars = 1:2), "ignored")
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
