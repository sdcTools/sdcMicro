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
