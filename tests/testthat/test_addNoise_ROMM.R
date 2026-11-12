library(sdcMicro)

# ROMM as implemented since 5.8.2 (issue #271): mean and covariance preserving,
# following the QR formulation suggested by Oyvind Langsrud (R package RegSDC).

# the time limit makes a regression to the former O(n^3) algorithm fail instead of hang
romm <- function(x, p = 0.001) {
  setTimeLimit(elapsed = 30, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf), add = TRUE)
  addNoise(x, method = "ROMM", p = p)$xm
}

num_vars <- c("expend", "income", "savings")

test_that("ROMM preserves the sample means and the covariance matrix exactly", {
  data(testdata)
  y <- testdata[1:300, num_vars]
  set.seed(271)
  for (p in c(0.001, 0.05, 1)) {
    xm <- romm(y, p = p)
    expect_equal(dim(xm), dim(y))
    expect_equal(colnames(xm), num_vars)
    expect_equal(unname(colMeans(xm)), unname(colMeans(y)), tolerance = 1e-8)
    expect_equal(unname(cov(xm)), unname(cov(y)), tolerance = 1e-8)
    expect_false(isTRUE(all.equal(unname(as.matrix(xm)), unname(as.matrix(y)))))
  }
})

test_that("the amount of perturbation of ROMM is governed by p", {
  data(testdata)
  y <- testdata[1:300, num_vars]
  ym <- as.matrix(y)
  sds <- apply(ym, 2, sd)
  rel_change <- function(p) {
    mean(abs(sweep(romm(y, p = p) - ym, 2, sds, "/")))
  }
  set.seed(271)
  expect_lt(rel_change(1e-8), 1e-5)
  expect_lt(rel_change(0.001), rel_change(0.02))
  # p = 0 means no perturbation
  expect_equal(unname(romm(y, p = 0)), unname(ym))
})

test_that("ROMM does not flip the sign of a variable's deviations", {
  # for moderate p the perturbed values must stay positively correlated with the
  # original ones; a sign flip of a column of the orthonormal basis would give a
  # negative correlation
  data(testdata)
  y <- testdata[, num_vars]
  for (seed in 1:20) {
    set.seed(seed)
    xm <- romm(y, p = 0.01)
    expect_true(all(diag(cor(xm, as.matrix(y))) > 0), info = paste("seed", seed))
  }
})

test_that("ROMM is fast (no n x n matrix)", {
  skip_on_cran()
  data(testdata)
  y <- testdata[, num_vars]
  elapsed <- system.time(xm <- romm(y))[["elapsed"]]
  expect_lt(elapsed, 1)
  expect_equal(unname(colMeans(xm)), unname(colMeans(y)), tolerance = 1e-8)
})

test_that("ROMM gives the same result for the same seed", {
  data(testdata)
  y <- testdata[1:300, num_vars]
  set.seed(1)
  a <- romm(y)
  set.seed(1)
  b <- romm(y)
  set.seed(2)
  d <- romm(y)
  expect_identical(a, b)
  expect_false(identical(a, d))
})

test_that("ROMM perturbs the complete rows only and warns about the others", {
  data(testdata)
  y <- testdata[1:300, num_vars]
  y[c(5, 17), "income"] <- NA
  y[40, ] <- NA
  incomplete <- c(5, 17, 40)
  set.seed(271)
  expect_warning(xm <- romm(y, p = 0.05), "3 rows with missing values")

  # rows with missing values are returned as they are
  expect_equal(unname(xm[incomplete, ]), unname(as.matrix(y[incomplete, ])))
  # means and covariance of the complete rows are preserved
  yc <- as.matrix(y[-incomplete, ])
  xc <- xm[-incomplete, ]
  expect_equal(unname(colMeans(xc)), unname(colMeans(yc)), tolerance = 1e-8)
  expect_equal(unname(cov(xc)), unname(cov(yc)), tolerance = 1e-8)
  expect_true(all(xc[, 1] != yc[, 1]))
})

test_that("ROMM needs more complete rows than variables", {
  y <- as.data.frame(matrix(runif(9), nrow = 3))
  expect_error(romm(y), "more complete rows than variables")
  y <- as.data.frame(matrix(runif(30), nrow = 10))
  y[, 2] <- NA
  expect_error(suppressWarnings(romm(y)), "more complete rows than variables")
})

test_that("ROMM copes with constant and collinear variables", {
  data(testdata)
  y <- testdata[1:300, num_vars]
  y$const <- 7
  y$twice <- 2 * y$income
  set.seed(271)
  xm <- romm(y, p = 0.05)
  expect_equal(unname(xm[, "const"]), rep(7, 300))
  expect_equal(unname(xm[, "twice"]), unname(2 * xm[, "income"]), tolerance = 1e-8)
  expect_equal(unname(colMeans(xm)), unname(colMeans(y)), tolerance = 1e-8)
  expect_equal(unname(cov(xm)), unname(cov(y)), tolerance = 1e-8)
  expect_true(all(xm[, "income"] != y$income))
})

test_that("ROMM on an sdcMicroObj preserves the means, also for a single variable", {
  data(testdata2)
  sdc <- createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = num_vars, w = "sampling_weight")
  orig <- as.matrix(testdata2[, num_vars])

  set.seed(271)
  res <- addNoise(sdc, method = "ROMM", p = 0.05)
  xm <- as.matrix(get.sdcMicroObj(res, "manipNumVars"))
  expect_equal(unname(colMeans(xm)), unname(colMeans(orig)), tolerance = 1e-8)
  expect_equal(unname(cov(xm)), unname(cov(orig)), tolerance = 1e-8)
  expect_true(all(xm != orig))

  res1 <- addNoise(sdc, variables = "income", method = "ROMM", p = 0.05)
  xm1 <- as.matrix(get.sdcMicroObj(res1, "manipNumVars"))
  expect_equal(mean(xm1[, "income"]), mean(orig[, "income"]), tolerance = 1e-8)
  expect_equal(var(xm1[, "income"]), var(orig[, "income"]), tolerance = 1e-8)
  expect_true(all(xm1[, "income"] != orig[, "income"]))
  expect_identical(xm1[, "expend"], orig[, "expend"])
})
