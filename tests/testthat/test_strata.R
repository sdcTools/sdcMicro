# TDD for strataVars: sdcMicro's semantics is strictly per stratum -- kAnon
# splits the data and anonymises each stratum independently, so the criterion,
# the model and the optimum all decompose by stratum. Verified against
# kAnon(strataVars=) behaviour on testdata.

test_that("ls_check with strataVars checks the criterion per stratum", {
  # Pooled, (1,1) appears twice -> 2-anonymous. Split by stratum s, each
  # stratum holds one unique record -> both violate.
  x <- data.frame(a = c("1", "1"), b = c("1", "1"), s = c("A", "B"),
                  stringsAsFactors = FALSE)
  pooled <- ls_check(x, keyVars = c("a", "b"), k = 2)
  strat  <- ls_check(x, keyVars = c("a", "b"), k = 2, strataVars = "s")
  expect_true(pooled$ok)
  expect_false(strat$ok)
  expect_equal(strat$fk, c(1, 1))
  expect_setequal(strat$violators, c(1L, 2L))
})

test_that("ls_optimal with strataVars satisfies the per-stratum criterion", {
  skip_if_not_installed("highs")
  data("testdata", package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water")
  res <- ls_optimal(testdata, keyVars = kv, k = 3, strataVars = "sex")
  expect_identical(res$status, "optimal")
  chk <- ls_check(res$xAnon, keyVars = kv, k = 3, strataVars = "sex")
  expect_true(chk$ok)
  # and never worse than sdcMicro under the same strata
  h <- kAnon(testdata, keyVars = kv, strataVars = "sex", k = 3)
  nh <- sum(is.na(h$xAnon)) - sum(is.na(testdata[, kv]))
  expect_lte(res$objective, nh)
})

test_that("the stratified optimum is the sum of the per-stratum optima", {
  skip_if_not_installed("highs")
  data("testdata", package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water")
  res <- ls_optimal(testdata, keyVars = kv, k = 3, strataVars = "sex")
  parts <- vapply(split(testdata, testdata$sex), function(d) {
    ls_optimal(d, keyVars = kv, k = 3)$objective
  }, numeric(1))
  expect_equal(res$objective, sum(parts))
})

test_that("strata leave rows in their original order", {
  skip_if_not_installed("highs")
  x <- data.frame(
    a = c("1", "9", "1", "9"), b = c("1", "9", "2", "9"),
    s = c("A", "B", "A", "B"), stringsAsFactors = FALSE
  )
  res <- ls_optimal(x, keyVars = c("a", "b"), k = 2, strataVars = "s")
  expect_identical(res$xAnon$s, x$s)          # untouched, same order
  expect_identical(dim(res$xAnon), dim(x))
})
