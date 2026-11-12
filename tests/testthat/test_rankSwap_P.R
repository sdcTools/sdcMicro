# P is the rank range as a proportion of the swappable records (issue: docs
# described it as a percentage of the sample size).
dat <- data.frame(a = as.numeric(1:1000))

test_that("P is a proportion: values move by less than P * N_s ranks", {
  for (P in c(0.01, 0.05, 0.1)) {
    sw <- rankSwap(dat, variables = "a", TopPercent = 0, BottomPercent = 0,
                   P = P, seed = 123)
    expect_true(max(abs(sw$a - dat$a)) < P * nrow(dat))
    expect_false(all(sw$a == dat$a))
    expect_setequal(sw$a, dat$a)
  }
})

test_that("P = 0 swaps nothing", {
  sw <- rankSwap(dat, variables = "a", TopPercent = 0, BottomPercent = 0,
                 P = 0, seed = 1)
  expect_equal(sw$a, dat$a)
})

test_that("P > 1 warns (percentage passed by mistake), negative P errors", {
  expect_warning(
    rankSwap(dat, variables = "a", TopPercent = 0, BottomPercent = 0, P = 10, seed = 1),
    "proportion, not a percentage")
  expect_error(
    rankSwap(dat, variables = "a", TopPercent = 0, BottomPercent = 0, P = -0.1),
    "non-negative")
  expect_silent(
    rankSwap(dat, variables = "a", TopPercent = 0, BottomPercent = 0, P = 0.1, seed = 1))
})

test_that("P warning fires once for sdcMicroObj input", {
  data(testdata2, package = "sdcMicro")
  sdc <- createSdcObj(testdata2, keyVars = c("urbrur", "roof", "sex"),
                      numVars = c("expend", "income", "savings"),
                      w = "sampling_weight")
  w <- testthat::capture_warnings(rankSwap(sdc, P = 5, seed = 1))
  expect_equal(sum(grepl("proportion, not a percentage", w)), 1L)
})
