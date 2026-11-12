library(sdcMicro)
data(testdata)

testdata_pram <- function(n = 100, missing = FALSE) {
  set.seed(3)
  df <- data.frame(
    Var1 = runif(n, 1, 100),
    Var2 = runif(n, 1, 100),
    Var3 = as.factor(round(runif(n, 1, 2))),
    Var4 = as.factor(round(runif(n, 1, 4))),
    Var5 = as.factor(round(runif(n, 1, 6))),
    Var6 = as.factor(round(runif(n, 1, 3)))
  )
  if (missing) {
    df[c(1, 3), 3] <- NA
    df[c(2, 5), 5] <- NA
  }
  return(df)
}

test_that("pram handles various input classes consistently", {
  data(testdata)
  testdata$roof <- factor(testdata$roof)
  v <- "roof"
  strata <- c("urbrur", "sex")

  # Factor input
  expect_is(testdata[[v]], "factor")
  res_f <- pram(testdata[[v]])
  expect_s3_class(res_f, "pram")
  expect_false(identical(res_f$x, res_f$x_pram))

  # data.table vs data.frame consistency
  library(data.table)
  dt <- as.data.table(testdata)

  set.seed(1)
  res_dt <- pram(dt, variables = v, strata_variables = strata)
  set.seed(1)
  res_df <- pram(testdata, variables = v, strata_variables = strata)

  expect_identical(res_dt$roof_pram, res_df$roof_pram)
})


test_that("pram responds correctly to pd and alpha changes", {
  sample_Data <- testdata_pram()

  # Test combinations
  set.seed(123)
  def <- pram(sample_Data, variables = "Var3", strata_variables = "Var5")

  set.seed(123)
  low_alpha <- pram(sample_Data, variables = "Var3", strata_variables = "Var5", alpha = 0.1)

  set.seed(123)
  low_pd <- pram(sample_Data, variables = "Var3", strata_variables = "Var5", pd = 0.1)

  # Assertions: results should differ when parameters change
  expect_false(identical(def$Var3_pram, low_alpha$Var3_pram))
  expect_false(identical(def$Var3_pram, low_pd$Var3_pram))
})

test_that("pram respects identity and custom transition matrices", {
  data(testdata)
  testdata$roof <- as.factor(testdata$roof)
  testdata$walls <- as.factor(testdata$walls)
  lvl <- levels(testdata$roof)

  # Identity matrix should result in NO change
  mat_id <- diag(length(lvl))
  rownames(mat_id) <- colnames(mat_id) <- lvl

  res <- pram(testdata, variables = "roof", pd = mat_id)
  expect_identical(res$roof, res$roof_pram)

  # List of matrices for multiple variables
  res_multi <- pram(testdata, variables = c("roof", "walls"),
                    pd = list(mat_id, 0.5), alpha = c(NA, 0.5))
  expect_identical(res_multi$roof, res_multi$roof_pram) # Roof stays same
  expect_false(all(res_multi$walls == res_multi$walls_pram)) # Walls change
})

test_that("pram integrates with sdcMicro objects", {
  data(testdata2)
  testdata2$urbrur <- as.factor(testdata2$urbrur)

  sdc <- createSdcObj(
    dat = testdata2,
    keyVars = c('roof', 'walls'),
    pramVars = "urbrur",
    w = 'sampling_weight'
  )

  sdc <- pram(sdc)

  # Check if the pram slot was populated correctly
  pram_info <- get.sdcMicroObj(sdc, "pram")
  expect_true("urbrur" %in% names(pram_info$comparison))
  expect_s3_class(pram_info$comparison$urbrur, "data.table")
})

test_that("the generated transition matrix is row-stochastic and invariant (#347)", {
  data(testdata)
  x <- factor(testdata$roof)
  set.seed(347)
  res <- pram(x, pd = 0.8, alpha = 1)
  Rs <- attr(res, "pram_params")$x$Rs
  p <- as.numeric(table(x)) / length(x)

  expect_equal(unname(rowSums(Rs)), rep(1, nlevels(x)))
  # invariance refers to the observed distribution, not to column sums
  expect_equal(as.numeric(p %*% Rs), p)
  expect_false(isTRUE(all.equal(unname(colSums(Rs)), rep(1, nlevels(x)))))
})

test_that("alpha = 0 leaves the data unchanged (#347)", {
  data(testdata)
  x <- factor(testdata$roof)
  res <- pram(x, pd = 0.5, alpha = 0)
  Rs <- attr(res, "pram_params")$x$Rs
  expect_equal(unname(Rs), diag(nlevels(x)))
  expect_identical(res$x, res$x_pram)
})

test_that("a user-supplied matrix may deviate from row sums 1 by rounding error (#347)", {
  set.seed(1)
  L <- 49
  x <- factor(sample(seq_len(L), 2000, replace = TRUE), levels = seq_len(L))
  mat <- matrix(1 / L, nrow = L, ncol = L, dimnames = list(levels(x), levels(x)))
  # the row sums are 1 only up to floating-point error
  expect_gt(max(abs(rowSums(mat) - 1)), .Machine$double.eps)
  expect_error(pram(x, pd = mat), NA)

  # a guaranteed diagonal of 0.8, the rest spread evenly
  x12 <- factor(sample(month.abb, 500, replace = TRUE), levels = month.abb)
  mat12 <- matrix(0.2 / 11, nrow = 12, ncol = 12, dimnames = list(month.abb, month.abb))
  diag(mat12) <- 0.8
  expect_error(pram(x12, pd = mat12), NA)

  # a real violation is still rejected
  mat12[1, 2] <- mat12[1, 2] + 1e-4
  expect_error(pram(x12, pd = mat12), "rowSums")
})
