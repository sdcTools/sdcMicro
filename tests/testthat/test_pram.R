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
