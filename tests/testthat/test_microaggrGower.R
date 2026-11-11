# maGowerWORK() must pass the number of levels of each ordered factor to
# VIM::gowerD() (levOrders), not the label of its last level.
test_that("microaggrGower works with ordered factors with non-numeric labels", {
  set.seed(1)
  n <- 60
  df <- data.frame(
    edu = factor(sample(c("low", "mid", "high"), n, replace = TRUE),
                 levels = c("low", "mid", "high"), ordered = TRUE),
    sex = factor(sample(c("f", "m"), n, replace = TRUE)),
    age = sample(18:80, n, replace = TRUE),
    income = round(rlnorm(n, 10, 1))
  )
  res <- microaggrGower(df, variables = c("age", "income"),
                        dist_var = c("edu", "sex", "age", "income"), aggr = 3)
  expect_equal(dim(res), dim(df))
  expect_false(isTRUE(all.equal(res$income, df$income)))
})

test_that("microaggrGower example with ordered testdata runs", {
  data(testdata, package = "sdcMicro")
  for (i in c(1:7, 9)) testdata[, i] <- as.ordered(testdata[, i])
  sdc <- createSdcObj(testdata,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = c("expend", "income", "savings"), w = "sampling_weight")
  expect_s4_class(microaggrGower(sdc), "sdcMicroObj")
})
