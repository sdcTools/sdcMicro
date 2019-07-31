require(sdcMicro)
### for a data.frame
test_that("pram on a factor", {
  dat <- data.frame(x = rnorm(100))
  datout <- microaggregation(obj = dat, variables = "x", method = "mdav", aggr = 3, measure = "mean")
  expect_identical(dat,datout$x)
  expect_identical(dim(dat),dim(datout$mx))
  
  dat <- data.frame(x = rnorm(100), y = rnorm(100))
  datout <- microaggregation(obj = dat, variables = "x", method = "mdav", aggr = 3, measure = "mean")
  expect_identical(dat,datout$x)
  expect_identical(dim(dat),dim(datout$mx))
  
  datout <- microaggregation(obj = dat, variables = c("x", "y"), method = "mdav", aggr = 3, measure = "mean")
  expect_identical(dat,datout$x)
  expect_identical(dim(dat),dim(datout$mx))
  
  # for a sdcObj
  set.seed(199723)
  activedataset <- testdata
  sdcObject <- createSdcObj(activedataset, keyVars = c("urbrur", "roof", "sex", "age"),
                            numVars = c("expend", "income", "savings"),
                            weightVar = c("sampling_weight"), hhId = c("ori_hid"))
  expect_message(microaggregation(sdcObject, aggr = c(3), method = c("mdav"),
                                variables = c("expend"), strata_variables = c("sex")))
  
})
