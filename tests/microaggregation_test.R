require(sdcMicro)
### for a data.frame
dat <- data.frame(x = rnorm(100))
microaggregation(obj = dat, variables = "x", method = "mdav", aggr = 3, measure = "mean")
dat <- data.frame(x = rnorm(100), y = rnorm(100))
microaggregation(obj = dat, variables = "x", method = "mdav", aggr = 3, measure = "mean")
microaggregation(obj = dat, variables = c("x", "y"), method = "mdav", aggr = 3, measure = "mean")
# for a sdcObj
set.seed(199723)
activedataset <- testdata
sdcObject <- createSdcObj(activedataset, keyVars = c("urbrur", "roof", "sex", "age"),
  numVars = c("expend", "income", "savings"),
  weightVar = c("sampling_weight"), hhId = c("ori_hid"))
sdcObject <- microaggregation(sdcObject, aggr = c(3), method = c("mdav"),
  variables = c("expend"), strata_variables = c("sex"))
