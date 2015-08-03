require(sdcMicro)
set.seed(199723)
activedataset <- testdata
sdcObject <- createSdcObj(activedataset, keyVars = c("urbrur", "roof", "sex", "age"),
  numVars = c("expend", "income", "savings"),
  weightVar = c("sampling_weight"), hhId = c("ori_hid"))
sdcObject <- varToFactor(sdcObject, var = c("urbrur"))
sdcObject <- varToFactor(sdcObject, var = c("roof"))
sdcObject <- varToFactor(sdcObject, var = c("sex"))
sdcObject <- renameVars(sdcObject, var = c("sex"), before = c("1"), after = c("m"))
sdcObject <- renameVars(sdcObject, var = c("sex"), before = c("2"), after = c("w"))
sdcObject <- globalRecode(sdcObject, column = c("age"), breaks = c(6), labels = NULL)
sdcObject <- groupVars(sdcObject, var = c("age"),
  before = c("(63.333333,79.166667]", "(79.166667,95.095]"),
  after = c("(63.333,95.095]", "(63.333,95.095]"))
sdcObject <- localSuppression(sdcObject, k = c(2), importance = c(1, 3, 1, 3))
sdcObject <- localSuppression(sdcObject, k = c(3), importance = c(1, 3, 1, 3))
sdcObject <- microaggregation(sdcObject, aggr = c(3), method = c("mdav"),
  variables = c("expend"), strata_variables = c("sex"))

td <- tempdir()
report(sdcObject, outdir = td)
report(sdcObject, outdir = td, internal = TRUE, filename = "fullreport")
