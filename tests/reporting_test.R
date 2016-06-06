require(sdcMicro)
set.seed(199723)
activedataset <- testdata
sdcObject <- createSdcObj(activedataset, keyVars=c("urbrur", "roof", "sex", "age"),
  numVars=c("expend", "income", "savings"),
  weightVar=c("sampling_weight"), hhId=c("ori_hid"))
sdcObject <- varToFactor(sdcObject, var=c("urbrur"))
sdcObject <- varToFactor(sdcObject, var=c("roof"))
sdcObject <- varToFactor(sdcObject, var=c("sex"))
sdcObject <- groupAndRename(sdcObject, var=c("sex"), before=c("1"), after=c("m"))
sdcObject <- groupAndRename(sdcObject, var=c("sex"), before=c("2"), after=c("w"))
sdcObject <- globalRecode(sdcObject, column=c("age"), breaks=c(6), labels=paste0("AGE",1:6))
sdcObject <- groupAndRename(sdcObject, var=c("age"), before=c("AGE5", "AGE6"), after=c("AGE5_6"))
sdcObject <- localSuppression(sdcObject, k=c(2), importance=c(1, 3, 1, 3))
sdcObject <- localSuppression(sdcObject, k=c(3), importance=c(1, 3, 1, 3))
sdcObject <- microaggregation(sdcObject, aggr=c(3), method=c("mdav"),
  variables=c("expend"), strata_variables=c("sex"))
