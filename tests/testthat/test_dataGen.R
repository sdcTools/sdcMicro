#load library
library(testthat)

#load dataset
data("testdata2")







###########################################################################################################################################
test_that("data frame method v/s method on sdcMicro object, numeric values", {
  #create a dataset with numeric values
  data_numeric <- as.data.frame(testdata2)
  
  set.seed(3)
  fr <- dataGen(data_numeric[,c(10,11,12)])
  sdc <- createSdcObj(data_numeric, keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), numVars=c('expend','income','savings'), w='sampling_weight')
  set.seed(3)
  sdc <- dataGen(sdc)
  expect_equal(as.data.frame(fr), as.data.frame(sdc@manipNumVars))
})
###########################################################################################################################################
