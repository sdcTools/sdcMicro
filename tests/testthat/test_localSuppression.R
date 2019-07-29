test_that("localSuppression for objects of class sdcMicro, no stratification", {
  data(testdata2)
  sdc <- createSdcObj(testdata2,
                      keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
                      numVars=c('expend','income','savings'), w='sampling_weight')
  sdc <- localSuppression(sdc)
  sdcData <- extractManipData(sdc)
  expect_true(any(apply(sdcData[,c('urbrur','roof','walls','water','electcon','relat','sex')],2,function(x)any(is.na(x)))))
})

test_that("localSuppression for objects of class sdcMicro, with stratification", {
  data(testdata2)
  testdata2$ageG <- cut(testdata2$age, 5, labels=paste0("AG",1:5))
  sdc <- createSdcObj(testdata2,
                      keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
                      numVars=c('expend','income','savings'), w='sampling_weight',
                      strataVar='ageG')
  sdc <- localSuppression(sdc)
  sdcData <- extractManipData(sdc)
  expect_true(any(apply(sdcData[,c('urbrur','roof','walls','water','electcon','relat','sex')],2,function(x)any(is.na(x)))))
})

test_that("localSuppression for k-anonymity for subsets of key-variables", {
  data(testdata2)
  testdata2$ageG <- cut(testdata2$age, 5, labels=paste0("AG",1:5))
  sdc <- createSdcObj(testdata2,
                      keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
                      numVars=c('expend','income','savings'), w='sampling_weight',
                      strataVar='ageG')
  combs <- 5:3
  k <- c(10,20,30)
  
  sdc <- localSuppression(sdc, k=k, combs=combs)
  
  sdcData <- extractManipData(sdc)
  expect_true(any(apply(sdcData[,c('urbrur','roof','walls','water','electcon','relat','sex')],2,function(x)any(is.na(x)))))
  
})

test_that("localSuppression for objects of data frame no stratification", {
  data(testdata2)
  testdata2$ageG <- cut(testdata2$age, 5, labels=paste0("AG",1:5))
  keyVars <- c("urbrur","roof","walls","water","electcon","relat","sex")
  strataVars <- c("ageG")
  inp <- testdata2[,c(keyVars, strataVars)]
  ls <- localSuppression(inp, keyVars=1:7)
  expect_true(ls$newSupps==97)
})

test_that("localSuppression for objects of data frame with stratification", {
  data(testdata2)
  testdata2$ageG <- cut(testdata2$age, 5, labels=paste0("AG",1:5))
  keyVars <- c("urbrur","roof","walls","water","electcon","relat","sex")
  strataVars <- c("ageG")
  inp <- testdata2[,c(keyVars, strataVars)]
  ls <- kAnon(inp, keyVars=1:7, strataVars=8)
  expect_true(ls$newSupps==109)
})

 