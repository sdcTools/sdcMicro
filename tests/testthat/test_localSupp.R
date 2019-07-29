test_that("localSupp", {
  data(francdat)
  keyVars <- paste0("Key",1:4)
  f <- freqCalc(francdat, keyVars=keyVars,w=8)
  indivf <- indivRisk(f)
## Local Suppression
  localS <- localSupp(f, keyVar="Key4", threshold=0.15)
  f2 <- freqCalc(localS$freqCalc, keyVars=keyVars, w=8)
  indivf2 <- indivRisk(f2)
  expect_true(all(indivf$rk-indivf2$rk>=0))
})


#'
## select another keyVar and run localSupp once again,
# if you think the table is not fully protected
#'
## for objects of class sdcMicro:
test_that("localSupp sdcMicroObj", {
  data(testdata)
  sdc <- createSdcObj(testdata,
    keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
    numVars=c('expend','income','savings'), w='sampling_weight')
  sdc <- localSupp(sdc, keyVar='urbrur', threshold=0.045)
  expect_true(sum(sdc@localSuppression$totalSupps)>0)
})
