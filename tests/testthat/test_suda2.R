test_that("suda2 data frame",{
  data(testdata2)
  data_suda2 <- suda2(testdata2,variables=c("urbrur","roof","walls","water","sex"))
  expect_true(data_suda2$score[1]==2)
})

test_that("suda2 sdcObject non original scores",{
## for objects of class sdcMicro:
  data(testdata2)
  sdc <- createSdcObj(testdata2,
    keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
    numVars=c('expend','income','savings'), w='sampling_weight')
  sdc <- suda2(sdc, original_scores=FALSE)
  expect_true(round(sdc@risk$suda2$score[1],2)==2.73)
})

test_that("suda2 sdcObject  original scores",{
  ## for objects of class sdcMicro:
  data(testdata2)
  sdc <- createSdcObj(testdata2,
                      keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
                      numVars=c('expend','income','savings'), w='sampling_weight')
  sdc <- suda2(sdc, original_scores=TRUE)
  expect_true(round(sdc@risk$suda2$score[1],2)==180)
})
