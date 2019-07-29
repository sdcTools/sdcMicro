data(testdata2)
test_that("riskyCells data.frame method / all combinations up to maxDim",{
  r0 <- riskyCells(testdata2, keyVars=c(1:5), threshold=c(50,25,10,5),
    useIdentificationLevel=FALSE, maxDim=4)
  expect_true(r0$unsafe_cells[1]=="1")
  r1 <- riskyCells(testdata2, keyVars=c(1:5), threshold=10,
    useIdentificationLevel=FALSE, maxDim=3)
  expect_true(r1$unsafe_cells[1]=="0")
})

test_that("riskyCells data.frame method / using identification levels",{
  r0 <- riskyCells(testdata2, keyVars=c(1:6), threshold=20,
    useIdentificationLevel=TRUE, level=c(1,1,2,3,3,5))
  expect_true(r0$unsafe_cells[1]=="0")
  r1 <- riskyCells(testdata2, keyVars=c(1,3,4,6), threshold=10,
  useIdentificationLevel=TRUE, level=c(1,2,2,4))
  expect_true(r1$unsafe_cells[1]=="0")
})

testdata2[1:6] <- lapply(1:6, function(x) {
  testdata2[[x]] <- as.factor(testdata2[[x]])
})
sdc <- createSdcObj(testdata2,
                    keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
                    numVars=c('expend','income','savings'), w='sampling_weight')

test_that("riskyCells sdcMicroObj-method / all combinations up to maxDim",{
  
  r0 <- riskyCells(sdc, useIdentificationLevel=FALSE, threshold=c(20,10,5), maxDim=3)
  expect_true(r0$unsafe_cells[1]=="0")
  
})

test_that("riskyCells in case key-variables have been modified, we get counts for original and modified data",{
  sdc <- groupAndRename(sdc, var="roof", before=c("5","6","9"), after=c("5+"))
  r1 <- riskyCells(sdc, useIdentificationLevel=FALSE, threshold=c(10,5,3), maxDim=3)
  expect_true(r1$unsafe_cells_orig[1]=="0")
})

test_that("riskyCells sdcMicroObj-method / using identification levels",{
  sdc <- groupAndRename(sdc, var="roof", before=c("5","6","9"), after=c("5+"))
  r0 <- riskyCells(sdc, useIdentificationLevel=TRUE, threshold=10, level=c(c(1,1,3,4,5,5,5)))
  expect_true(r0$unsafe_cells_orig[1]=="0")
})


