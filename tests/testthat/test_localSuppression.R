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

test_that("localSuppression for k-anonymity for subsets of key-variables without stratification", {
  data(testdata2)
  testdata2$ageG <- cut(testdata2$age, 5, labels=paste0("AG",1:5))
  sdc <- createSdcObj(testdata2,
                      keyVars=c('urbrur','roof','walls'),
                      numVars=c('expend','income','savings'), w='sampling_weight')
  combs <- 3:2
  k <- c(10,20)
  
  sdc <- localSuppression(sdc, k=k, combs=combs)
  sdcData <- extractManipData(sdc)
  expect_true(any(apply(sdcData[,c('urbrur','roof','walls')],2,function(x)any(is.na(x)))))
  
})


test_that("localSuppression for k-anonymity for subsets of key-variables with stratification", {
  data(testdata2)
  testdata2$ageG <- cut(testdata2$age, 5, labels=paste0("AG",1:5))
  sdc <- createSdcObj(testdata2,
                      keyVars=c('urbrur','roof','walls'),
                      numVars=c('expend','income','savings'), w='sampling_weight',
                      strataVar='ageG')
  combs <- 3
  k <- c(10)
  
  expect_error(sdc <- localSuppression(sdc, k=k, combs=combs))
  
  k <- c(3,4)
  sdcData <- localSuppression(testdata2,keyVars=c('urbrur','roof','walls'),
                              k=k, combs=combs,strataVars="ageG")
  
  expect_true(any(apply(sdcData$xAnon[,c('urbrur','roof','walls')],2,function(x)any(is.na(x)))))
  
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



# dataframe
# dataframe without missing
set.seed(3)
sample_Data <- runif(30, 1, 100)
sample_Data <- array(sample_Data, dim = c(10,3))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
sample_Data <- as.data.frame(sample_Data)
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))

# "dataframe without missing, k = 2
# "dataframe without missing, k = 2, importance=NULL
test_that("dataframe without missing, k = 2, importance=NULL, combs=NULL)", {
  sample_DataX <- localSuppression(sample_Data, keyVars = paste0("Var",4:5), k =2, importance=NULL, combs=NULL)
  expect_equal(sum(sample_DataX$totalSupps),2)
})

test_that("dataframe without missing, k = 2, importance=NULL, combs=c(1:2))", {
  sample_DataX <- localSuppression(sample_Data, keyVars = paste0("Var",4:5), k =2, importance=NULL, combs=c(1:2))
  expect_equal(sum(sample_DataX$totalSupps),2)
})

test_that("dataframe without missing, k = 2, importance=NULL, combs=c(3:2))", {
  expect_error(sample_DataX <- localSuppression(sample_Data, keyVars = paste0("Var",4:5), k =2, importance=NULL, combs=c(3:2)),"at least one element of 'combs' is to large!")
})


# "dataframe without missing, k = 2, importance=c(3,2,1)
test_that("dataframe without missing, k = 2, importance=c(3,2,1), combs=NULL)", {
  sample_DataX <- localSuppression(sample_Data, keyVars = paste0("Var",3:5), k =2, importance=c(3,2,1), combs=NULL)
  expect_equal(sum(sample_DataX$totalSupps),10L)
})

test_that("dataframe without missing, k = 2, importance=c(3,2,1), combs=c(3:2))", {
  sample_DataX <- localSuppression(sample_Data, keyVars = paste0("Var",3:5), k =2, importance=c(3,2,1), combs=c(3:2))
  expect_equal(sum(sample_DataX$totalSupps),11L)
})