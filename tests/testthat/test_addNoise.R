#Testing on data frame values

#data frame, numeric values without missing value

#load library
library(testthat)

set.seed(3)
sample_Data <- runif(15, 1, 100)
sample_Data <- array(sample_Data, dim = c(5,3))
sample_Data <- as.data.frame(sample_Data)

###########################################################################################################################################
test_that("data frame, numeric values (default noise, additive method)", {
  set.seed(3)
  fr <- addNoise(sample_Data)
  fr2 <- addNoise(sample_Data, noise = 20)
  expect_true(all(colMeans(abs(sample_Data-fr$xm))>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x)))>mean(colMeans(abs(fr2$xm-fr2$x))))
})
###########################################################################################################################################


###########################################################################################################################################
test_that("data frame, numeric values (noise = default, correlated method)", {
  set.seed(3)
  fr <- addNoise(sample_Data, method = "correlated")
  fr2 <- addNoise(sample_Data, noise = 20, method = "correlated")
  expect_true(all(colMeans(abs(sample_Data-fr$xm))>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x)))>mean(colMeans(abs(fr2$xm-fr2$x))))
})
###########################################################################################################################################


###########################################################################################################################################
test_that("data frame, numeric values (noise = default, correlated2 method)", {
  set.seed(3)
  fr <- addNoise(sample_Data, method = "correlated2")
  fr2 <- addNoise(sample_Data, delta = .01, method = "correlated2")
  expect_true(all(colMeans(abs(sample_Data-fr$xm))>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x)))>mean(colMeans(abs(fr2$xm-fr2$x))))
})
###########################################################################################################################################


###########################################################################################################################################
test_that("data frame, numeric values (noise = default, ROMM method)", {
  set.seed(3)
  fr <- addNoise(sample_Data, method = "ROMM")
  fr2 <- addNoise(sample_Data, noise = 20, method = "ROMM")
  expect_true(all(colMeans(abs(sample_Data-fr$xm))>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x)))>mean(colMeans(abs(fr2$xm-fr2$x))))
})
###########################################################################################################################################


set.seed(3)
sample_Data <- runif(30, 1, 100)
sample_Data <- array(sample_Data, dim = c(10,3))
sample_Data <- as.data.frame(sample_Data)

###########################################################################################################################################
test_that("data frame, numeric values (noise = default, outdect method)", {
  set.seed(3)
  fr <- addNoise(sample_Data, method = "outdect")
  fr2 <- addNoise(sample_Data, noise = 20, method = "outdect")
  expect_true(all(colMeans(abs(sample_Data-fr$xm))>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x)))>mean(colMeans(abs(fr2$xm-fr2$x))))
})
###########################################################################################################################################



#data frame, numeric values with missing value
set.seed(3)
sample_Data <- runif(15, 1, 100)
set.seed(3)
sample_Data[ceiling(runif(8, 1, length(sample_Data)))] <- NA
sample_Data <- array(sample_Data, dim = c(5,3))
sample_Data <- as.data.frame(sample_Data)
###########################################################################################################################################
test_that("data frame, numeric values with some NAs (default noise, additive method)", {
  set.seed(3)
  fr <- addNoise(sample_Data)
  fr2 <- addNoise(sample_Data, noise = 20)
  expect_true(all(colMeans(abs(sample_Data-fr$xm),na.rm=TRUE)>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x),na.rm=TRUE),na.rm=TRUE)>mean(colMeans(abs(fr2$xm-fr2$x),na.rm=TRUE),na.rm=TRUE))
})
###########################################################################################################################################



###########################################################################################################################################
test_that("data frame, numeric values with some NAs (noise = default, correlated2 method)", {
  set.seed(3)
  fr <- addNoise(sample_Data, method = "correlated2")
  fr2 <- addNoise(sample_Data, noise = 20, method = "correlated2")
  expect_true(all(colMeans(abs(sample_Data-fr$xm),na.rm=TRUE)>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x),na.rm=TRUE),na.rm=TRUE)>mean(colMeans(abs(fr2$xm-fr2$x),na.rm=TRUE),na.rm=TRUE))
})
###########################################################################################################################################


###########################################################################################################################################
test_that("data frame, numeric values with some NAs (noise = default, restr method)", {
  set.seed(3)
  fr <- addNoise(sample_Data, method = "restr")
  fr2 <- addNoise(sample_Data, noise = 20, method = "restr")
  expect_true(all(colMeans(abs(sample_Data-fr$xm),na.rm=TRUE)>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x),na.rm=TRUE),na.rm=TRUE)>mean(colMeans(abs(fr2$xm-fr2$x),na.rm=TRUE),na.rm=TRUE))
})
###########################################################################################################################################



set.seed(3)
sample_Data <- runif(30, 1, 100)
set.seed(3)
sample_Data[ceiling(runif(4, 1, length(sample_Data)))] <- NA
sample_Data <- array(sample_Data, dim = c(10,3))
sample_Data <- as.data.frame(sample_Data)
###########################################################################################################################################
test_that("data frame, numeric values with some NAs (noise = default, outdect method)", {
  set.seed(3)
  fr <- addNoise(sample_Data, method = "outdect")
  fr2 <- addNoise(sample_Data, noise = 20, method = "outdect")
  expect_true(all(colMeans(abs(sample_Data-fr$xm),na.rm=TRUE)>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x),na.rm=TRUE),na.rm=TRUE)>mean(colMeans(abs(fr2$xm-fr2$x),na.rm=TRUE),na.rm=TRUE))
})
###########################################################################################################################################

#data frame, numeric values with one entire column, one entire row with NA
set.seed(3)
sample_Data <- runif(15, 1, 100)
sample_Data <- array(sample_Data, dim = c(5,3))
set.seed(3)
sample_Data[,2] <- NA
sample_Data[3,] <- NA
sample_Data <- as.data.frame(sample_Data)
###########################################################################################################################################
test_that("data frame, numeric values with one entire column, one entire row with NAs (default noise, additive method)", {
  set.seed(3)
  expect_warning(fr <- addNoise(sample_Data))
  expect_warning(fr2 <- addNoise(sample_Data, noise = 20))
  expect_true(all(na.omit(colMeans(abs(fr$x-fr$xm),na.rm=TRUE))>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x),na.rm=TRUE),na.rm=TRUE)>mean(colMeans(abs(fr2$xm-fr2$x),na.rm=TRUE),na.rm=TRUE))
})
###########################################################################################################################################


###########################################################################################################################################
test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = default, correlated method)", {
  set.seed(3)
  expect_error(fr <- addNoise(sample_Data, method = "correlated"))
})
###########################################################################################################################################

###########################################################################################################################################
test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = default, correlated2 method)", {
  set.seed(3)
  expect_warning(fr <- addNoise(sample_Data, method = "correlated2"))
  expect_warning(fr2 <- addNoise(sample_Data, noise = 20, method = "correlated2"))
  expect_true(all(na.omit(colMeans(abs(fr$x-fr$xm),na.rm=TRUE))>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x),na.rm=TRUE),na.rm=TRUE)>mean(colMeans(abs(fr2$xm-fr2$x),na.rm=TRUE),na.rm=TRUE))
})
###########################################################################################################################################

###########################################################################################################################################
test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = default, restr method)", {
  set.seed(3)
  fr <- addNoise(sample_Data, method = "restr")
  fr2 <- addNoise(sample_Data, noise = 20, method = "restr")
  expect_true(all(na.omit(colMeans(abs(fr$x-fr$xm),na.rm=TRUE))>0))
  expect_true(mean(colMeans(abs(fr$xm-fr$x),na.rm=TRUE),na.rm=TRUE)>mean(colMeans(abs(fr2$xm-fr2$x),na.rm=TRUE),na.rm=TRUE))
})
###########################################################################################################################################

###########################################################################################################################################
test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = default, ROMM method)", {
  set.seed(3)
  fr <- addNoise(sample_Data, method = "ROMM")
  fr2 <- addNoise(sample_Data, noise = 20, method = "ROMM")
  expect_true(all(is.na(fr$xm)))
})
###########################################################################################################################################

set.seed(3)
sample_Data <- runif(30, 1, 100)
sample_Data <- array(sample_Data, dim = c(10,3))
set.seed(3)
sample_Data[,2] <- NA
sample_Data[3,] <- NA
sample_Data <-as.data.frame(sample_Data)

###########################################################################################################################################
test_that("data frame, numeric values with one entire column, one entire row with NAs (noise = default, outdect method)", {
  set.seed(3)
  fr <- expect_error(addNoise(sample_Data, method = "outdect"))
})
###########################################################################################################################################




##########################################################################################################
#Testing on sdc  values
#sdc object, numeric values without missing value

set.seed(3)
sample_Data <- runif(15, 1, 100)
sample_Data <- array(sample_Data, dim = c(5,3))
colnames(sample_Data) <- c("Var1", "Var2", "Var3")
sample_Data <- as.data.frame(sample_Data)
sample_Data <- createSdcObj(sample_Data, keyVars = c("Var1", "Var2", "Var3"), numVars = c("Var1", "Var2", "Var3"))


###########################################################################################################################################
test_that("sdc object, numeric values (default noise, additive method)", {
  set.seed(3)
  fr <- addNoise(sample_Data)
  fr2 <- addNoise(sample_Data, noise = 20)
  expect_true(all(na.omit(colMeans(abs(sample_Data@origData-fr@manipNumVars),na.rm=TRUE))>0))
  expect_true(mean(colMeans(abs(fr@manipNumVars-sample_Data@origData),na.rm=TRUE),na.rm=TRUE)>mean(colMeans(abs(fr2@manipNumVars-sample_Data@origData),na.rm=TRUE),na.rm=TRUE))
})
###########################################################################################################################################

