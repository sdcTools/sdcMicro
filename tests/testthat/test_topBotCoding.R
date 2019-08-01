# dataframe without missing
set.seed(3)
sample_Data <- runif(30, 1, 100)
sample_Data <- array(sample_Data, dim = c(10,3))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))

test_that("dataframe without missing, kind=top)", {
  fr <- topBotCoding(sample_Data[,1], value=35, replacement=0, kind="top")
  expect_equal(fr, c(17.63611, 0.00000, 0.00000, 33.44570, 0.00000, 0.00000, 13.33871, 30.16549, 0.00000, 0.00000), tolerance=0.001)
})

test_that("dataframe without missing, kind=bottom)", {
  fr <- topBotCoding(sample_Data[,1], value=35, replacement=0, kind="bottom")
  expect_equal(fr, c(0.00000, 80.94412, 39.10929, 0.00000, 60.60797, 60.83501, 0.00000, 0.00000, 58.18338, 63.46695), tolerance=0.001)
})




# dataframe with missing
set.seed(3)
sample_Data <- runif(30, 1, 100)
sample_Data <- array(sample_Data, dim = c(10,3))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
sample_Data[c(1,2,7),1] <- NA
sample_Data[c(2,6,9),2] <- NA
sample_Data[c(2,10),3] <- NA
sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))

test_that("dataframe with missing, kind=top)", {
  fr <- topBotCoding(sample_Data[,1], value=35, replacement=0, kind="top")
  expect_equal(fr, c(NA, NA, 0.00000, 33.44570, 0.00000, 0.00000, NA, 30.16549, 0.00000, 0.00000), tolerance=0.001)
})

test_that("dataframe with missing, kind=bottom)", {
  fr <- topBotCoding(sample_Data[,1], value=35, replacement=0, kind="bottom")
  expect_equal(fr, c(NA, NA, 39.10929, 0.00000, 60.60797, 60.83501, NA, 0.00000, 58.18338, 63.46695), tolerance=0.001)
})



# sdc without missing
set.seed(3)
sample_Data <- runif(30, 1, 100)
sample_Data <- array(sample_Data, dim = c(10,3))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
sample_Data <- createSdcObj(sample_Data, keyVars = c("Var4", "Var5", "Var6"), numVars = c("Var1", "Var2", "Var3"))

test_that("sdc without missing, kind=top)", {
  sample_Data <- topBotCoding(sample_Data, value=35, replacement=0, kind="top", column="Var1")
  expect_equal(sample_Data@manipNumVars$Var1, c(17.63611, 0.00000, 0.00000, 33.44570, 0.00000, 0.00000, 13.33871, 30.16549, 0.00000, 0.00000), tolerance=0.001)
})

test_that("sdc without missing, kind=bottom)", {
  sample_Data <- topBotCoding(sample_Data, value=35, replacement=0, kind="bottom", column="Var1")
  expect_equal(sample_Data@manipNumVars$Var1, c(0.00000, 80.94412, 39.10929, 0.00000, 60.60797, 60.83501, 0.00000, 0.00000, 58.18338, 63.46695), tolerance=0.001)
})









# sdc with missing
set.seed(3)
sample_Data <- runif(30, 1, 100)
sample_Data <- array(sample_Data, dim = c(10,3))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
sample_Data[c(1,2,7),1] <- NA
sample_Data[c(2,6,9),2] <- NA
sample_Data[c(2,10),3] <- NA
sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
sample_Data <- createSdcObj(sample_Data, keyVars = c("Var4", "Var5", "Var6"), numVars = c("Var1", "Var2", "Var3"))

test_that("sdc with missing, kind=top)", {
  sample_Data <- topBotCoding(sample_Data, value=35, replacement=0, kind="top", column="Var1")
  expect_equal(sample_Data@manipNumVars$Var1, c(NA, NA, 0.00000, 33.44570, 0.00000, 0.00000, NA, 30.16549, 0.00000, 0.00000), tolerance=0.001)
})

test_that("sdc with missing, kind=bottom)", {
  sample_Data <- topBotCoding(sample_Data, value=35, replacement=0, kind="bottom", column="Var1")
  expect_equal(sample_Data@manipNumVars$Var1, c(NA, NA, 39.10929, 0.00000, 60.60797, 60.83501, NA, 0.00000, 58.18338, 63.46695), tolerance=0.001)
})