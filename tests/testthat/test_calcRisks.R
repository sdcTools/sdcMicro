# sdc without missing
set.seed(3)
sample_Data <- runif(30, 1, 100)
sample_Data <- array(sample_Data, dim = c(10,3))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
sample_Data <- createSdcObj(sample_Data, keyVars = c("Var4", "Var5", "Var6"), numVars = c("Var1", "Var2", "Var3"))


test_that("sdc without missing, DisFraction = default)", {
  sample_Data <- calcRisks(sample_Data)
  expect_equal(sample_Data@risk$global$risk, 0.8)
  expect_equal(sample_Data@risk$global$risk_ER, 8)
  expect_equal(sample_Data@risk$global$risk_pct, 80)
  expect_equal(sample_Data@risk$global$threshold, 0)
  expect_equal(sample_Data@risk$global$max_risk, 0.01)
  expect_equal(sample_Data@risk$individual[,1], c(0.5, 1.0, 0.5, 1.0, 1.0, 0.5, 1.0, 0.5, 1.0, 1.0))
  expect_equal(sample_Data@risk$individual[,2], c(2, 1, 2, 1, 1, 2, 1, 2, 1, 1))
  expect_equal(sample_Data@risk$individual[,3], c(2, 1, 2, 1, 1, 2, 1, 2, 1, 1))
  expect_equal(sample_Data@risk$numeric, 1)	
})



# sdc with missing
set.seed(3)
sample_Data <- runif(20, 1, 100)
sample_Data <- array(sample_Data, dim = c(4,5))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 2), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 4), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 6), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 3), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 5), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(4, 1, 2), digits = 0))
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9", "Var10", "Var11")
sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
sample_Data[,7] <- as.factor(as.numeric(sample_Data[,7]))
sample_Data[,8] <- as.factor(as.numeric(sample_Data[,8]))
sample_Data[,9] <- as.factor(as.numeric(sample_Data[,9]))
sample_Data[,10] <- as.factor(as.numeric(sample_Data[,10]))
sample_Data[,11] <- as.factor(as.numeric(sample_Data[,11]))
sample_Data[c(1,3,4),6] <- NA
sample_Data[c(2,3),7] <- NA
sample_Data[c(3),8] <- NA
sample_Data[c(1,3),9] <- NA
sample_Data[c(2,3),10] <- NA
sample_Data[c(3),11] <- NA
sample_Data <- createSdcObj(sample_Data, keyVars = c("Var6", "Var7", "Var8", "Var9", "Var10", "Var11"), numVars=c("Var1", "Var2", "Var3", "Var4", "Var5"))


test_that("sdc without missing, DisFraction = default)", {
  sample_Data <- calcRisks(sample_Data)
  expect_equal(sample_Data@risk$global$risk, 0.4375)
  expect_equal(sample_Data@risk$global$risk_ER, 1.75)
  expect_equal(sample_Data@risk$global$risk_pct, 43.75)
  expect_equal(sample_Data@risk$global$threshold, 0)
  expect_equal(sample_Data@risk$global$max_risk, 0.01)
  expect_equal(sample_Data@risk$individual[,1], c(0.50, 0.50, 0.25, 0.50))
  expect_equal(sample_Data@risk$individual[,2], c(2, 2, 4, 2))
  expect_equal(sample_Data@risk$individual[,3], c(2, 2, 4, 2))
  expect_equal(sample_Data@risk$numeric, 1)	
})