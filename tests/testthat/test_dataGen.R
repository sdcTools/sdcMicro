# data frame

test_that("data frame without missing)", {
  set.seed(3)
  sample_Data <- runif(30, 1, 100)
  sample_Data <- array(sample_Data, dim = c(10,3))
  sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
  sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
  sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
  sample_Data <- as.data.frame(sample_Data)
  colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
  sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
  sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
  sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
  set.seed(1243)
  fr <- dataGen(sample_Data[,c(1,2,3)])
  expect_false(all(fr[,1] == sample_Data[,1]))
  expect_false(all(fr[,2] == sample_Data[,2]))
  expect_false(all(fr[,3] == sample_Data[,3]))
})



# sdc

test_that("sdc without missing)", {
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
  set.seed(1243)
  fr <- dataGen(sample_Data)
  expect_equal(fr@manipNumVars[,1], c(37.37374, 23.08232, 30.01385, 64.14303, 41.04428, 32.67596, 26.38621, 31.66273, 84.17742, 14.02504), tolerance = 0.001)
  expect_equal(fr@manipNumVars[,2], c(57.09763, 42.42032, 24.10131, 88.64455, 80.98905, 56.30633, 42.34409, 62.39842, 75.37993, 23.66572), tolerance = 0.001)
  expect_equal(fr@manipNumVars[,3], c(74.619229, 80.780970, 102.426466, 40.615803, 100.663470, 18.847175, 120.169279, 7.698377, 9.155260, 33.304897), tolerance = 0.001)
  expect_false(all(fr@manipNumVars[,1] == sample_Data@manipNumVars[,1]))
  expect_false(all(fr@manipNumVars[,2] == sample_Data@manipNumVars[,2]))
  expect_false(all(fr@manipNumVars[,3] == sample_Data@manipNumVars[,3]))
})