# no missing data
set.seed(123)
sample_Data <- round(runif(30, 1, 4), digits = 0)
sample_Data <- array(sample_Data, dim = c(10,3))
sample_Data <- as.data.frame(sample_Data)
colnames(sample_Data) <- c("Var1", "Var2", "Var3")
sample_Data[,1] <- as.factor(sample_Data[,1])
sample_Data[,2] <- as.factor(sample_Data[,2])
sample_Data[,3] <- as.factor(sample_Data[,3])

test_that("numeric values, two different levels", {
  sample_Data <- generateStrata(sample_Data,c("Var1","Var2"),"Var12")
  expect_true(all(sample_Data$Var12 == c("2-4", "3-2", "2-3", "4-3", "4-1", "1-4", "3-2", "4-1", "3-2", "2-4"))) 
})

test_that("numeric values, three different levels", {
  sample_Data <- generateStrata(sample_Data,c("Var1","Var2", "Var3"),"Var123")
  expect_true(all(sample_Data$Var123 == c("2-4-4", "3-2-3", "2-3-3", "4-3-4", "4-1-3", "1-4-3", "3-2-3", "4-1-3", "3-2-2", "2-4-1"))) 
})

# missing data
set.seed(123)
sample_Data <- round(runif(30, 1, 4), digits = 0)
sample_Data[c(1,4,13,18,21,25,30)] <- NA
sample_Data <- array(sample_Data, dim = c(10,3))
sample_Data <- as.data.frame(sample_Data)
colnames(sample_Data) <- c("Var1", "Var2", "Var3")
sample_Data[,1] <- as.factor(sample_Data[,1])
sample_Data[,2] <- as.factor(sample_Data[,2])
sample_Data[,3] <- as.factor(sample_Data[,3])

test_that("missing values, two different levels", {
  sample_Data <- generateStrata(sample_Data,c("Var1","Var2"),"Var12")
  expect_true(all(sample_Data$Var12 == c("NA-4", "3-2", "2-NA", "NA-3", "4-1", "1-4", "3-2", "4-NA", "3-2", "2-4"))) 
})

test_that("missing values, three different levels", {
  sample_Data <- generateStrata(sample_Data,c("Var1","Var2", "Var3"),"Var123")
  expect_true(all(sample_Data$Var123 == c("NA-4-NA", "3-2-3", "2-NA-3", "NA-3-4", "4-1-NA", "1-4-3", "3-2-3", "4-NA-3", "3-2-2", "2-4-NA"))) 
})