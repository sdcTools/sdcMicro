library(testthat)

# data.frame
# dataframe without missing data
set.seed(123)
sample_Data <- round(runif(10, 1, 3), digits = 0)
sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 5), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 100, 100), digits = 0))
sample_Data <- as.data.frame(sample_Data)
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "weights")
sample_Data[,1] <- factor(sample_Data[,1], levels = 1:3, labels = LETTERS[1:3])
sample_Data[,2] <- factor(sample_Data[,2])
sample_Data[,3] <- factor(sample_Data[,3])
sample_Data[,4] <- as.numeric(sample_Data[,4])

formula <- ~Var1+Var2+Var3
w <- "weights"

# bound = none
test_that("data.frame, default method, bound = no", {
  fr <- modRisk(sample_Data, method="default", formulaM=formula, weights = w)
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.553)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.705)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.4, 0.4, 0.8, 0.4, 0.4, 0.4, 0.8, 0.4)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.01)))
})


test_that("data.frame, default method, bound = no", {
  fr <- modRisk(sample_Data, method="CE", formulaM=formula, weights = w)
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.542)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.699)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.10, 0.10, 0.20, 0.10, 0.10, 0.10, 0.21, 0.10, 0.52, 0.52, 0.95, 0.47, 0.52, 0.47, 1.03, 0.52, 0.39, 0.43, 0.85, 0.39, 0.38, 0.38, 0.76, 0.42)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.01)))
})


test_that("data.frame, default method, bound = no", {
  fr <- modRisk(sample_Data, method="PML", formulaM=formula, weights = w)
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.553)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.705)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.4, 0.4, 0.8, 0.4, 0.4, 0.4, 0.8, 0.4)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.01)))
})


test_that("data.frame, default method, bound = no", {
  fr <- modRisk(sample_Data, method="weightedLLM", formulaM=formula, weights = w)
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.408)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.613)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.03, 0.05, 0.01, 0.02, 0.06, 0.13, 0.63, 0.06, 0.53, 1.07, 0.01, 0.02, 1.34, 0.13, 0.65, 1.25, 0.01, 0.55, 2.68, 0.01, 0.03, 0.07, 0.02, 0.63)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.01)))
})


test_that("data.frame, default method, bound = no", {
  fr <- modRisk(sample_Data, method="IPF", formulaM=formula, weights = w)
  expect_true(round(as.numeric(unclass(fr[1])), digits=6)==0.552916)
  expect_true(round(as.numeric(unclass(fr[2])), digits=7)==0.7053842)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.1, 0.5, 0.4, 0.1, 0.5, 0.4, 0.1, 0.5, 0.4, 0.1, 0.5, 0.4, 
                                                                0.2, 1, 0.8, 0.2, 1, 0.8, 0.1, 0.5, 0.4, 0.1, 0.5, 0.4)))
})

# bound = yes
test_that("data.frame, default method, bound = yes", {
  fr <- modRisk(sample_Data, method="default", formulaM=formula, weights = w, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.622)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.794)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.4, 0.4, 0.8, 0.4, 0.4, 0.4, 0.8, 0.4)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)))
})

test_that("data.frame, CE method, bound = yes", {
  fr <- modRisk(sample_Data, method="CE", formulaM=formula, weights = w, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.609)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.786)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.10, 0.10, 0.20, 0.10, 0.10, 0.10, 0.21, 0.10, 0.52, 0.52, 0.95, 0.47, 0.52, 0.47, 1.03, 0.52, 0.39, 0.43, 0.85, 0.39, 0.38, 0.38, 0.76, 0.42)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)))
})

test_that("data.frame, PML method, bound = yes", {
  fr <- modRisk(sample_Data, method="PML", formulaM=formula, weights = w, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.622)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.794)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.4, 0.4, 0.8, 0.4, 0.4, 0.4, 0.8, 0.4)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)))
})

test_that("data.frame, weightedLLM method, bound = yes", {
  fr <- modRisk(sample_Data, method="weightedLLM", formulaM=formula, weights = w, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.459)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.69)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.03, 0.05, 0.01, 0.02, 0.06, 0.13, 0.63, 0.06, 0.53, 1.07, 0.01, 0.02, 1.34, 0.13, 0.65, 1.25, 0.01, 0.55, 2.68, 0.01, 0.03, 0.07, 0.02, 0.63)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)))
})

test_that("data.frame, IPF method, bound = yes", {
  fr <- modRisk(sample_Data, method="IPF", formulaM=formula, weights = w, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.622)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.794)
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)))
})



# with missing values
set.seed(123)
sample_Data <- round(runif(10, 1, 3), digits = 0)
sample_Data[c(2,7,9)] <- NA
sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
sample_Data[c(3,5,10),2] <- NA
sample_Data <- cbind(sample_Data, round(runif(10, 1, 5), digits = 0))
sample_Data[c(2,8),3] <- NA
sample_Data <- cbind(sample_Data, round(runif(10, 100, 100), digits = 0))
sample_Data[c(4,6,7),4] <- NA
sample_Data <- as.data.frame(sample_Data)
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "weights")
sample_Data[,1] <- factor(sample_Data[,1], levels = 1:3, labels = LETTERS[1:3])
sample_Data[,2] <- as.factor(sample_Data[,2])
sample_Data[,3] <- as.factor(sample_Data[,3])
sample_Data[,4] <- as.numeric(sample_Data[,4])

formula <- ~Var1+Var2+Var3
w <- "weights"



# bound = none
test_that("data.frame, default method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="default", formulaM=formula, weights = w), "glm.fit: fitted rates numerically 0 occurred")
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.582)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.771)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.67, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.67, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.67)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.00, 0.01, 0.00)))
})


test_that("data.frame, PML method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="PML", formulaM=formula, weights = w), "glm.fit: fitted rates numerically 0 occurred")
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.791)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.878)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.00, 0.01, 0.00)))
})


test_that("data.frame, weightedLLM method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="weightedLLM", formulaM=formula, weights = w), "glm.fit: fitted rates numerically 0 occurred")
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.528)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.736)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.5)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.00, 0.01, 0.00)))
})


test_that("data.frame, IPF method, bound = no", {
  fr <- modRisk(sample_Data, method="IPF", formulaM=formula, weights = w)
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.582)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.771)
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.00, 0.01, 0.00)))
  expect_warning(modRisk(sample_Data, method="default", formulaM=formula, weights = w), "glm.fit: fitted rates numerically 0 occurred")
})


# bound = yes
test_that("data.frame, default method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="default", formulaM=formula, weights = w, bound = 1.5), "glm.fit: fitted rates numerically 0 occurred")
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.582)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.771)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.67, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.67, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.67)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.00, 0.01, 0.00)))
})

test_that("data.frame, PML method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="PML", formulaM=formula, weights = w, bound = 1.5), "glm.fit: fitted rates numerically 0 occurred")
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.791)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.878)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.00, 0.01, 0.00)))
})

test_that("data.frame, weightedLLM method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="weightedLLM", formulaM=formula, weights = w, bound = 1.5), "glm.fit: fitted rates numerically 0 occurred")
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.528)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.736)
  expect_true(all(round(as.numeric(unlist(fr[7])), digits=2)==c(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.5, 0.5)))
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.00, 0.01, 0.00)))
})

test_that("data.frame, IPF method, bound = no", {
  fr <- modRisk(sample_Data, method="IPF", formulaM=formula, weights = w, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr[1])), digits=3)==0.582)
  expect_true(round(as.numeric(unclass(fr[2])), digits=3)==0.771)
  expect_true(all(round(as.numeric(unlist(fr[8])), digits=2)==c(0.00, 0.01, 0.00)))
  expect_warning(modRisk(sample_Data, method="default", formulaM=formula, weights = w), "glm.fit: fitted rates numerically 0 occurred")
})


set.seed(123)
sample_Data <- round(runif(10, 1, 3), digits = 0)
sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 5), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 100, 100), digits = 0))
sample_Data <- as.data.frame(sample_Data)
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "weights")
sample_Data[,1] <- factor(sample_Data[,1], levels = 1:3, labels = LETTERS[1:3])
sample_Data[,2] <- as.factor(sample_Data[,2])
sample_Data[,3] <- as.factor(sample_Data[,3])
sample_Data[,4] <- as.numeric(sample_Data[,4])
sample_Data <- createSdcObj(sample_Data, keyVars = c("Var1", "Var2", "Var3"), w = "weights")

formula <- ~Var1+Var2+Var3
w <- "weights"

# no missing value
# bound no
test_that("sdc, default method, bound = no", {
  fr <- modRisk(sample_Data, method="default", formulaM=formula)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.553)
  expect_true(round(as.numeric(unclass(fr@risk$model[2])), digits=3)==0.705)
  expect_true(all(round(as.numeric(unlist(fr@risk$model[7])), digits=2)==c(0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.4, 0.4, 0.8, 0.4, 0.4, 0.4, 0.8, 0.4)))
  expect_true(all(round(as.numeric(unlist(fr@risk$model[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.01)))
})

test_that("sdc, CE method, bound = no", {
  fr <- modRisk(sample_Data, method="CE", formulaM=formula)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.542)
  expect_true(round(as.numeric(unclass(fr@risk$model[2])), digits=3)==0.699)
  expect_true(all(round(as.numeric(unlist(fr@risk$model[7])), digits=2)==c(0.10, 0.10, 0.20, 0.10, 0.10, 0.10, 0.21, 0.10, 0.52, 0.52, 0.95, 0.47, 0.52, 0.47, 1.03, 0.52, 0.39, 0.43, 0.85, 0.39, 0.38, 0.38, 0.76, 0.42)))
  expect_true(all(round(as.numeric(unlist(fr@risk$model[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.01)))
})

test_that("sdc, PML method, bound = no", {
  fr <- modRisk(sample_Data, method="PML", formulaM=formula)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.553)
  expect_true(round(as.numeric(unclass(fr@risk$model[2])), digits=3)==0.705)
  expect_true(all(round(as.numeric(unlist(fr@risk$model[7])), digits=2)==c(0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.4, 0.4, 0.8, 0.4, 0.4, 0.4, 0.8, 0.4)))
  expect_true(all(round(as.numeric(unlist(fr@risk$model[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.01)))
})

test_that("sdc, weightedLLM method, bound = no", {
  fr <- modRisk(sample_Data, method="weightedLLM", formulaM=formula)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.408)
  expect_true(round(as.numeric(unclass(fr@risk$model[2])), digits=3)==0.613)
  expect_true(all(round(as.numeric(unlist(fr@risk$model[7])), digits=2)==c(0.03, 0.05, 0.01, 0.02, 0.06, 0.13, 0.63, 0.06, 0.53, 1.07, 0.01, 0.02, 1.34, 0.13, 0.65, 1.25, 0.01, 0.55, 2.68, 0.01, 0.03, 0.07, 0.02, 0.63)))
  expect_true(all(round(as.numeric(unlist(fr@risk$model[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.01)))
})

test_that("sdc, IPF method, bound = no", {
  fr <- modRisk(sample_Data, method="IPF", formulaM=formula)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.553)
  expect_true(round(as.numeric(unclass(fr@risk$model[2])), digits=3)==0.705)
  expect_true(all(round(as.numeric(unlist(fr@risk$model[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.02, 0.01)))
})




# bound yes
test_that("sdc, default method, bound = yes", {
  fr <- modRisk(sample_Data, method="default", formulaM=formula, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.622)
  expect_true(round(as.numeric(unclass(fr@risk$model[2])), digits=3)==0.794)
  expect_true(all(round(as.numeric(unlist(fr@risk$model[7])), digits=2)==c(0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.4, 0.4, 0.8, 0.4, 0.4, 0.4, 0.8, 0.4)))
  expect_true(all(round(as.numeric(unlist(fr@risk$model[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)))
})

test_that("sdc, CE method, bound = yes", {
  fr <- modRisk(sample_Data, method="CE", formulaM=formula, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.609)
  expect_true(round(as.numeric(unclass(fr@risk$model[2])), digits=3)==0.786)
  expect_true(all(round(as.numeric(unlist(fr@risk$model[7])), digits=2)==c(0.10, 0.10, 0.20, 0.10, 0.10, 0.10, 0.21, 0.10, 0.52, 0.52, 0.95, 0.47, 0.52, 0.47, 1.03, 0.52, 0.39, 0.43, 0.85, 0.39, 0.38, 0.38, 0.76, 0.42)))
  expect_true(all(round(as.numeric(unlist(fr@risk$model[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)))
})

test_that("sdc, PML method, bound = yes", {
  fr <- modRisk(sample_Data, method="PML", formulaM=formula, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.622)
  expect_true(round(as.numeric(unclass(fr@risk$model[2])), digits=3)==0.794)
  expect_true(all(round(as.numeric(unlist(fr@risk$model[7])), digits=2)==c(0.1, 0.1, 0.2, 0.1, 0.1, 0.1, 0.2, 0.1, 0.5, 0.5, 1.0, 0.5, 0.5, 0.5, 1.0, 0.5, 0.4, 0.4, 0.8, 0.4, 0.4, 0.4, 0.8, 0.4)))
  expect_true(all(round(as.numeric(unlist(fr@risk$model[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)))
})

test_that("sdc, weightedLLM method, bound = yes", {
  fr <- modRisk(sample_Data, method="weightedLLM", formulaM=formula, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.459)
  expect_true(round(as.numeric(unclass(fr@risk$model[2])), digits=3)==0.690)
  expect_true(all(round(as.numeric(unlist(fr@risk$model[7])), digits=2)==c(0.03, 0.05, 0.01, 0.02, 0.06, 0.13, 0.63, 0.06, 0.53, 1.07, 0.01, 0.02, 1.34, 0.13, 0.65, 1.25, 0.01, 0.55, 2.68, 0.01, 0.03, 0.07, 0.02, 0.63)))
  expect_true(all(round(as.numeric(unlist(fr@risk$model[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)))
})

test_that("sdc, IPF method, bound = yes", {
  fr <- modRisk(sample_Data, method="IPF", formulaM=formula, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.622)
  expect_true(round(as.numeric(unclass(fr@risk$model[2])), digits=3)==0.794)
  expect_true(all(round(as.numeric(unlist(fr@risk$model[8])), digits=2)==c(0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01, 0.01)))
})


# missing value
set.seed(123)
sample_Data <- round(runif(10, 1, 3), digits = 0)
sample_Data[c(2,7,9)] <- NA
sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
sample_Data[c(3,5,10),2] <- NA
sample_Data <- cbind(sample_Data, round(runif(10, 1, 5), digits = 0))
sample_Data[c(2,8),3] <- NA
sample_Data <- cbind(sample_Data, round(runif(10, 100, 100), digits = 0))
sample_Data <- as.data.frame(sample_Data)
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "weights")
sample_Data[,1] <- factor(sample_Data[,1], levels = 1:3, labels = LETTERS[1:3])
sample_Data[,2] <- as.factor(sample_Data[,2])
sample_Data[,3] <- as.factor(sample_Data[,3])
sample_Data[,4] <- as.numeric(sample_Data[,4])
sample_Data <- createSdcObj(sample_Data, keyVars = c("Var1", "Var2", "Var3"), w = "weights")

formula <- ~Var1+Var2+Var3
w <- "weights"

# bound no
test_that("sdc, default method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="default", formulaM=formula), "glm.fit: fitted rates numerically 0 occurred")
  expect_true(all(round(as.numeric(unlist(fr@risk$model[7])), digits=2)==c(0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.67, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.67, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.33, 0.67)))
  expect_warning(modRisk(sample_Data, method="default", formulaM=formula, weights = w), "glm.fit: fitted rates numerically 0 occurred")
})

test_that("sdc, CE method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="CE", formulaM=formula), "glm.fit: fitted rates numerically 0 occurred")
})

test_that("sdc, PML method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="PML", formulaM=formula), "glm.fit: fitted rates numerically 0 occurred")
})

test_that("sdc, weightedLLM method, bound = no", {
  fr <- modRisk(sample_Data, method="weightedLLM", formulaM=formula)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.372)
})

test_that("sdc, IPF method, bound = no", {
  fr <- modRisk(sample_Data, method="IPF", formulaM=formula)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.584)
})



# bound yes
test_that("sdc, default method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="default", formulaM=formula, bound = 1.5), "glm.fit: fitted rates numerically 0 occurred")
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.584)
})

test_that("sdc, CE method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="CE", formulaM=formula, bound = 1.5), "glm.fit: fitted rates numerically 0 occurred")
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.573)
})

test_that("sdc, PML method, bound = no", {
  expect_warning(fr <- modRisk(sample_Data, method="PML", formulaM=formula, bound = 1.5), "glm.fit: fitted rates numerically 0 occurred")
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.584)
})

test_that("sdc, weightedLLM method, bound = no", {
  fr <- modRisk(sample_Data, method="weightedLLM", formulaM=formula, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.372)
})

test_that("sdc, IPF method, bound = no", {
  fr <- modRisk(sample_Data, method="IPF", formulaM=formula, bound = 1.5)
  expect_true(round(as.numeric(unclass(fr@risk$model[1])), digits=3)==0.584)
})
