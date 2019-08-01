set.seed(3)
sample_Data <- rlnorm(200)
sample_Data <- array(sample_Data, dim = c(100,2))
sample_Data <- cbind(sample_Data, rlnorm(100))
sample_Data <- cbind(sample_Data, rlnorm(100))
sample_Data <- cbind(sample_Data, rlnorm(100))
sample_Data <- cbind(sample_Data, rlnorm(100))
sample_Data <- cbind(sample_Data, rlnorm(100))
sample_Data <- as.data.frame(sample_Data)
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "weight")


# ds
# without weights
test_that("data frame without missing, method = ds, weights = NULL, covmethod = spearman, regmethod = lm, gadp = TRUE)", {
  set.seed(123)
  fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "spearman", regmethod = "lm", gadp = TRUE) 
  expect_false(all(fr[[1]]!=sample_Data[,c("Var1","Var2")]))
})

test_that("data frame without missing,method = ds, weights = NULL, covmethod = spearman, regmethod = lm, gadp = FALSE)", {
  set.seed(123)
  fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "spearman", regmethod = "lm", gadp = FALSE) 
  expect_false(all(fr[[1]]!=sample_Data[,c("Var1","Var2")]))
})
test_that("data frame without missing, method = ds, weights = NULL, covmethod = spearman, regmethod = MM, gadp = TRUE)", {
  set.seed(123)
  fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "spearman", regmethod = "MM", gadp = TRUE) 
  expect_false(all(fr[[1]]!=sample_Data[,c("Var1","Var2")]))
})

test_that("data frame without missing, method = ds, weights = NULL, covmethod = spearman, regmethod = MM, gadp = FALSE)", {
  set.seed(123)
  fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "spearman", regmethod = "MM", gadp = FALSE) 
  expect_false(all(fr[[1]]!=sample_Data[,c("Var1","Var2")]))
})

test_that("data frame without missing, method = ds, weights = NULL, covmethod = pearson, regmethod = lm, gadp = TRUE)", {
  set.seed(123)
  fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "pearson", regmethod = "lm", gadp = TRUE) 
  expect_false(all(fr[[1]]!=sample_Data[,c("Var1","Var2")]))
})

test_that("data frame without missing, method = ds, weights = NULL, covmethod = pearson, regmethod = lm, gadp = FALSE)", {
  set.seed(123)
  fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "pearson", regmethod = "lm", gadp = FALSE) 
  expect_false(all(fr[[1]]!=sample_Data[,c("Var1","Var2")]))
})

test_that("data frame without missing, method = ds, weights = NULL, covmethod = pearson, regmethod = MM, gadp = TRUE)", {
  set.seed(123)
  fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "pearson", regmethod = "MM", gadp = TRUE) 
  expect_false(all(fr[[1]]!=sample_Data[,c("Var1","Var2")]))
})

test_that("data frame without missing, method = ds, weights = NULL, covmethod = pearson, regmethod = MM, gadp = FALSE)", {
  set.seed(123)
  fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "pearson", regmethod = "MM", gadp = FALSE) 
  expect_false(all(fr[[1]]!=sample_Data[,c("Var1","Var2")]))
})


test_that("data frame without missing, method = mvn, weights = NULL, covmethod = mcd, regmethod = MM, gadp = FALSE)", {
  set.seed(123)
  fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "mcd", regmethod = "MM", gadp = FALSE) 
  expect_false(all(fr[[1]]!=sample_Data[,c("Var1","Var2")]))
})