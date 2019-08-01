# data frame
set.seed(3)
sample_Data <- runif(10, 1, 100)
sample_Data <- cbind(sample_Data, as.factor(round(runif(10, 1, 4), digits = 0)))
sample_Data <- as.data.frame(sample_Data)
colnames(sample_Data) <- c("Var1", "Var2")
sample_Data[,2] <- as.factor(sample_Data[,2])


# Continuous Variable
# breaks = 1
test_that("numeric values (breaks = 1, labels = yes, method = equidistant)", {
  expect_equal(length(levels(globalRecode(sample_Data[,1], breaks = 1, labels = 1, method = "equidistant"))),1L)
})

test_that("numeric values (breaks = 1, labels = yes, method = logEqui)", {
  fr <- globalRecode(sample_Data[,1], breaks = 1, labels = 1, method = "logEqui")
  A <- levels(fr)[1]
  expect_true(all(fr == c(A,A,A,A,A,A,A,A,A,A)))
})

test_that("numeric values (breaks = 1, labels = yes, method = equalAmount)", {
  fr <- globalRecode(sample_Data[,1], breaks = 1, labels = 1, method = "equalAmount")
  A <- levels(fr)[1]
  expect_true(all(fr == c(A,A,A,A,A,A,A,A,A,A)))
})


test_that("numeric values (breaks = 1, labels = no, method = equidistant)", {
  expect_equal(length(levels(globalRecode(sample_Data[,1], breaks = 1, labels = 1, method = "equidistant"))), 1L)
})

test_that("numeric values (breaks = 1, labels = no, method = logEqui)", {
  fr <- globalRecode(sample_Data[,1], breaks = 1, method = "logEqui")
  A <- levels(fr)[1]
  expect_true(all(fr == c(A,A,A,A,A,A,A,A,A,A)))
})

test_that("numeric values (breaks = 1, labels = no, method = equalAmount)", {
  fr <- globalRecode(sample_Data[,1], breaks = 1, method = "equalAmount")
  A <- levels(fr)[1]
  expect_true(all(fr == c(A,A,A,A,A,A,A,A,A,A)))
})


test_that("numeric values (breaks = 1, labels = invalid, method = equidistant)", {
  expect_error(globalRecode(sample_Data[,1], breaks = 1, labels = 1:5, method = "equidistant"), "lengths of 'breaks' and 'labels' differ")
})

test_that("numeric values (breaks = 1, labels = invalid, method = logEqui)", {
  expect_error(globalRecode(sample_Data[,1], breaks = 1, labels = 1:5, method = "logEqui"), "lengths of 'breaks' and 'labels' differ")
})

test_that("numeric values (breaks = 1, labels = invalid, method = equalAmount)", {
  expect_error(globalRecode(sample_Data[,1], breaks = 1, labels = 1:5, method = "equalAmount"), "lengths of 'breaks' and 'labels' differ")
})




# breaks = more
test_that("numeric values (breaks = more, labels = yes, method = equidistant)", {
  fr <- globalRecode(sample_Data[,1], breaks = 3, labels = 1:3, method = "equidistant")
  A <- levels(fr)[1]
  B <- levels(fr)[2]
  C <- levels(fr)[3]
  expect_true(all(fr == c(A,C,B,A,C,C,A,A,B,C)))
})

test_that("numeric values (breaks = more, labels = yes, method = logEqui)", {
  fr <- globalRecode(sample_Data[,1], breaks = 3, labels = 1:3, method = "logEqui")
  A <- levels(fr)[1]
  B <- levels(fr)[2]
  C <- levels(fr)[3]
  expect_true(all(fr == c(B,C,B,B,B,B,A,B,B,C)))
})

test_that("numeric values (breaks = more, labels = yes, method = equalAmount)", {
  fr <- globalRecode(sample_Data[,1], breaks = 3, labels = 1:3, method = "equalAmount")
  A <- levels(fr)[1]
  B <- levels(fr)[2]
  C <- levels(fr)[3]
  expect_true(all(fr == c(A,C,B,A,B,C,A,A,B,C)))
})



test_that("numeric values (breaks = more, labels = no, method = equidistant)", {
  fr <- globalRecode(sample_Data[,1], breaks = 3, method = "equidistant")
  A <- levels(fr)[1]
  B <- levels(fr)[2]
  C <- levels(fr)[3]
  expect_true(all(fr == c(A,C,B,A,C,C,A,A,B,C)))
})

test_that("numeric values (breaks = more, labels = no, method = logEqui)", {
  fr <- globalRecode(sample_Data[,1], breaks = 3, method = "logEqui")
  A <- levels(fr)[1]
  B <- levels(fr)[2]
  C <- levels(fr)[3]
  expect_true(all(fr == c(B,C,B,B,B,B,A,B,B,C)))
})

test_that("numeric values (breaks = more, labels = no, method = equalAmount)", {
  fr <- globalRecode(sample_Data[,1], breaks = 3, method = "equalAmount")
  A <- levels(fr)[1]
  B <- levels(fr)[2]
  C <- levels(fr)[3]
  expect_true(all(fr == c(A,C,B,A,B,C,A,A,B,C)))
})


test_that("numeric values (breaks = more, labels = invalid, method = equidistant)", {
  expect_error(globalRecode(sample_Data[,1], breaks = 3, labels = 1:5, method = "equidistant"), "lengths of 'breaks' and 'labels' differ")
})

test_that("numeric values (breaks = more, labels = invalid, method = logEqui)", {
  expect_error(globalRecode(sample_Data[,1], breaks = 3, labels = 1:5, method = "logEqui"), "lengths of 'breaks' and 'labels' differ")
})

test_that("numeric values (breaks = more, labels = invalid, method = equalAmount)", {
  expect_error(globalRecode(sample_Data[,1], breaks = 3, labels = 1:5, method = "equalAmount"), "lengths of 'breaks' and 'labels' differ")
})




# breaks = cutoff
test_that("numeric values (breaks = cutoff, labels = yes, method = none)", {
  fr <- globalRecode(sample_Data[,1], breaks = c(0,30,60,90), labels=1:3)
  A <- levels(fr)[1]
  B <- levels(fr)[2]
  C <- levels(fr)[3]
  expect_equal(all(fr == c(A,C,B,B,C,C,A,B,B,C)), TRUE)
})


test_that("numeric values (breaks = cutoff, labels = no, method = none)", {
  fr <- globalRecode(sample_Data[,1], breaks = c(0,30,60,90))
  A <- levels(fr)[1]
  B <- levels(fr)[2]
  C <- levels(fr)[3]
  expect_equal(all(fr == c(A,C,B,B,C,C,A,B,B,C)), TRUE)
})


test_that("numeric values (breaks = cutoff, labels = invalid, method = none)", {
  expect_error(globalRecode(sample_Data[,1], breaks = c(0,30,60,90), labels = 1:5), "lengths of 'breaks' and 'labels' differ")
})









# Categorical Variable
# breaks = 1
test_that("numeric values (breaks = 1, labels = yes, method = equidistant)", {
  expect_error(globalRecode(sample_Data[,2], breaks = 1, labels = 1, method = "equidistant"), "'x' must be numeric")
})


test_that("numeric values (breaks = 1, labels = no, method = equidistant)", {
  expect_error(globalRecode(sample_Data[,2], breaks = 1, labels = 1, method = "equidistant"), "'x' must be numeric")  
})


test_that("numeric values (breaks = 1, labels = invalid, method = equidistant)", {
  expect_error(globalRecode(sample_Data[,2], breaks = 1, labels = 1:5, method = "equidistant"), "'x' must be numeric")  
})

test_that("numeric values (breaks = 1, labels = invalid, method = logEqui)", {
  expect_error(globalRecode(sample_Data[,2], breaks = 1, labels = 1:5, method = "logEqui"), "'x' must be numeric")  
})

test_that("numeric values (breaks = 1, labels = invalid, method = equalAmount)", {
  expect_error(globalRecode(sample_Data[,2], breaks = 1, labels = 1:5, method = "equalAmount"), "'x' must be numeric")  
})



test_that("numeric values (breaks = more, labels = invalid, method = equidistant)", {
  expect_error(globalRecode(sample_Data[,1], breaks = 3, labels = 1:5, method = "equidistant"), "lengths of 'breaks' and 'labels' differ")  
})

test_that("numeric values (breaks = more, labels = invalid, method = logEqui)", {
  expect_error(globalRecode(sample_Data[,1], breaks = 3, labels = 1:5, method = "logEqui"), "lengths of 'breaks' and 'labels' differ")  
})


# breaks = cutoff
test_that("numeric values (breaks = cutoff, labels = yes, method = none)", {
  fr <- globalRecode(sample_Data[,1], breaks = c(0,2,3,4), labels=1:3)
  A <- levels(fr)[1]
  B <- levels(fr)[2]
  C <- levels(fr)[3]
  expect_true(all(is.na(fr)))
})




test_that("numeric values (breaks = cutoff, labels = invalid, method = none)", {
  expect_error(globalRecode(sample_Data[,1], breaks = c(0,2,3,4), labels = 1:5), "lengths of 'breaks' and 'labels' differ")  
})

# sdc
set.seed(3)
sample_Data <- runif(10, 1, 100)
sample_Data <- cbind(sample_Data, as.factor(round(runif(10, 1, 4), digits = 0)))
sample_Data <- as.data.frame(sample_Data)
colnames(sample_Data) <- c("Var1", "Var2")
sample_Data[,2] <- as.factor(sample_Data[,2])
sample_Data <- createSdcObj(sample_Data, keyVars = c("Var2","Var1"))


# Continuous Variable
# breaks = 1
test_that("numeric values (breaks = 1, labels = yes, method = equidistant)", {
  expect_error(globalRecode(sample_Data, column ="Var2", breaks = 1, labels = 1, method = "equidistant"), "'x' must be numeric")  
})

test_that("numeric values (breaks = 1, labels = yes, method = logEqui)", {
  sample_DataX <- globalRecode(sample_Data, column="Var1",breaks = 1, labels = 1, method = "logEqui")
  expect_equal(sample_DataX@risk$global$risk, 0.4)
})

test_that("numeric values (breaks = 1, labels = yes, method = equalAmount)", {
  sample_DataX <- globalRecode(sample_Data, column="Var1",breaks = 1, labels = 1, method = "equalAmount")
  expect_equal(sample_DataX@risk$global$risk, 0.4)
})

test_that("numeric values (breaks = 1, labels = no, method = logEqui)", {
  sample_DataX <- globalRecode(sample_Data, column = "Var1", breaks = 1, method = "logEqui")
  expect_equal(sample_DataX@risk$global$risk_ER, 4.00)
})

test_that("numeric values (breaks = 1, labels = no, method = equalAmount)", {
  sample_DataX <- globalRecode(sample_Data, column = "Var1", breaks = 1, method = "equalAmount")
  expect_equal(sample_DataX@risk$global$risk, 0.4)
  expect_equal(sample_DataX@risk$global$risk_ER, 4.00)
  expect_equal(sample_DataX@risk$global$risk_pct, 40.00)
  expect_equal(sample_DataX@risk$global$threshold, 0.00)
  expect_equal(sample_DataX@risk$global$max_risk, 0.01)
})

test_that("numeric values (breaks = 1, labels = invalid, method = equidistant)", {
  expect_error(globalRecode(sample_Data, column = "Var1", breaks = 1, labels = 1:5, method = "equidistant"), "lengths of 'breaks' and 'labels' differ")  
})

test_that("numeric values (breaks = 1, labels = invalid, method = logEqui)", {
  expect_error(globalRecode(sample_Data, column = "Var1", breaks = 1, labels = 1:5, method = "logEqui"), "lengths of 'breaks' and 'labels' differ")  
})

test_that("numeric values (breaks = 1, labels = invalid, method = equalAmount)", {
  expect_error(globalRecode(sample_Data, column = "Var1", breaks = 1, labels = 1:5, method = "equalAmount"), "lengths of 'breaks' and 'labels' differ")  
})


# breaks = more
test_that("numeric values (breaks = more, labels = yes, method = equidistant)", {
  sample_DataX <- globalRecode(sample_Data, column = "Var1", breaks = 3, labels = 1:3, method = "equidistant")
  expect_equal(sample_DataX@risk$global$risk, 0.7)
  expect_equal(sample_DataX@risk$global$risk_ER, 7)
  expect_equal(sample_DataX@risk$global$risk_pct, 70)
})

test_that("numeric values (breaks = more, labels = yes, method = logEqui)", {
  sample_DataX <- globalRecode(sample_Data, column = "Var1", breaks = 3, labels = 1:3, method = "logEqui")
  expect_equal(sample_DataX@risk$global$risk, 0.5)
  expect_equal(sample_DataX@risk$global$risk_ER, 5)
  expect_equal(sample_DataX@risk$global$risk_pct, 50)
})

test_that("numeric values (breaks = more, labels = yes, method = equalAmount)", {
  sample_DataX <- globalRecode(sample_Data, column = "Var1", breaks = 3, labels = 1:3, method = "equalAmount")
  expect_equal(sample_DataX@risk$global$risk, 0.6)
  expect_equal(sample_DataX@risk$global$risk_ER, 6)
  expect_equal(sample_DataX@risk$global$risk_pct, 60)
})



# breaks = cutoff
test_that("numeric values (breaks = cutoff, labels = yes, method = none)", {
  sample_DataX <- globalRecode(sample_Data, column="Var1",breaks = c(0,30,60,90), labels=1:3)
  expect_equal(sample_DataX@risk$global$risk, 0.7)
  expect_equal(sample_DataX@risk$global$risk_ER, 7)
  expect_equal(sample_DataX@risk$global$risk_pct, 70)
})


test_that("numeric values (breaks = cutoff, labels = invalid, method = none)", {
  expect_error(globalRecode(sample_Data, column = "Var1", breaks = c(0,30,60,90), labels = 1:5), "lengths of 'breaks' and 'labels' differ")  
})