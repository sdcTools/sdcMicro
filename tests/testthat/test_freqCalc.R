#df without NAs
###########################################################################################################################################
test_that("data frame, numeric values (weighted)", {
  sample_Data <- array(c(2, 3, 2, 2, 3, 3, 1, 2, 3, 3, 3, 3, 3, 3, 4, 3, 1, 3, 1, 2, 1, 1, 1, 1), dim = c(8,3))
  colnames(sample_Data) <- c("A", "B", "C")
  sample_Data <- cbind(sample_Data, "weight" = c(18.0, 45.5, 39.0, 17.0, 92.0, 8.0, 123.5, 51))
  sample_Data <- as.data.frame(sample_Data)
  fr <- freqCalc(sample_Data, keyVars = c(1,2,3), w = 4)
  expect_equal(unclass(fr$Fk), c(108.0, 45.5, 108.0, 17.0, 100.0, 100.0, 123.5, 108.0))
  expect_equal(unclass(fr$fk), c(3, 1, 3, 1, 2, 2, 1, 3))
  expect_equal(unclass(fr$n1), 3)
  expect_equal(unclass(fr$n2), 2)
})
###########################################################################################################################################


###########################################################################################################################################
test_that("data frame, numeric values (non-weighted, non-fast method)", {
  sample_Data <- array(c(2, 3, 2, 2, 3, 3, 1, 2, 3, 3, 3, 3, 3, 3, 4, 3, 1, 3, 1, 2, 1, 1, 1, 1), dim = c(8,3))
  colnames(sample_Data) <- c("A", "B", "C")
  sample_Data <- cbind(sample_Data, "weight" = c(18.0, 45.5, 39.0, 17.0, 92.0, 8.0, 123.5, 51))
  sample_Data <- as.data.frame(sample_Data)
  
  fr <- freqCalc(sample_Data, keyVars = c(1,2,3))
  expect_equal(unclass(fr$Fk), c(3, 1, 3, 1, 2, 2, 1, 3))
  expect_equal(unclass(fr$fk), c(3, 1, 3, 1, 2, 2, 1, 3))
  expect_equal(unclass(fr$n1), 3)
  expect_equal(unclass(fr$n2), 2)
})
###########################################################################################################################################



###########################################################################################################################################
test_that("data frame, numeric values with missing values (weighted)", {
  #df with NAs
  sample_Data <- array(c(2, 3, 2, 2, 3, 3, 1, 2, 3, 3, 3, 3, 3, 3, 4, 3, 1, 3, NA, 2, 1, 1, NA, 1), dim = c(8,3))
  colnames(sample_Data) <- c("A", "B", "C")
  sample_Data <- cbind(sample_Data, "weight" = c(18.0, 45.5, 39.0, 17.0, 92.0, 8.0, 123.5, 51))
  sample_Data <- as.data.frame(sample_Data)
  fr <- freqCalc(sample_Data, keyVars = c(1,2,3), w = 4)
  expect_equal(unclass(fr$Fk), c(108.0, 45.5, 125.0, 56.0, 100.0, 100.0, 123.5, 108.0))
  expect_equal(unclass(fr$fk), c(3, 1, 4, 2, 2, 2, 1, 3))
  expect_equal(unclass(fr$n1), 2)
  expect_equal(unclass(fr$n2), 3)
})
###########################################################################################################################################


###########################################################################################################################################
test_that("data frame, numeric values with missing values (non-weighted)", {
  sample_Data <- array(c(2, 3, 2, 2, 3, 3, 1, 2, 3, 3, 3, 3, 3, 3, 4, 3, 1, 3, NA, 2, 1, 1, NA, 1), dim = c(8,3))
  colnames(sample_Data) <- c("A", "B", "C")
  sample_Data <- cbind(sample_Data, "weight" = c(18.0, 45.5, 39.0, 17.0, 92.0, 8.0, 123.5, 51))
  sample_Data <- as.data.frame(sample_Data)
  fr <- freqCalc(sample_Data, keyVars = c(1,2,3))
  expect_equal(unclass(fr$Fk), c(3, 1, 4, 2, 2, 2, 1, 3))
  expect_equal(unclass(fr$fk), c(3, 1, 4, 2, 2, 2, 1, 3))
  expect_equal(unclass(fr$n1), 2)
  expect_equal(unclass(fr$n2), 3)
})
###########################################################################################################################################


#df with one column and one row with NAs
sample_Data <- array(c(2, 3, NA, 2, 3, 3, 1, 2, 3, 3, NA, 3, 3, 3, 4, 3, NA, NA, NA, NA, NA, NA, NA, NA), dim = c(8,3))
colnames(sample_Data) <- c("A", "B", "C")
sample_Data <- cbind(sample_Data, "weight" = c(18.0, 45.5, 39.0, 17.0, 92.0, 8.0, 123.5, 51))
sample_Data <- as.data.frame(sample_Data)

###########################################################################################################################################
test_that("data frame, numeric values with with one column and one row with NAs (weighted)", {
  fr <- freqCalc(sample_Data, keyVars = c(1,2,3), w = 4)
  expect_equal(unclass(fr$Fk), c(125.0, 184.5, 394.0, 125.0, 184.5, 184.5, 162.5, 125.0))
  expect_equal(unclass(fr$fk), c(4, 4, 8, 4, 4, 4, 2, 4))
  expect_equal(unclass(fr$n1), 0)
  expect_equal(unclass(fr$n2), 1)
})
###########################################################################################################################################


###########################################################################################################################################
test_that("data frame, numeric values with with one column and one row with NAs (non-weighted)", {
  fr <- freqCalc(sample_Data, keyVars = c(1,2,3))
  expect_equal(unclass(fr$Fk), c(4, 4, 8, 4, 4, 4, 2, 4))
  expect_equal(unclass(fr$fk), c(4, 4, 8, 4, 4, 4, 2, 4))
  expect_equal(unclass(fr$n1), 0)
  expect_equal(unclass(fr$n2), 1)
})
###########################################################################################################################################



#df with one column with NAs
sample_Data <- array(c(2, 3, NA, 2, 1, NA, 1, 2), dim = c(8,1))
colnames(sample_Data) <- c("A")
sample_Data <- cbind(sample_Data, "weight" = c(18.0, 45.5, 39.0, 17.0, 92.0, 8.0, 123.5, 51))
sample_Data <- as.data.frame(sample_Data)

###########################################################################################################################################
test_that("data frame, numeric values with one column with NAs (weighted)", {
  fr <- freqCalc(sample_Data, keyVars = c(1), w = 2)
  expect_equal(unclass(fr$Fk), c(133.0, 92.5, 394.0, 133.0, 262.5, 394.0, 262.5, 133.0))
  expect_equal(unclass(fr$fk), c(5, 3, 8, 5, 4, 8, 4, 5))
  expect_equal(unclass(fr$n1), 0)
  expect_equal(unclass(fr$n2), 0)
})
###########################################################################################################################################

###########################################################################################################################################
test_that("data frame, numeric values with one column with NAs (non-weighted)", {
  fr <- freqCalc(sample_Data, keyVars = c(1))
  expect_equal(unclass(fr$Fk), c(5, 3, 8, 5, 4, 8, 4, 5))
  expect_equal(unclass(fr$fk), c(5, 3, 8, 5, 4, 8, 4, 5))
  expect_equal(unclass(fr$n1), 0)
  expect_equal(unclass(fr$n2), 0)
})
###########################################################################################################################################




#df with more columns then rows
sample_Data <- array(c(2, 3, NA, 2, 3, 3, 1, NA, 3, 3, NA, 3, 3, 2, 2, 3, NA, NA, 1, 2, NA, NA, 4, 4), dim = c(3,8))
colnames(sample_Data) <- c("A", "B", "C", "D", "E", "F", "G", "H")
sample_Data <- cbind(sample_Data, "weight" = c(18.0, 45.5, 122))
sample_Data <- as.data.frame(sample_Data)

###########################################################################################################################################
test_that("data frame, numeric values with one column with NAs (weighted)", {
  fr <- freqCalc(sample_Data, keyVars = c(1, 2, 3, 4, 5, 6, 7, 8), w = 9)
  expect_equal(unclass(fr$Fk), c(18.0, 167.5, 167.5))
  expect_equal(unclass(fr$fk), c(1, 2, 2))
  expect_equal(unclass(fr$n1), 1)
  expect_equal(unclass(fr$n2), 2)
})
###########################################################################################################################################

###########################################################################################################################################
test_that("data frame, numeric values with one column with NAs (non-weighted)", {
  fr <- freqCalc(sample_Data, keyVars = c(1, 2, 3, 4, 5, 6, 7, 8))
  expect_equal(unclass(fr$Fk), c(1, 2, 2))
  expect_equal(unclass(fr$fk), c(1, 2, 2))
  expect_equal(unclass(fr$n1), 1)
  expect_equal(unclass(fr$n2), 2)
})
###########################################################################################################################################