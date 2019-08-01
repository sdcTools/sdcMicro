library(testthat)

#sdc without missing values
test_that("numeric without missing values", {     
	sample_Data <- array(c(2, 3, 2, 2, 3, 3, 1, 2, 3, 3, 3, 3, 3, 3, 4, 3, 1, 3, 1, 2, 1, 1, 1, 1), dim = c(8,3))
	colnames(sample_Data) <- c("A", "B", "C")
	sample_Data <- createSdcObj(sample_Data, keyVars = c(1,2,3))
	sample_Data <- varToFactor(sample_Data, "A")
	expect_equal(all(sapply(sample_Data@manipKeyVars$A, is.factor)), TRUE)
	expect_equal(all(sapply(sample_Data@manipKeyVars$B, is.numeric)), TRUE)
	expect_equal(all(sapply(sample_Data@manipKeyVars$C, is.numeric)), TRUE)
	sample_Data <- varToNumeric(sample_Data, "A")
	expect_equal(all(sapply(sample_Data@manipKeyVars$A, is.numeric)), TRUE)
	expect_equal(all(sapply(sample_Data@manipKeyVars$B, is.numeric)), TRUE)
	expect_equal(all(sapply(sample_Data@manipKeyVars$C, is.numeric)), TRUE)	
})


#sdc with missing values
test_that("numeric with missing values", {     
	sample_Data <- array(c(2, 3, NA, 2, 3, NA, 1, 2, 3, NA, 3, 3, 3, 3, NA, 3, 1, 3, NA, 2, 1, 1, 1, 1), dim = c(8,3))
	colnames(sample_Data) <- c("A", "B", "C")
	sample_Data <- createSdcObj(sample_Data, keyVars = c(1,2,3))
	sample_Data <- varToFactor(sample_Data, "A")
	expect_equal(all(sapply(sample_Data@manipKeyVars$A, is.factor)), TRUE)
	expect_equal(all(sapply(sample_Data@manipKeyVars$B, is.numeric)), TRUE)
	expect_equal(all(sapply(sample_Data@manipKeyVars$C, is.numeric)), TRUE)
	sample_Data <- varToNumeric(sample_Data, "A")
	expect_equal(all(sapply(sample_Data@manipKeyVars$A, is.numeric)), TRUE)
	expect_equal(all(sapply(sample_Data@manipKeyVars$B, is.numeric)), TRUE)
	expect_equal(all(sapply(sample_Data@manipKeyVars$C, is.numeric)), TRUE)		
})

