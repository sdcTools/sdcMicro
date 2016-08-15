# no missing values
{
	set.seed(3)
	sample_Data <- runif(30, 1, 100)
	sample_Data <- cbind(sample_Data, as.factor(round(runif(30, 1, 4), digits = 0)))
	sample_Data <- as.data.frame(sample_Data)
	colnames(sample_Data) <- c("Var1", "Var2")
	sample_Data[,2] <- as.factor(sample_Data[,2])
	sample_Data <- createSdcObj(sample_Data, keyVars = c("Var2"), numVars = c("Var1"))


	test_that("numeric values, into 1 level from many", {
		sample_Data <- groupVars(sample_Data, var="Var2", before=c("1","2", "3", "4"), after=c("1","1", "1", "1"))
	  	expect_true(all(sample_Data@manipKeyVars$Var2 == c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))) 
	})

	test_that("numeric values, into multiple level from many", {
		sample_Data <- groupVars(sample_Data, var="Var2", before=c("1","2", "3", "4"), after=c("1","2", "3", "2"))
	  	expect_true(all(sample_Data@manipKeyVars$Var2 == c(2, 2, 2, 2, 2, 2, 2, 2, 3, 2, 2, 3, 2, 3, 2, 2, 1, 1, 2, 3, 2, 2, 2, 2, 2, 2, 2, 2, 1, 2))) 
	})
}


# missing values
{
	set.seed(3)
	sample_Data <- runif(30, 1, 100)
	sample_Data <- cbind(sample_Data, as.factor(round(runif(30, 1, 4), digits = 0)))
	sample_Data <- as.data.frame(sample_Data)
	colnames(sample_Data) <- c("Var1", "Var2")
	sample_Data[,2] <- as.factor(sample_Data[,2])
	sample_Data[c(1,4,11,14,21,24,28),2] <- NA
	sample_Data <- createSdcObj(sample_Data, keyVars = c("Var2"), numVars = c("Var1"))

	test_that("numeric values, into 1 level from many", {
		sample_Data <- groupVars(sample_Data, var="Var2", before=c("1","2", "3", "4"), after=c("1","1", "1", "1"))
		expect_true(all(is.na(sample_Data@manipKeyVars$Var2[c(1,4,11,14,21,24,28)])))
	  	expect_true(all(sample_Data@manipKeyVars$Var2[c(2:3,5:10,12:13,15:20,22:23,25:27,29:30)] == c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1))) 
	})

	test_that("numeric values, into multiple level from many", {
		sample_Data <- groupVars(sample_Data, var="Var2", before=c("1","2", "3", "4"), after=c("1","2", "3", "2"))
		expect_true(all(is.na(sample_Data@manipKeyVars$Var2[c(1,4,11,14,21,24,28)])))
	  	expect_true(all(sample_Data@manipKeyVars$Var2[c(2:3,5:10,12:13,15:20,22:23,25:27,29:30)] == c(2, 2, 2, 2, 2, 2, 3, 2, 3, 2, 2, 2, 1, 1, 2, 3, 2, 2, 2, 2, 2, 1, 2))) 
	})
}