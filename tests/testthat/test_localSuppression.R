library(testthat)



# dataframe
{
	# dataframe without missing
	{
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

		# "dataframe without missing, k = 2
		{
			# "dataframe without missing, k = 2, importance=NULL
			{
				test_that("dataframe without missing, k = 2, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k = 2, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k = 2, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}




			# "dataframe without missing, k = 2, importance=c(3,2,1)
			{
				test_that("dataframe without missing, k = 2, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k = 2, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k = 2, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}





			# "dataframe without missing, k = 2, importance=c(1,2,3)
			{
				test_that("dataframe without missing, k = 2, importance=c(1,2,3), combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k = 2, importance=c(1,2,3), combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k = 2, importance=c(1,2,3), combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}
		}









		# "dataframe without missing, k=3
		{
			# "dataframe without missing, k=3, importance=NULL
			{
				test_that("dataframe without missing, k=3, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}




			# "dataframe without missing, k=3, importance=c(3,2,1)
			{
				test_that("dataframe without missing, k=3, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}





			# "dataframe without missing, k=3, importance=c(1,2,3)
			{
				test_that("dataframe without missing, k=3, importance=c(1,2,3), combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=c(1,2,3), combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=c(1,2,3), combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}
		}
	}









	# dataframe with missing
	{
		set.seed(3)
		sample_Data <- runif(30, 1, 100)
		sample_Data <- array(sample_Data, dim = c(10,3))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
		colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
		sample_Data[c(1,2,7),4] <- NA
		sample_Data[c(2,6,9),5] <- NA
		sample_Data[c(2,10),6] <- NA
		sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
		sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
		sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))

		# "dataframe without missing, k = 2
		{
			# "dataframe without missing, k = 2, importance=NULL
			{
				test_that("dataframe without missing, k = 2, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k = 2, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k = 2, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}




			# "dataframe without missing, k = 2, importance=c(3,2,1)
			{
				test_that("dataframe without missing, k = 2, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA ,2))
				})

				test_that("dataframe without missing, k = 2, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA ,2))
				})

				test_that("dataframe without missing, k = 2, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA ,2))
				})
			}





			# "dataframe without missing, k = 2, importance=c(1,2,3)
			{
				test_that("dataframe without missing, k = 2, importance=c(1,2,3), combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k = 2, importance=c(1,2,3), combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k = 2, importance=c(1,2,3), combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}
		}









		# "dataframe without missing, k=3
		{
			# "dataframe without missing, k=3, importance=NULL
			{
				test_that("dataframe without missing, k=3, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}




			# "dataframe without missing, k=3, importance=c(3,2,1)
			{
				test_that("dataframe without missing, k=3, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}





			# "dataframe without missing, k=3, importance=c(1,2,3)
			{
				test_that("dataframe without missing, k=3, importance=c(1,2,3), combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=NULL)
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=c(1,2,3), combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=c(1:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("dataframe without missing, k=3, importance=c(1,2,3), combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=c(3:2))
					expect_equal(fr[[1]][,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(fr[[1]][,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(fr[[1]][,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}
		}
	}
}









# sdc
{
	# sdc without missing
	{
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

		# "sdc without missing, k = 2
		{
			# "sdc without missing, k = 2, importance=NULL
			{
				test_that("sdc without missing, k = 2, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("sdc without missing, k = 2, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(2, 2, NA, 2, 2, 2, NA, 1, 2, NA))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("sdc without missing, k = 2, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}




			# "sdc without missing, k = 2, importance=c(3,2,1)
			{
				test_that("sdc without missing, k = 2, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, NA, 1, 1, 1, 1, 1, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, 3, 3, 3, 2, 2, NA, 2))
				})

				test_that("sdc without missing, k = 2, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(2, 2, NA, 2, 2, 2, NA, 1, 2, NA))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, NA, 1, 1, 1, 1, 1, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, 3, 3, 3, 2, 2, NA, 2))
				})

				test_that("sdc without missing, k = 2, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, NA, 2, 2, 2, 3, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, NA, 1, 1, 1, 1, 1, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, 3, 3, 3, 2, 2, NA, 2))
				})
			}





			# "sdc without missing, k = 2, importance=c(1,2,3)
			{
				test_that("sdc without missing, k = 2, importance=c(1,2,3), combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(2, 2, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(NA, NA, NA, 3, 3, 3, 2, 2, 1, 2))
				})

				test_that("sdc without missing, k = 2, importance=c(1,2,3), combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(2, 2, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, NA, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("sdc without missing, k = 2, importance=c(1,2,3), combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(2, 2, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(NA, NA, NA, 3, 3, 3, 2, 2, 1, 2))
				})
			}
		}









		# "sdc without missing, k=3
		{
			# "sdc without missing, k=3, importance=NULL
			{
				test_that("sdc without missing, k=3, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, NA, NA, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("sdc without missing, k=3, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(2, 2, NA, 2, 2, 2, NA, 1, 2, NA))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})

				test_that("sdc without missing, k=3, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, NA, NA, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, 2, 1, 2, 1, 1, 1, 1, 1, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, NA, 3, 3, 2, 2, NA, 2))
				})
			}




			# "sdc without missing, k=3, importance=c(3,2,1)
			{
				test_that("sdc without missing, k=3, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, NA, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, NA, 1, 1, 1, 1, 1, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, 3, 3, 3, 2, 2, NA, 2))
				})

				test_that("sdc without missing, k=3, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(2, 2, NA, 2, 2, 2, NA, 1, 2, NA))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, NA, 1, 1, 1, 1, 1, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, 3, 3, 3, 2, 2, NA, 2))
				})

				test_that("sdc without missing, k=3, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, NA, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, NA, 1, 1, 1, 1, 1, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, 3, 3, 3, 3, 2, 2, NA, 2))
				})
			}





			# "sdc without missing, k=3, importance=c(1,2,3)
			{
				test_that("sdc without missing, k=3, importance=c(1,2,3), combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(2, 2, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, NA, 1, 1, 1, 1, 1, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(NA, NA, NA, 3, 3, 3, 2, 2, 1, 2))
				})

				test_that("sdc without missing, k=3, importance=c(1,2,3), combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(2, 2, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, NA, 1, 1, 1, 1, 1, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, 2, NA, 3, 3, 3, 2, 2, NA, 2))
				})

				test_that("sdc without missing, k=3, importance=c(1,2,3), combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(2, 2, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, NA, 1, 1, 1, 1, 1, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(NA, NA, NA, 3, 3, 3, 2, 2, 1, 2))
				})
			}
		}
	}









	# sdc with missing
	{
		set.seed(3)
		sample_Data <- runif(30, 1, 100)
		sample_Data <- array(sample_Data, dim = c(10,3))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
		colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
		sample_Data[c(1,2,7),4] <- NA
		sample_Data[c(2,6,9),5] <- NA
		sample_Data[c(2,10),6] <- NA
		sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
		sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
		sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
		sample_Data <- createSdcObj(sample_Data, keyVars = c("Var4", "Var5", "Var6"), numVars = c("Var1", "Var2", "Var3"))

		# "sdc without missing, k = 2
		{
			# "sdc without missing, k = 2, importance=NULL
			{
				test_that("sdc without missing, k = 2, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})

				test_that("sdc without missing, k = 2, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})

				test_that("sdc without missing, k = 2, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=NULL, combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})
			}




			# "sdc without missing, k = 2, importance=c(3,2,1)
			{
				test_that("sdc without missing, k = 2, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})

				test_that("sdc without missing, k = 2, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})

				test_that("sdc without missing, k = 2, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(3,2,1), combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})
			}





			# "sdc without missing, k = 2, importance=c(1,2,3)
			{
				test_that("sdc without missing, k = 2, importance=c(1,2,3), combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})

				test_that("sdc without missing, k = 2, importance=c(1,2,3), combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})

				test_that("sdc without missing, k = 2, importance=c(1,2,3), combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k =2, importance=c(1,2,3), combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})
			}
		}









		# "sdc without missing, k=3
		{
			# "sdc without missing, k=3, importance=NULL
			{
				test_that("sdc without missing, k=3, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, NA))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, NA, 3, 3, 3, 2, 2, NA, NA))
				})

				test_that("sdc without missing, k=3, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, NA))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})

				test_that("sdc without missing, k=3, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=NULL, combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, NA))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, NA, 3, 3, 3, 2, 2, NA, NA))
				})
			}




			# "sdc without missing, k=3, importance=c(3,2,1)
			{
				test_that("sdc without missing, k=3, importance=NULL, combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, NA, 2, 2, 2, NA, 1, NA, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})

				test_that("sdc without missing, k=3, importance=NULL, combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, NA))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})

				test_that("sdc without missing, k=3, importance=NULL, combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(3,2,1), combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, NA, 2, 2, 2, NA, 1, NA, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, 2))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, 1, NA))
				})
			}





			# "sdc without missing, k=3, importance=c(1,2,3)
			{
				test_that("sdc without missing, k=3, importance=c(1,2,3), combs=NULL)", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=NULL)
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, NA, 3, 3, 3, 2, 2, NA, NA))
				})

				test_that("sdc without missing, k=3, importance=c(1,2,3), combs=c(1:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=c(1:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, 3, 3, 3, 3, 2, 2, NA, NA))
				})

				test_that("sdc without missing, k=3, importance=c(1,2,3), combs=c(3:2))", {
					sample_Data <- localSuppression(sample_Data, k=3, importance=c(1,2,3), combs=c(3:2))
					expect_equal(sample_Data@manipKeyVars[,1], c(NA, NA, 1, 2, 2, 2, NA, 1, 2, 1))
					expect_equal(sample_Data@manipKeyVars[,2], c(1, NA, 1, 2, 1, NA, 1, 1, NA, NA))
					expect_equal(sample_Data@manipKeyVars[,3], c(2, NA, NA, 3, 3, 3, 2, 2, NA, NA))
				})
			}
		}
	}
}
