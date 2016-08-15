library(testthat)


# data frame
{	
	# data frame without missing
	{
		set.seed(3)
		sample_Data <- runif(20, 1, 100)
		sample_Data <- array(sample_Data, dim = c(10,2))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
		colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
		sample_Data[,3] <- as.factor(as.numeric(sample_Data[,3]))
		sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
		sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
		sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))

		{
			{
				test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"))
					expect_equal(fr[[7]], c(2, 1, 1, 1, 2, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
				})
			}


			{
				test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1)
					expect_equal(fr[[7]], c(2, 1, 1, 1, 2, 2, 2, 1, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2))
				})
			}




			{
				test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"))
					expect_equal(fr[[7]], c(2, 1, 1, 1, 1, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
				})
			}


			{
				test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1)
					expect_equal(fr[[7]], c(2, 1, 1, 1, 1, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
				})
			}
		}









		{
			{
				test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"))
					expect_equal(fr[[7]], c(2, 1, 1, 1, 2, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})
			}


			{
				test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1)
					expect_equal(fr[[7]], c(2, 1, 1, 1, 2, 2, 2, 1, 2, 2))
					expect_equal(fr[[8]], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})
			}




			{
				test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"))
					expect_equal(fr[[7]], c(2, 1, 1, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})
			}


			{
				test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1)
					expect_equal(fr[[7]], c(2, 1, 1, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})
			}
		}
	}



















	# data frame with missing
	{
		set.seed(3)
		sample_Data <- runif(20, 1, 100)
		sample_Data <- array(sample_Data, dim = c(10,2))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
		colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
		sample_Data[,3] <- as.factor(as.numeric(sample_Data[,3]))
		sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
		sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
		sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
		sample_Data[c(1,3,4,9),1] <- NA
		sample_Data[c(3,10),2] <- NA
		sample_Data[c(3),3] <- NA
		sample_Data[c(1,3,6),4] <- NA
		sample_Data[c(2,3,7,10),5] <- NA
		sample_Data[c(3,8),6] <- NA

		{
			{
				test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"))
					expect_equal(fr[[7]], c(1, 1, NA, 1, 2, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})
			}


			{
				test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1)
					expect_equal(fr[[7]], c(1, 1, 1, 1, 2, 1, 2, 1, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})
			}




			{
				test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"))
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})
			}


			{
				test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1)
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})
			}
		}









		{
			{
				test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"))
					expect_equal(fr[[7]], c(1, 1, NA, 1, 2, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 1))
				})
			}


			{
				test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1)
					expect_equal(fr[[7]], c(1, 1, 1, 1, 2, 1, 2, 1, 2, 2))
					expect_equal(fr[[8]], c(1, 1, 1, 1, 1, NA, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 1))
				})
			}




			{
				test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"))
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 3))
				})

				test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 1))
				})
			}


			{
				test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1)
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 3))
				})

				test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
					expect_equal(fr[[7]], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(fr[[8]], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 3))
				})
			}
		}
	}



















	# data frame with more columns then rows with missing
	{
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

		{

			{
				test_that("data frame without missing, variables=3, strata_variables=3, pd = default, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var6", "Var7", "Var8"), strata_variables=c("Var9", "Var10", "Var11"))
					expect_equal(fr[[12]], c(NA, 1, NA, NA))
					expect_equal(fr[[13]], c(1, NA, NA, 3))
					expect_equal(fr[[14]], c(2, 3, NA, 1))
				})

				test_that("data frame without missing, variables=3, strata_variables=3, pd = default, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var6", "Var7", "Var8"), strata_variables=c("Var9", "Var10", "Var11"), alpha = 0.1)
					expect_equal(fr[[12]], c(NA, 1, NA, NA))
					expect_equal(fr[[13]], c(1, NA, NA, 3))
					expect_equal(fr[[14]], c(2, 3, NA, 1))
				})
			}


			{
				test_that("data frame without missing, variables=3, strata_variables=3, pd = different, alpha = default)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var6", "Var7", "Var8"), strata_variables=c("Var9", "Var10", "Var11"), pd = 0.1)
					expect_equal(fr[[12]], c(NA, 1, NA, NA))
					expect_equal(fr[[13]], c(1, NA, NA, 3))
					expect_equal(fr[[14]], c(2, 3, NA, 1))
				})

				test_that("data frame without missing, variables=3, strata_variables=3, pd = different, alpha = different)", {
					set.seed(123)
					fr <- pram(sample_Data, variables = c("Var6", "Var7", "Var8"), strata_variables=c("Var9", "Var10", "Var11"), pd = 0.1, alpha = 0.1)
					expect_equal(fr[[12]], c(NA, 1, NA, NA))
					expect_equal(fr[[13]], c(1, NA, NA, 3))
					expect_equal(fr[[14]], c(2, 3, NA, 1))
				})
			}
		}
	}
}



















# sdc
{
	# sdc without missing values
	{
		set.seed(3)
		sample_Data <- runif(20, 1, 100)
		sample_Data <- array(sample_Data, dim = c(10,2))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
		colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
		sample_Data[,3] <- as.factor(as.numeric(sample_Data[,3]))
		sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
		sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
		sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
		sample_Data <- createSdcObj(sample_Data, keyVars=c("Var1", "Var2"))

		{
			{
				test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"))
					expect_equal(sample_Data@manipPramVars[,1], c(2, 1, 1, 1, 2, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
				})
			}


			{
				test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(2, 1, 1, 1, 2, 2, 2, 1, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2))
				})
			}




			{
				test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"))
					expect_equal(sample_Data@manipPramVars[,1], c(2, 1, 1, 1, 1, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
				})
			}


			{
				test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(2, 1, 1, 1, 1, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
				})
			}
		}









		{
			{
				test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"))
					expect_equal(sample_Data@manipPramVars[,1], c(2, 1, 1, 1, 2, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})
			}


			{
				test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(2, 1, 1, 1, 2, 2, 2, 1, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})
			}




			{
				test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"))
					expect_equal(sample_Data@manipPramVars[,1], c(2, 1, 1, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})
			}


			{
				test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(2, 1, 1, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(1, 1, 1, 1, 1, 1, 3, 1, 2, 1))
				})
			}
		}	
	}









	# sdc with missing values
	{
		set.seed(3)
		sample_Data <- runif(20, 1, 100)
		sample_Data <- array(sample_Data, dim = c(10,2))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
		colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
		sample_Data[,3] <- as.factor(as.numeric(sample_Data[,3]))
		sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
		sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
		sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
		sample_Data[c(1,3,4,9),1] <- NA
		sample_Data[c(3,10),2] <- NA
		sample_Data[c(3),3] <- NA
		sample_Data[c(1,3,6),4] <- NA
		sample_Data[c(2,3,7,10),5] <- NA
		sample_Data[c(3,8),6] <- NA
		sample_Data <- createSdcObj(sample_Data, keyVars=c("Var1", "Var2"))



		{
			{
				test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"))
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 2, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})
			}


			{
				test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, 1, 1, 2, 1, 2, 1, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})
			}




			{
				test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"))
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})
			}


			{
				test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})

				test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
				})
			}
		}
	



		{
			{
				test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"))
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 2, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 1))
				})
			}


			{
				test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, 1, 1, 2, 1, 2, 1, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(1, 1, 1, 1, 1, NA, 3, 1, 2, 1))
				})

				test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 1))
				})
			}




			{
				test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"))
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 3))
				})

				test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 1))
				})
			}


			{
				test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 3))
				})

				test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(1, 1, NA, 1, 1, 2, 2, 2, 2, 2))
					expect_equal(sample_Data@manipPramVars[,2], c(NA, 1, NA, 1, 1, NA, 3, 1, 2, 3))
				})
			}
		}
	}









	# sdc with more columns then rows with missing
	{
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
		sample_Data <- createSdcObj(sample_Data, keyVars=c("Var1", "Var2", "Var3", "Var4", "Var5"))


		{
			{
				test_that("data frame without missing, variables=3, strata_variables=3, pd = default, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var6", "Var7", "Var8"), strata_variables=c("Var9", "Var10", "Var11"))
					expect_equal(sample_Data@manipPramVars[,1], c(NA, 1, NA, NA))
					expect_equal(sample_Data@manipPramVars[,2], c(1, NA, NA, 3))
					expect_equal(sample_Data@manipPramVars[,3], c(2, 3, NA, 1))
				})

				test_that("data frame without missing, variables=3, strata_variables=3, pd = default, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var6", "Var7", "Var8"), strata_variables=c("Var9", "Var10", "Var11"), alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(NA, 1, NA, NA))
					expect_equal(sample_Data@manipPramVars[,2], c(1, NA, NA, 3))
					expect_equal(sample_Data@manipPramVars[,3], c(2, 3, NA, 1))
				})
			}


			{
				test_that("data frame without missing, variables=3, strata_variables=3, pd = different, alpha = default)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var6", "Var7", "Var8"), strata_variables=c("Var9", "Var10", "Var11"), pd = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(NA, 1, NA, NA))
					expect_equal(sample_Data@manipPramVars[,2], c(1, NA, NA, 3))
					expect_equal(sample_Data@manipPramVars[,3], c(2, 3, NA, 1))
				})

				test_that("data frame without missing, variables=3, strata_variables=3, pd = different, alpha = different)", {
					set.seed(123)
					sample_Data <- pram(sample_Data, variables = c("Var6", "Var7", "Var8"), strata_variables=c("Var9", "Var10", "Var11"), pd = 0.1, alpha = 0.1)
					expect_equal(sample_Data@manipPramVars[,1], c(NA, 1, NA, NA))
					expect_equal(sample_Data@manipPramVars[,2], c(1, NA, NA, 3))
					expect_equal(sample_Data@manipPramVars[,3], c(2, 3, NA, 1))
				})
			}
		}
	}
}