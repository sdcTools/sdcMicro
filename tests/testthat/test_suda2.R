library(testthat)

# dataframe
{
	# dataframe without missing
	{
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


		test_that("data frame without missing, DisFraction = default)", {
			sample_Data <- suda2(sample_Data, variables=c("Var4", "Var5", "Var6"))
			expect_equal((sample_Data[[1]])[,1], c(0.00, 0.00, 0.00, 0.00, 0.50, 0.00, 0.50, 0.00, 0.75, 0.50))
			expect_equal((sample_Data[[1]])[,2], c(0.00, 1.00, 0.00, 1.00, 1.00, 0.00, 0.50, 0.00, 0.25, 0.50))
			expect_equal((sample_Data[[1]])[,3], c(0.00, 1.00, 0.00, 0.00, 0.50, 0.00, 0.00, 0.00, 0.25, 1.00))
			expect_equal(round((sample_Data[[2]]), digits = 7), c(0.0000000, 0.3333333, 0.0000000, 1.0000000, 0.6666667, 0.0000000, 2.0000000, 0.0000000, 1.3333333, 0.6666667))
		 	expect_equal(sample_Data[[3]], c(0.000000000, 0.005967813, 0.000000000, 0.023154855, 0.014078124, 0.000000000, 0.053368476, 0.000000000, 0.032846213, 0.014078124))
		})

		test_that("data frame without missing, DisFraction = different)", {
			sample_Data <- suda2(sample_Data, variables=c("Var4", "Var5", "Var6"), DisFraction = 0.1)
			expect_equal((sample_Data[[1]])[,1], c(0.00, 0.00, 0.00, 0.00, 0.50, 0.00, 0.50, 0.00, 0.75, 0.50))
			expect_equal((sample_Data[[1]])[,2], c(0.00, 1.00, 0.00, 1.00, 1.00, 0.00, 0.50, 0.00, 0.25, 0.50))
			expect_equal((sample_Data[[1]])[,3], c(0.00, 1.00, 0.00, 0.00, 0.50, 0.00, 0.00, 0.00, 0.25, 1.00))
			expect_equal(round((sample_Data[[2]]), digits = 7), c(0.0000000, 0.3333333, 0.0000000, 1.0000000, 0.6666667, 0.0000000, 2.0000000, 0.0000000, 1.3333333, 0.6666667))
		 	expect_equal(sample_Data[[3]], c(0.00000000, 0.06194895, 0.00000000, 0.20681556, 0.13574852, 0.00000000, 0.38277308, 0.00000000, 0.27197489, 0.13574852))
		})
	}




	# dataframe with missing
	{
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
		sample_Data[c(1,4,8),1] <- NA
		sample_Data[c(2,6,10),2] <- NA
		sample_Data[c(8,9),3] <- NA
		sample_Data[c(5,7,8,10),4] <- NA
		sample_Data[c(1),1] <- NA
		sample_Data[c(1,4,8),1] <- NA
		

		test_that("sdc with missing, DisFraction = default)", {
			sample_Data <- suda2(sample_Data, variables=c("Var4", "Var5", "Var6"))
			expect_equal((sample_Data[[1]])[,1], c(0.00, 0.50, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.75, 0.00))
			expect_equal((sample_Data[[1]])[,2], c(0.00, 1.00, 0.00, 1.00, 1.00, 0.00, 1.00, 0.00, 0.25, 1.00))
			expect_equal((sample_Data[[1]])[,3], c(0.00, 0.50, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.25, 1.00))
			expect_equal(round((sample_Data[[2]]), digits = 7), c(0.0000000, 0.6666667, 0.0000000, 1.0000000, 0.3333333, 0.0000000, 1.0000000, 0.0000000, 1.3333333, 0.3333333))
		 	expect_equal((sample_Data[[3]]), c(0.00000000, 0.03594315, 0.00000000, 0.05828377, 0.01543373, 0.00000000, 0.05828377, 0.00000000, 0.08145219, 0.01543373))
		})

		test_that("sdc with missing, DisFraction = different)", {
			sample_Data <- suda2(sample_Data, variables=c("Var4", "Var5", "Var6"), DisFraction = 0.1)
			expect_equal((sample_Data[[1]])[,1], c(0.00, 0.50, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.75, 0.00))
			expect_equal((sample_Data[[1]])[,2], c(0.00, 1.00, 0.00, 1.00, 1.00, 0.00, 1.00, 0.00, 0.25, 1.00))
			expect_equal((sample_Data[[1]])[,3], c(0.00, 0.50, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.25, 1.00))
			expect_equal(round((sample_Data[[2]]), digits = 7), c(0.0000000, 0.6666667, 0.0000000, 1.0000000, 0.3333333, 0.0000000, 1.0000000, 0.0000000, 1.3333333, 0.3333333))
		 	expect_equal(round((sample_Data[[3]]), digits = 7), c(0.0000000, 0.2908383, 0.0000000, 0.4050456, 0.1470723, 0.0000000, 0.4050456, 0.0000000, 0.4937797, 0.1470723))
		})
	}




	# data frame with one column with missing value
	{
		set.seed(3)
		sample_Data <- runif(10, 1, 100)
		sample_Data <- array(sample_Data, dim = c(10,1))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
		colnames(sample_Data) <- c("Var1", "Var2")
		sample_Data[,2] <- as.factor(as.numeric(sample_Data[,2]))
		sample_Data[c(1,4,8),1] <- NA
		sample_Data[c(2,6,10),2] <- NA
		
		test_that("sdc without missing, DisFraction = default)", {
			expect_warning(suda2(sample_Data, variables=c("Var2")),"This version of Suda2 can find MSUs only in Dataset with more than 2 variables.\nDummy variables have been added and the result might be wrong!")
			sample_Data <- suda2(sample_Data, variables=c("Var2"))
			expect_equal(sample_Data[[1]], c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0))
			expect_equal(sample_Data[[2]], c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0))
		 	expect_equal(sample_Data[[3]], c(0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.005025126, 0.000000000, 0.000000000, 0.000000000))
		})

		test_that("sdc without missing, DisFraction = different)", {
			expect_warning(suda2(sample_Data, variables=c("Var2"), DisFraction = 0.1),"This version of Suda2 can find MSUs only in Dataset with more than 2 variables.\nDummy variables have been added and the result might be wrong!")
			sample_Data <- suda2(sample_Data, variables=c("Var2"), DisFraction = 0.1)
			expect_equal(sample_Data[[1]], c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0))
			expect_equal(sample_Data[[2]], c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0))
		 	expect_equal(sample_Data[[3]], c(0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.05263158, 0.000000000, 0.000000000, 0.000000000))
		})
	}




	# data frame with more columns then rows with missing value
	{
		set.seed(3)
		sample_Data <- runif(30, 1, 100)
		sample_Data <- array(sample_Data, dim = c(5,6))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 4), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 6), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 3), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 4), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 6), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 3), digits = 0))
		colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9", "Var10", "Var11", "Var12")
		sample_Data[,7] <- as.factor(as.numeric(sample_Data[,7]))
		sample_Data[,8] <- as.factor(as.numeric(sample_Data[,8]))
		sample_Data[,9] <- as.factor(as.numeric(sample_Data[,9]))
		sample_Data[,10] <- as.factor(as.numeric(sample_Data[,10]))
		sample_Data[,11] <- as.factor(as.numeric(sample_Data[,11]))
		sample_Data[,12] <- as.factor(as.numeric(sample_Data[,12]))
		sample_Data[c(1,4),7] <- NA
		sample_Data[c(2,3),8] <- NA
		sample_Data[c(5),11] <- NA
		sample_Data[c(1),12] <- NA
		

		test_that("sdc without missing, DisFraction = default)", {
			sample_Data <- suda2(sample_Data, variables=c("Var7", "Var8", "Var9", "Var10", "Var11", "Var12"))
			expect_equal(round((sample_Data[[1]])[,1], digits = 7), c(0.0000000, 0.1090909, 0.0810811, 0.0000000, 0.0810811))
			expect_equal(round((sample_Data[[1]])[,2], digits = 7), c(0.7209302, 0.0000000, 0.0000000, 0.4189189, 0.4189189))
			expect_equal(round((sample_Data[[1]])[,3], digits = 7), c(0.1395349, 0.5636364, 0.4189189, 0.0810811, 0.1621622))
			expect_equal(round((sample_Data[[1]])[,4], digits = 7), c(0.1395349, 0.2181818, 0.0810811, 0.0810811, 0.4189189))
			expect_equal(round((sample_Data[[1]])[,5], digits = 7), c(0.2790698, 0.3272727, 0.4189189, 0.4189189, 0.0000000))
			expect_equal(round((sample_Data[[1]])[,6], digits = 7), c(0.0000000, 0.2181818, 0.1621622, 0.1621622, 0.0810811))
			expect_equal(round(sample_Data[[2]], digits = 6), c(7.166667, 9.166667, 12.333333, 12.333333, 12.333333))
		 	expect_equal(sample_Data[[3]], c(1, 1, 1, 1, 1))
		})

		test_that("sdc without missing, DisFraction = default)", {
			sample_Data <- suda2(sample_Data, variables=c("Var7", "Var8", "Var9", "Var10", "Var11", "Var12"), DisFraction=0.1)
			expect_equal(round((sample_Data[[1]])[,1], digits = 7), c(0.0000000, 0.1090909, 0.0810811, 0.0000000, 0.0810811))
			expect_equal(round((sample_Data[[1]])[,2], digits = 7), c(0.7209302, 0.0000000, 0.0000000, 0.4189189, 0.4189189))
			expect_equal(round((sample_Data[[1]])[,3], digits = 7), c(0.1395349, 0.5636364, 0.4189189, 0.0810811, 0.1621622))
			expect_equal(round((sample_Data[[1]])[,4], digits = 7), c(0.1395349, 0.2181818, 0.0810811, 0.0810811, 0.4189189))
			expect_equal(round((sample_Data[[1]])[,5], digits = 7), c(0.2790698, 0.3272727, 0.4189189, 0.4189189, 0.0000000))
			expect_equal(round((sample_Data[[1]])[,6], digits = 7), c(0.0000000, 0.2181818, 0.1621622, 0.1621622, 0.0810811))
			expect_equal(round(sample_Data[[2]], digits = 6), c(7.166667, 9.166667, 12.333333, 12.333333, 12.333333))
		 	expect_equal(sample_Data[[3]], c(1, 1, 1, 1, 1))
		})
	}
}









# sdc
{
	# sdc without missing
	{
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


		test_that("sdc without missing, DisFraction = default)", {
			sample_Data <- suda2(sample_Data)
			expect_equal(sample_Data@risk$suda2$contributionPercent[,1], c(0.00, 0.00, 0.00, 0.00, 0.50, 0.00, 0.50, 0.00, 0.75, 0.50))
			expect_equal(sample_Data@risk$suda2$contributionPercent[,2], c(0.00, 1.00, 0.00, 1.00, 1.00, 0.00, 0.50, 0.00, 0.25, 0.50))
			expect_equal(sample_Data@risk$suda2$contributionPercent[,3], c(0.00, 1.00, 0.00, 0.00, 0.50, 0.00, 0.00, 0.00, 0.25, 1.00))
			expect_equal(round(sample_Data@risk$suda2$score, digits = 7), c(0.0000000, 0.3333333, 0.0000000, 1.0000000, 0.6666667, 0.0000000, 2.0000000, 0.0000000, 1.3333333, 0.6666667))
		 	expect_equal(sample_Data@risk$suda2$disScore, c(0.000000000, 0.005967813, 0.000000000, 0.023154855, 0.014078124, 0.000000000, 0.053368476, 0.000000000, 0.032846213, 0.014078124))
		})

		test_that("sdc without missing, DisFraction = different)", {
			sample_Data <- suda2(sample_Data, DisFraction = 0.1)
			expect_equal(sample_Data@risk$suda2$contributionPercent[,1], c(0.00, 0.00, 0.00, 0.00, 0.50, 0.00, 0.50, 0.00, 0.75, 0.50))
			expect_equal(sample_Data@risk$suda2$contributionPercent[,2], c(0.00, 1.00, 0.00, 1.00, 1.00, 0.00, 0.50, 0.00, 0.25, 0.50))
			expect_equal(sample_Data@risk$suda2$contributionPercent[,3], c(0.00, 1.00, 0.00, 0.00, 0.50, 0.00, 0.00, 0.00, 0.25, 1.00))
			expect_equal(round(sample_Data@risk$suda2$score, digits = 7), c(0.0000000, 0.3333333, 0.0000000, 1.0000000, 0.6666667, 0.0000000, 2.0000000, 0.0000000, 1.3333333, 0.6666667))
		 	expect_equal(sample_Data@risk$suda2$disScore, c(0.00000000, 0.06194895, 0.00000000, 0.20681556, 0.13574852, 0.00000000, 0.38277308, 0.00000000, 0.27197489, 0.13574852))
		})
	}




	# sdc with missing
	{
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
		sample_Data[c(1,4,8),1] <- NA
		sample_Data[c(2,6,10),2] <- NA
		sample_Data[c(8,9),3] <- NA
		sample_Data[c(5,7,8,10),4] <- NA
		sample_Data[c(1),1] <- NA
		sample_Data[c(1,4,8),1] <- NA
		sample_Data <- createSdcObj(sample_Data, keyVars = c("Var4", "Var5", "Var6"), numVars = c("Var1", "Var2", "Var3"))


		test_that("sdc with missing, DisFraction = default)", {
			sample_Data <- suda2(sample_Data)
			expect_equal(sample_Data@risk$suda2$contributionPercent[,1], c(0.00, 0.50, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.75, 0.00))
			expect_equal(sample_Data@risk$suda2$contributionPercent[,2], c(0.00, 1.00, 0.00, 1.00, 1.00, 0.00, 1.00, 0.00, 0.25, 1.00))
			expect_equal(sample_Data@risk$suda2$contributionPercent[,3], c(0.00, 0.50, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.25, 1.00))
			expect_equal(round(sample_Data@risk$suda2$score, digits = 7), c(0.0000000, 0.6666667, 0.0000000, 1.0000000, 0.3333333, 0.0000000, 1.0000000, 0.0000000, 1.3333333, 0.3333333))
		 	expect_equal(sample_Data@risk$suda2$disScore, c(0.00000000, 0.03594315, 0.00000000, 0.05828377, 0.01543373, 0.00000000, 0.05828377, 0.00000000, 0.08145219, 0.01543373))
		})

		test_that("sdc with missing, DisFraction = different)", {
			sample_Data <- suda2(sample_Data, DisFraction = 0.1)
			expect_equal(sample_Data@risk$suda2$contributionPercent[,1], c(0.00, 0.50, 0.00, 0.00, 0.00, 0.00, 0.00, 0.00, 0.75, 0.00))
			expect_equal(sample_Data@risk$suda2$contributionPercent[,2], c(0.00, 1.00, 0.00, 1.00, 1.00, 0.00, 1.00, 0.00, 0.25, 1.00))
			expect_equal(sample_Data@risk$suda2$contributionPercent[,3], c(0.00, 0.50, 0.00, 0.00, 1.00, 0.00, 0.00, 0.00, 0.25, 1.00))
			expect_equal(round(sample_Data@risk$suda2$score, digits = 7), c(0.0000000, 0.6666667, 0.0000000, 1.0000000, 0.3333333, 0.0000000, 1.0000000, 0.0000000, 1.3333333, 0.3333333))
		 	expect_equal(round(sample_Data@risk$suda2$disScore, digits = 7), c(0.0000000, 0.2908383, 0.0000000, 0.4050456, 0.1470723, 0.0000000, 0.4050456, 0.0000000, 0.4937797, 0.1470723))
		})
	}




	# sdc with one column with missing value
	{
		set.seed(3)
		sample_Data <- runif(10, 1, 100)
		sample_Data <- array(sample_Data, dim = c(10,1))
		sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
		colnames(sample_Data) <- c("Var1", "Var2")
		sample_Data[,2] <- as.factor(as.numeric(sample_Data[,2]))
		sample_Data[c(1,4,8),1] <- NA
		sample_Data[c(2,6,10),2] <- NA
		sample_Data <- createSdcObj(sample_Data, keyVars = c("Var2"), numVars = c("Var1"))

		test_that("sdc without missing, DisFraction = default)", {
			sample_Data <- suda2(sample_Data)
			expect_warning(suda2(sample_Data) ,"This version of Suda2 can find MSUs only in Dataset with more than 2 variables.\nDummy variables have been added and the result might be wrong!")
			expect_equal(sample_Data@risk$suda2$contributionPercent, c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0))
			expect_equal(sample_Data@risk$suda2$score, c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0))
		 	expect_equal(sample_Data@risk$suda2$disScore, c(0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.005025126, 0.000000000, 0.000000000, 0.000000000))
		})

		test_that("sdc without missing, DisFraction = different)", {
			sample_Data <- suda2(sample_Data, DisFraction = 0.1)
			expect_warning(suda2(sample_Data) ,"This version of Suda2 can find MSUs only in Dataset with more than 2 variables.\nDummy variables have been added and the result might be wrong!")
			expect_equal(sample_Data@risk$suda2$contributionPercent, c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0))
			expect_equal(sample_Data@risk$suda2$score, c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0))
		 	expect_equal(sample_Data@risk$suda2$disScore, c(0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.000000000, 0.05263158, 0.000000000, 0.000000000, 0.000000000))
		})
	}




	# sdc with more columns then rows with missing value
	{
		set.seed(3)
		sample_Data <- runif(30, 1, 100)
		sample_Data <- array(sample_Data, dim = c(5,6))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 4), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 6), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 3), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 4), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 6), digits = 0))
		sample_Data <- cbind(sample_Data, round(runif(5, 1, 3), digits = 0))
		colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9", "Var10", "Var11", "Var12")
		sample_Data[,7] <- as.factor(as.numeric(sample_Data[,7]))
		sample_Data[,8] <- as.factor(as.numeric(sample_Data[,8]))
		sample_Data[,9] <- as.factor(as.numeric(sample_Data[,9]))
		sample_Data[,10] <- as.factor(as.numeric(sample_Data[,10]))
		sample_Data[,11] <- as.factor(as.numeric(sample_Data[,11]))
		sample_Data[,12] <- as.factor(as.numeric(sample_Data[,12]))
		sample_Data[c(1,4),7] <- NA
		sample_Data[c(2,3),8] <- NA
		sample_Data[c(5),11] <- NA
		sample_Data[c(1),12] <- NA
		sample_Data <- createSdcObj(sample_Data, keyVars = c("Var7", "Var8", "Var9", "Var10", "Var11", "Var12"), numVars = c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6"))


		test_that("sdc without missing, DisFraction = default)", {
			sample_Data <- suda2(sample_Data)
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,1], digits = 7), c(0.0000000, 0.1090909, 0.0810811, 0.0000000, 0.0810811))
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,2], digits = 7), c(0.7209302, 0.0000000, 0.0000000, 0.4189189, 0.4189189))
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,3], digits = 7), c(0.1395349, 0.5636364, 0.4189189, 0.0810811, 0.1621622))
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,4], digits = 7), c(0.1395349, 0.2181818, 0.0810811, 0.0810811, 0.4189189))
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,5], digits = 7), c(0.2790698, 0.3272727, 0.4189189, 0.4189189, 0.0000000))
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,6], digits = 7), c(0.0000000, 0.2181818, 0.1621622, 0.1621622, 0.0810811))
			expect_equal(round(sample_Data@risk$suda2$score, digits = 6), c(7.166667, 9.166667, 12.333333, 12.333333, 12.333333))
		 	expect_equal(sample_Data@risk$suda2$disScore, c(1, 1, 1, 1, 1))
		})

		test_that("sdc without missing, DisFraction = different)", {
			sample_Data <- suda2(sample_Data, DisFraction=0.1)
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,1], digits = 7), c(0.0000000, 0.1090909, 0.0810811, 0.0000000, 0.0810811))
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,2], digits = 7), c(0.7209302, 0.0000000, 0.0000000, 0.4189189, 0.4189189))
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,3], digits = 7), c(0.1395349, 0.5636364, 0.4189189, 0.0810811, 0.1621622))
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,4], digits = 7), c(0.1395349, 0.2181818, 0.0810811, 0.0810811, 0.4189189))
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,5], digits = 7), c(0.2790698, 0.3272727, 0.4189189, 0.4189189, 0.0000000))
			expect_equal(round(sample_Data@risk$suda2$contributionPercent[,6], digits = 7), c(0.0000000, 0.2181818, 0.1621622, 0.1621622, 0.0810811))
			expect_equal(round(sample_Data@risk$suda2$score, digits = 6), c(7.166667, 9.166667, 12.333333, 12.333333, 12.333333))
		 	expect_equal(sample_Data@risk$suda2$disScore, c(1, 1, 1, 1, 1))
		})
	}
}
