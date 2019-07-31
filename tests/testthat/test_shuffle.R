set.seed(3)
sample_Data <- runif(20, 1, 100)
sample_Data <- array(sample_Data, dim = c(10,2))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
sample_Data <- cbind(sample_Data, round(runif(10, 1, 100), digits = 0))
colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "weight")
sample_Data[,3] <- as.factor(as.numeric(sample_Data[,3]))
sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))


# ds
{
	# without weights
	{
		{
			{
				test_that("data frame without missing, method = ds, weights = NULL, covmethod = spearman, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "spearman", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing,method = ds, weights = NULL, covmethod = spearman, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "spearman", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = ds, weights = NULL, covmethod = spearman, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "spearman", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.89529, 45.89903, 45.86300, 45.87471, 45.88541, 55.07064, 22.66298, 55.14696, 38.91396, 55.12480),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.19949, 58.20685, 58.22932, 58.23248, 58.25111, 66.49342, 113.23155, 66.47868, 89.82965, 66.48468),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = ds, weights = NULL, covmethod = spearman, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "spearman", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}




		{
			{
				test_that("data frame without missing, method = ds, weights = NULL, covmethod = pearson, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "pearson", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = ds, weights = NULL, covmethod = pearson, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "pearson", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = ds, weights = NULL, covmethod = pearson, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "pearson", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.88383, 45.88378, 45.88326, 45.88333, 45.88323, 55.11681, 22.67232, 55.11758, 38.89528, 55.11734),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.22368, 58.22374, 58.22388, 58.22391, 58.22404, 66.48204, 113.22077, 66.48197, 89.85119, 66.48200),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = ds, weights = NULL, covmethod = pearson, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "pearson", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}





		{
			{
				test_that("data frame without missing, method = ds, weights = NULL, covmethod = mcd, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "mcd", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = ds, weights = NULL, covmethod = mcd, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "mcd", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = ds, weights = NULL, covmethod = mcd, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "mcd", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.88383, 45.88378, 45.88326, 45.88333, 45.88323, 55.11681, 22.67232, 55.11758, 38.89528, 55.11734),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.22368, 58.22374, 58.22388, 58.22391, 58.22404, 66.48204, 113.22077, 66.48197, 89.85119, 66.48200),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = ds, weights = NULL, covmethod = mcd, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = NULL, covmethod = "mcd", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}
	}








	# with weights
	{
		{
			{
				test_that("data frame without missing, method = ds, weights = yes, covmethod = spearman, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "spearman", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing,method = ds, weights = yes, covmethod = spearman, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "spearman", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = ds, weights = yes, covmethod = spearman, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "spearman", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.89529, 45.89903, 45.86300, 45.87471, 45.88541, 55.07064, 22.66298, 55.14696, 38.91396, 55.12480),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.19949, 58.20685, 58.22932, 58.23248, 58.25111, 66.49342, 113.23155, 66.47868, 89.82965, 66.48468),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = ds, weights = yes, covmethod = spearman, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "spearman", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}




		{
			{
				test_that("data frame without missing, method = ds, weights = yes, covmethod = pearson, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "pearson", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = ds, weights = yes, covmethod = pearson, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "pearson", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = ds, weights = yes, covmethod = pearson, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "pearson", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.88383, 45.88378, 45.88326, 45.88333, 45.88323, 55.11681, 22.67232, 55.11758, 38.89528, 55.11734),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.22368, 58.22374, 58.22388, 58.22391, 58.22404, 66.48204, 113.22077, 66.48197, 89.85119, 66.48200),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = ds, weights = yes, covmethod = pearson, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "pearson", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}





		{
			{
				test_that("data frame without missing, method = ds, weights = yes, covmethod = mcd, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "mcd", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = ds, weights = yes, covmethod = mcd, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "mcd", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = ds, weights = yes, covmethod = mcd, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "mcd", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.88383, 45.88378, 45.88326, 45.88333, 45.88323, 55.11681, 22.67232, 55.11758, 38.89528, 55.11734),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.22368, 58.22374, 58.22388, 58.22391, 58.22404, 66.48204, 113.22077, 66.48197, 89.85119, 66.48200),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = ds, weights = yes, covmethod = mcd, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "ds", weights = "weight", covmethod = "mcd", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}

	}
}



















# mvn
{
	# without weights
	{
		{
			{
				test_that("data frame without missing, method = mvn, weights = NULL, covmethod = spearman, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "spearman", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(58.18338, 60.83501, 60.60797, 30.16549, 33.44570, 80.94412, 17.63611, 13.33871, 63.46695, 39.10929),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(56.16769, 83.14116, 70.66515, 51.68957, 50.99737, 89.85134, 28.69352, 12.03347, 86.92403, 53.86950),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(46.70189, 48.09844, 47.97862, 45.12276, 45.26499, 50.08855, 44.99338, 44.39275, 48.92975, 45.56687),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(58.37521, 59.69312, 59.51102, 57.35352, 57.00657, 61.78223, 56.73883, 56.49007, 60.28109, 57.76408),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing,method = mvn, weights = NULL, covmethod = spearman, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "spearman", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(58.18338, 60.83501, 60.60797, 30.16549, 33.44570, 80.94412, 17.63611, 13.33871, 63.46695, 39.10929),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(56.16769, 83.14116, 70.66515, 51.68957, 50.99737, 89.85134, 28.69352, 12.03347, 86.92403, 53.86950),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(46.70189, 48.09844, 47.97862, 45.12276, 45.26499, 50.08855, 44.99338, 44.39275, 48.92975, 45.56687),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(58.37521, 59.69312, 59.51102, 57.35352, 57.00657, 61.78223, 56.73883, 56.49007, 60.28109, 57.76408),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = mvn, weights = NULL, covmethod = spearman, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "spearman", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(58.18338, 60.83501, 60.60797, 30.16549, 33.44570, 80.94412, 17.63611, 13.33871, 63.46695, 39.10929),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(56.16769, 83.14116, 70.66515, 51.68957, 50.99737, 89.85134, 28.69352, 12.03347, 86.92403, 53.86950),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(46.70189, 48.09844, 47.97862, 45.12276, 45.26499, 50.08855, 44.99338, 44.39275, 48.92975, 45.56687),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(58.37521, 59.69312, 59.51102, 57.35352, 57.00657, 61.78223, 56.73883, 56.49007, 60.28109, 57.76408),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.91907, 45.89445, 45.85630, 45.86915, 45.87847, 55.08588, 22.66015, 55.12449, 38.91962, 55.12919),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.25513, 58.18883, 58.25983, 58.20225, 58.21321, 66.48998, 113.20594, 66.43152, 89.88085, 66.50967),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = mvn, weights = NULL, covmethod = spearman, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "spearman", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(58.18338, 60.83501, 60.60797, 30.16549, 33.44570, 80.94412, 17.63611, 13.33871, 63.46695, 39.10929),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(56.16769, 83.14116, 70.66515, 51.68957, 50.99737, 89.85134, 28.69352, 12.03347, 86.92403, 53.86950),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(46.70189, 48.09844, 47.97862, 45.12276, 45.26499, 50.08855, 44.99338, 44.39275, 48.92975, 45.56687),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(58.37521, 59.69312, 59.51102, 57.35352, 57.00657, 61.78223, 56.73883, 56.49007, 60.28109, 57.76408),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}




		{
			{
				test_that("data frame without missing, method = mvn, weights = NULL, covmethod = pearson, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "pearson", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(39.10929, 33.44570, 17.63611, 60.60797, 58.18338, 13.33871, 80.94412, 60.83501, 30.16549, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(53.86950, 51.68957, 28.69352, 70.66515, 56.16769, 12.03347, 86.92403, 83.14116, 50.99737, 89.85134),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(47.82383, 47.43341, 46.95064, 47.91517, 47.83316, 46.46017, 48.31670, 48.20028, 47.21574, 48.20862),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(54.71699, 53.61276, 29.33908, 57.10081, 55.99944, 12.98064, 78.98764, 75.74346, 42.14465, 83.99853),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = mvn, weights = NULL, covmethod = pearson, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "pearson", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(39.10929, 33.44570, 17.63611, 60.60797, 58.18338, 13.33871, 80.94412, 60.83501, 30.16549, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(53.86950, 51.68957, 28.69352, 70.66515, 56.16769, 12.03347, 86.92403, 83.14116, 50.99737, 89.85134),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(47.82383, 47.43341, 46.95064, 47.91517, 47.83316, 46.46017, 48.31670, 48.20028, 47.21574, 48.20862),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(54.71699, 53.61276, 29.33908, 57.10081, 55.99944, 12.98064, 78.98764, 75.74346, 42.14465, 83.99853),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = mvn, weights = NULL, covmethod = pearson, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "pearson", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(30.16549, 63.46695, 33.44570, 17.63611, 39.10929, 13.33871, 60.60797, 60.83501, 58.18338, 80.94412),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(53.86950, 51.68957, 28.69352, 70.66515, 56.16769, 12.03347, 86.92403, 83.14116, 50.99737, 89.85134),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(46.71469, 47.63147, 46.72631, 46.68752, 46.80728, 46.54067, 47.59662, 47.60711, 47.17783, 48.31857),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(55.49635, 53.19770, 28.45937, 58.06731, 56.74943, 11.26634, 80.26103, 76.79256, 41.54228, 84.71077),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.88383, 45.88378, 45.88326, 45.88333, 45.88323, 55.11681, 22.67232, 55.11758, 38.89528, 55.11734),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.22368, 58.22374, 58.22388, 58.22391, 58.22404, 66.48204, 113.22077, 66.48197, 89.85119, 66.48200),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = mvn, weights = NULL, covmethod = pearson, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "pearson", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(30.16549, 63.46695, 33.44570, 17.63611, 39.10929, 13.33871, 60.60797, 60.83501, 58.18338, 80.94412),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(53.86950, 51.68957, 28.69352, 70.66515, 56.16769, 12.03347, 86.92403, 83.14116, 50.99737, 89.85134),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(46.71469, 47.63147, 46.72631, 46.68752, 46.80728, 46.54067, 47.59662, 47.60711, 47.17783, 48.31857),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(55.49635, 53.19770, 28.45937, 58.06731, 56.74943, 11.26634, 80.26103, 76.79256, 41.54228, 84.71077),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}





		{
			{
				test_that("data frame without missing, method = mvn, weights = NULL, covmethod = mcd, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "mcd", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(60.83501, 33.44570, 60.60797, 58.18338, 39.10929, 80.94412, 17.63611, 30.16549, 63.46695, 13.33871),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(50.99737, 70.66515, 51.68957, 53.86950, 56.16769, 12.03347, 86.92403, 83.14116, 28.69352, 89.85134),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(59.69405, 46.87544, 55.09455, 53.48729, 51.74148, 64.50678, 30.76465, 42.87662, 60.77683, 26.25907),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(34.30038, 54.46075, 41.53421, 44.06214, 46.80784, 26.73121, 79.79886, 60.74979, 32.59753, 86.88491),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = mvn, weights = NULL, covmethod = mcd, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "mcd", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(60.83501, 33.44570, 60.60797, 58.18338, 39.10929, 80.94412, 17.63611, 30.16549, 63.46695, 13.33871),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(50.99737, 70.66515, 51.68957, 53.86950, 56.16769, 12.03347, 86.92403, 83.14116, 28.69352, 89.85134),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(59.69405, 46.87544, 55.09455, 53.48729, 51.74148, 64.50678, 30.76465, 42.87662, 60.77683, 26.25907),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(34.30038, 54.46075, 41.53421, 44.06214, 46.80784, 26.73121, 79.79886, 60.74979, 32.59753, 86.88491),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = mvn, weights = NULL, covmethod = mcd, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "mcd", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.88383, 45.88378, 45.88326, 45.88333, 45.88323, 55.11681, 22.67232, 55.11758, 38.89528, 55.11734),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.22368, 58.22374, 58.22388, 58.22391, 58.22404, 66.48204, 113.22077, 66.48197, 89.85119, 66.48200),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = mvn, weights = NULL, covmethod = mcd, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = NULL, covmethod = "mcd", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}
	}








	# with weights
	{
		{
			{
				test_that("data frame without missing, method = mvn, weights = yes, covmethod = spearman, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "spearman", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing,method = mvn, weights = yes, covmethod = spearman, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "spearman", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = mvn, weights = yes, covmethod = spearman, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "spearman", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.89529, 45.89903, 45.86300, 45.87471, 45.88541, 55.07064, 22.66298, 55.14696, 38.91396, 55.12480),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.19949, 58.20685, 58.22932, 58.23248, 58.25111, 66.49342, 113.23155, 66.47868, 89.82965, 66.48468),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = mvn, weights = yes, covmethod = spearman, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "spearman", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(13.33871, 39.10929, 60.60797, 58.18338, 60.83501, 33.44570, 30.16549, 80.94412, 17.63611, 63.46695),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(51.68957, 53.86950, 86.92403, 70.66515, 56.16769, 89.85134, 83.14116, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.22549866, -0.40293955, 0.25705168, -0.08624228, 0.47221741, -0.48121782, -0.92758523, 1.38120696, -1.21628268, 0.49799109),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(-0.15379785, -0.13276082, 1.55339542, 0.06541226, -0.10156435, 2.30494134, 0.44432894, -1.73113980, -0.47805595, -0.46764360),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}




		{
			{
				test_that("data frame without missing, method = mvn, weights = yes, covmethod = pearson, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "pearson", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = mvn, weights = yes, covmethod = pearson, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "pearson", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = mvn, weights = yes, covmethod = pearson, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "pearson", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.88383, 45.88378, 45.88326, 45.88333, 45.88323, 55.11681, 22.67232, 55.11758, 38.89528, 55.11734),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.22368, 58.22374, 58.22388, 58.22391, 58.22404, 66.48204, 113.22077, 66.48197, 89.85119, 66.48200),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = mvn, weights = yes, covmethod = pearson, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "pearson", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}





		{
			{
				test_that("data frame without missing, method = mvn, weights = yes, covmethod = mcd, regmethod = lm, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "mcd", regmethod = "lm", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(46.36306, 46.36613, 46.32727, 46.33886, 46.34787, 54.66176, 22.99150, 54.74080, 38.87781, 54.71768),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(59.90370, 59.91081, 59.93723, 59.93928, 59.95715, 67.52680, 32.07718, 67.50421, 49.76392, 67.51253),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = mvn, weights = yes, covmethod = mcd, regmethod = lm, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "mcd", regmethod = "lm", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}

			{
				test_that("data frame without missing, method = mvn, weights = yes, covmethod = mcd, regmethod = MM, gadp = TRUE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "mcd", regmethod = "MM", gadp = TRUE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,1]), c(45.88383, 45.88378, 45.88326, 45.88333, 45.88323, 55.11681, 22.67232, 55.11758, 38.89528, 55.11734),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[3]][,2]), c(58.22368, 58.22374, 58.22388, 58.22391, 58.22404, 66.48204, 113.22077, 66.48197, 89.85119, 66.48200),  tolerance = 0.001)
				})	

				test_that("data frame without missing, method = mvn, weights = yes, covmethod = mcd, regmethod = MM, gadp = FALSE)", {
					set.seed(123)
					fr <- shuffle(sample_Data, form=Var1+Var2 ~ Var3+Var4, method = "mvn", weights = "weight", covmethod = "mcd", regmethod = "MM", gadp = FALSE) 
					expect_equal(as.numeric(fr[[1]][,1]), c(17.63611, 33.44570, 80.94412, 39.10929, 60.60797, 63.46695, 30.16549, 60.83501, 13.33871, 58.18338),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[1]][,2]), c(83.14116, 56.16769, 86.92403, 70.66515, 51.68957, 89.85134, 53.86950, 12.03347, 28.69352, 50.99737),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,1]), c(-1.08523724, -0.37842598, 0.86328396, -0.02948054, 0.37714376, 0.51097160, -0.63983094, 0.46100622, -1.25128210, 0.22180792),  tolerance = 0.001)
					expect_equal(as.numeric(fr[[2]][,2]), c(0.27279456, 0.03903341, 1.33662228, 0.11813974, -0.20183340, 2.43944212, -0.10730698, -1.72310211, -0.50971627, -0.32787781),  tolerance = 0.001)
					expect_equal(fr[[3]], NULL)
				})	
			}
		}

	}
}
