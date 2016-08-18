# ### comment MT: 
# ### Region, Status and Age group are categorical variables usually
# ### saved of class factor in R
# ### addNoise should NOT BE APPLIED on such variables.
# ### What does it mean, to have a region like 1.128993? after adding noise?
# df <- data.frame("region"=c(1,2,3,4),
#                  "status"=c(5,6,7,8),
#                  "agegroup"=c(9,10,11,12))
# 
# test_that("data frame method gives the same as method on sdcMicro object", {
#   fr <- addNoise(df, variables = c("region", "status", "agegroup"))
#   sdc <- createSdcObj(df, keyVars = 1:3)
#   sdc <- addNoise(sdc, variables = c("region", "status", "agegroup"))
#   expect_equal(fr, sdc)
# })
# 
# #############################################
# 
# ### Create realistic small data sets to test addNoise directly and within sdcMicroClass objects
# ## d1) data set without missing values of class data.frame with all numeric entries and more rows than columns
# ## d2) same data set but of class matrix
# ## d3) same data sets but including missing values
# ## d4) same data sets with higher amount of missing rates even one variable and one row with only NA
# ## d5) data set including a factor variable (system should give an error or warning)
# ## d6) data frame with only one column
# ## d7) data frame with more columns than rows
# 
# ### Test addNoise on d1 till d7
# ## for all methods (6 methods)
# ## for different amount of noises
# ## test for application of addNoise on data.frame and of sdcMicroObj.
# ## test if the output has the desired/expected structure
# 
# ### check outputs if they look like expected.
# ## 1) use a seed, e.g. set.seed(123)
# ## 2) do addNoise for the situations above and do an 
# ## 3) apply over all columns, calculating the mean of noised variables.
# ## 4) notice these means forever and compare it to
# ## 5) actual caluclations.
# ## 6) expect_equal from the result.
# ## --> the aim is that you once do the calculation, notice the results and compare it to the check results.
# ## --> e.g. in the simplest case, imagine you have a 2-dimenional data set,
# ## for that you calculate m <- apply(addNoise(....)$..., 2, mean, na.rm=TRUE) 
# ## you noticed that  is equal m <- c(2.234234, 8.898908) for example.
# ## then run the test when checking the package and expect_equal(m, meanaddNoiseResult) should be equal.
# ## This keeps the package stable over time.
# 
# ## this results in more than 100 tests for addNoise.
# 
