# test_that("suda2 data frame",{
#   data(testdata2)
#   data_suda2 <- suda2(testdata2,variables=c("urbrur","roof","walls","water","sex"))
#   expect_true(data_suda2$score[1]==2)
# })
# 
# test_that("suda2 sdcObject non original scores",{
#   ## for objects of class sdcMicro:
#   data(testdata2)
#   sdc <- createSdcObj(testdata2,
#                       keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#                       numVars=c('expend','income','savings'), w='sampling_weight')
#   sdc <- suda2(sdc, original_scores=FALSE)
#   expect_true(round(sdc@risk$suda2$score[1],2)==2.73)
# })
# 
# test_that("suda2 sdcObject  original scores",{
#   ## for objects of class sdcMicro:
#   data(testdata2)
#   sdc <- createSdcObj(testdata2,
#                       keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#                       numVars=c('expend','income','savings'), w='sampling_weight')
#   sdc <- suda2(sdc, original_scores=TRUE)
#   expect_true(round(sdc@risk$suda2$score[1],2)==180)
# })
# 
# test_that("suda2 simple example contrubtion",{
#   a <- c(2, 1, 1)
#   b <- c(0, 3, 0)
#   c <- c(0, 6, 0)
#   dat <- data.frame(a,b,c)
#   su <- suda2(dat)
#   
#   expect_true(all(su$attribute_contributions$contribution==c(50,37.5,37.5)))
# })
# 
# test_that("suda2 simple example contrubtion 2",{
#   a <- c(2, 1, 1)
#   b <- c(0, 3, 0)
#   c <- c(0, 6, 0)
#   dat <- data.frame(a,c,b)
#   su <- suda2(dat)
#   
#   expect_true(all(su$attribute_contributions$contribution==c(50,37.5,37.5)))
# })
# 
# test_that("suda2 simple example contrubtion 3",{
#   a <- c(2, 1, 1)
#   b <- c(0, 3, 0)
#   c <- c(0, 6, 0)
#   dat <- data.frame(c,a,b)
#   su <- suda2(dat)
#   
#   expect_true(all(su$attribute_contributions$contribution==c(37.5,50,37.5)))
# })
# 
# test_that("suda2 simple example contrubtion 4",{
#   a <- c(2, 1, 1)
#   b <- c(0, 3, 0)
#   c <- c(0, 6, 0)
#   dat <- data.frame(c,b,a)
#   su <- suda2(dat)
#   
#   expect_true(all(su$attribute_contributions$contribution==c(37.5,37.5,50)))
# })
# 
# test_that("suda2 book example",{
#   tab <- data.frame("age" = c(rep("20s", 7), "60s"),
#                     "gender" = c(rep("male", 4), rep("female", 3), "male"),
#                     "income" = c("50k+", "50k+", rep("50k-", 6)), "education" = c(rep("highschool", 4), "university", "highschool", "middleschool", "university"))
#   
#   su <- suda2(tab)
#   expect_equal(su$score[5:8],c(4,2,6,8))
# })
# 
# 
# test_that("suda2 sdc guideline",{
#   testdat <- data.frame(Residence = c("Urban", "Urban", "Urban", "Urban", "Rural", "Urban", "Urban", 
#                                       "Urban", "Urban", "Urban"),
#                         Gender = c("Female", "Female", "Female", "Male", "Female", "Male", "Female", 
#                                    "Male", "Female", "Female"),
#                         Education = c("Secondary incomplete", "Secondary incomplete", "Primary incomplete", 
#                                       "Secondary complete", "Secondary complete", "Secondary complete", 
#                                       "Primary complete", "Post-secondary", "Secondary incomplete", 
#                                       "Secondary incomplete"),
#                         Labor = c("Employed", "Employed", "Non-LF", "Employed", "Unemployed", 
#                                   "Employed", "Non-LF", "Unemployed", "Non-LF", "Non-LF"))
#   su <- suda2(testdat)
#   expect_equal(su$score,c(0,0,6,0,12,0,6,10,0,0))
# })
# 
# # dataframe
# # dataframe without missing
# set.seed(3)
# sample_Data <- runif(30, 1, 100)
# sample_Data <- array(sample_Data, dim = c(10,3))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
# sample_Data <- as.data.frame(sample_Data)
# colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
# sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
# sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
# sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
# 
# 
# test_that("data frame without missing, Original = FALSE)", {
#   sample_DataX <- suda2(sample_Data, variables=c("Var4", "Var5", "Var6"), original = FALSE)
#   expect_equal(sample_Data[[3]], c(23.5919862471055, 2.51765935891308, 13.7691743019968, 10.2448109227698, 
#                                    24.4516157396138, 79.3235935377888, 60.3734250029083, 91.1046233826783, 
#                                    56.4820308389608, 75.8147721474525))
# })
# 
# 
# test_that("data frame without missing, Original = TRUE)", {
#   sample_DataX <- suda2(sample_Data, variables=c("Var4", "Var5", "Var6"), original = TRUE)
#   expect_equal(sample_Data[[3]], c(23.5919862471055, 2.51765935891308, 13.7691743019968, 10.2448109227698, 
#                                    24.4516157396138, 79.3235935377888, 60.3734250029083, 91.1046233826783, 
#                                    56.4820308389608, 75.8147721474525))
# })
# 
# 
# test_that("data frame without missing, DisFraction = different)", {
#   sample_DataX <- suda2(sample_Data, variables=c("Var4", "Var5", "Var6"), DisFraction = 0.1)
#   expect_equal(sample_Data[[3]], c(23.5919862471055, 2.51765935891308, 13.7691743019968, 10.2448109227698, 
#                                    24.4516157396138, 79.3235935377888, 60.3734250029083, 91.1046233826783, 
#                                    56.4820308389608, 75.8147721474525)
#                
#   )
# })
# 
# 
# 
# # sdc
# # sdc without missing
# set.seed(3)
# sample_Data <- runif(30, 1, 100)
# sample_Data <- array(sample_Data, dim = c(10,3))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
# sample_Data <- as.data.frame(sample_Data)
# colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
# sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
# sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
# sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
# sample_Data <- createSdcObj(sample_Data, keyVars = c("Var4", "Var5", "Var6"), numVars = c("Var1", "Var2", "Var3"))
# 
# 
# test_that("sdc without missing, DisFraction = default)", {
#   sample_DataX <- suda2(sample_Data)
#   expect_equal(sample_DataX@risk$suda2$contributionPercent[,1], c(0, 0, 0, 0, 0.5, 0, 0.5, 0, 0.666666666666667, 0.5))
#   expect_equal(sample_DataX@risk$suda2$score, c(0, 1, 0, 2, 2, 0, 4, 0, 3, 2))
# })