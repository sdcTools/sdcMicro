library(sdcMicro)
data(testdata)

# test_that("pram on a factor", {
#   ## application on a factor-variable
#   res <- pram(as.factor(testdata$roof))
#   print(res)
#   summary(res)
#   expect_false(identical(res$x,res$x_pram))
# })
# testdata$roof <- factor(testdata$roof)
# testdata$walls <- factor(testdata$walls)
# testdata$water <- factor(testdata$water)
# test_that("pram on a data.frame", {
#   ## application on a data.frame
#   ## pram can only be applied to factors, thus we have to recode
#   ## to factors before the method can be applied
#   
#   
#   ## pram() is applied within subgroups defined by
#   ## variables "urbrur" and "sex"
#   res <- pram(testdata, variables="roof",
#               strata_variables=c("urbrur","sex"))
#   expect_false(identical(res$roof,res$roof_pram))
# })
# 
# test_that("pram on a data.table", {
#   library(data.table)
#   testdataDT <- as.data.table(testdata)
#   set.seed(1)
#   res <- pram(testdataDT, variables="roof",
#               strata_variables=c("urbrur","sex"))
#   set.seed(1)
#   res2 <- pram(testdata, variables="roof",
#                strata_variables=c("urbrur","sex"))
#   expect_identical(res2$roof_pram,res$roof_pram)
# })
# 
# test_that("pram on a data.frame 2", {
#   ## default parameters (pd=0.8 and alpha=0.5) for the generation
#   ## of the invariant transition matrix will be used for all variables
#   res1 <- pram(testdata, variables=c("roof","walls","water"))
#   expect_false(all(res1$roof_pram==testdata$roof&res1$walls_pram==testdata$walls&res1$water_pram==testdata$water))
# })
# 
# test_that("pram on a data.frame, non default pd", {
#   ## specific parameters for each variable
#   res1 <- pram(testdata,variables=c("roof","walls","water"),
#                pd=c(0.95,0.8,0.9), alpha=0.5)
#   expect_false(all(res1$roof_pram==testdata$roof&res1$walls_pram==testdata$walls&res1$water_pram==testdata$water))
# })
# 
# test_that("pram on a data.frame, full matrix for pd", {
#   ## we can also specify a custom transition-matrix directly
#   # for variable roof; matrix must have rownames and colnames that match
#   # the levels of the variable that should be post-randomized
#   # rowSums() and colSums() must equal 1 too!
#   mat <- diag(length(levels(testdata$roof)))
#   rownames(mat) <- colnames(mat) <- levels(testdata$roof)
#   res1 <- pram(testdata,variables="roof", pd=mat)
#   expect_true(all(res1$roof_pram==testdata$roof))
#   ## it is possible use a transistion matrix for a variable and use the 'traditional' way
#   ## of specifying a number for the minimal diagonal entries of the transision matrix
#   ## for other variables. In this case we must supply \\code{pd} as list.
#   res1 <- pram(testdata,variables=c("roof","walls"), pd=list(mat,0.5), alpha=c(NA, 0.5))
#   expect_false(all(res1$roof_pram==testdata$roof&res1$walls_pram==testdata$walls))
# })
# 
# data(testdata2)
# testdata2$urbrur <- factor(testdata2$urbrur)
# test_that("pram on a sdcObj", {
#   ## application to objects of class sdcMicro with default parameters
#   
#   sdc <- createSdcObj(testdata2,
#                       keyVars=c('roof','walls','water','electcon','relat','sex'),
#                       numVars=c('expend','income','savings'), w='sampling_weight')
#   sdc <- pram(sdc, variables=c("urbrur"))
#   expect_true(is.data.table(sdc@pram$comparison$urbrur))
# })
# 
# 
# test_that("pram on a sdcObj 2", {
#   ## this is equal to the previous application. If argument 'variables' is NULL,
#   ## all variables from slot 'pramVars' will be used if possible.
#   sdc <- createSdcObj(testdata2,
#                       keyVars=c('roof','walls','water','electcon','relat','sex'),
#                       numVars=c('expend','income','savings'), w='sampling_weight',
#                       pramVars="urbrur")
#   sdc <- pram(sdc)
#   expect_true(is.data.table(sdc@pram$comparison$urbrur))
# })
# 
# test_that("pram on a sdcObj, matrix", {
#   ## we can specify transition matrices for sdcMicroObj-objects too
#   testdata2$roof <- factor(testdata2$roof)
#   sdc <- createSdcObj(testdata2,
#                       keyVars=c('roof','walls','water','electcon','relat','sex'),
#                       numVars=c('expend','income','savings'), w='sampling_weight')
#   mat <- diag(length(levels(testdata2$roof)))
#   rownames(mat) <- colnames(mat) <- levels(testdata2$roof)
#   mat[1,] <- c(0.9,0,0,0.05,0.05)
#   # we can also have a look at the transitions
#   gsm <- get.sdcMicroObj(sdc, "pram")$transitions
#   expect_warning(sdc <- pram(sdc, variables="roof", pd=mat))
#   
# })
# ################################################
# # data frame
# # data frame without missing
# set.seed(3)
# sample_Data <- runif(20, 1, 100)
# sample_Data <- array(sample_Data, dim = c(10,2))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
# sample_Data <- as.data.frame(sample_Data)
# colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
# sample_Data[,3] <- as.factor(as.numeric(sample_Data[,3]))
# sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
# sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
# sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
# 
# test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"))
#   expect_equal(fr[[7]], structure(c(1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1)
#   expect_equal(fr[[7]], structure(c(2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1","2"), class = "factor"))
# })
# 
# 
# test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"))
#   expect_equal(fr[[7]], structure(c(1L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor")
#   )
# })
# 
# test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"))
#   expect_equal(fr[[7]], structure(c(1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1)
#   expect_equal(fr[[7]], structure(c(2L, 1L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"))
#   expect_equal(fr[[7]], structure(c(1L, 2L, 1L, 1L, 1L, 2L, 2L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, 1L, 2L, 1L, 1L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# # data frame with missing
# set.seed(3)
# sample_Data <- runif(20, 1, 100)
# sample_Data <- array(sample_Data, dim = c(10,2))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
# sample_Data <- as.data.frame(sample_Data)
# colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
# sample_Data[,3] <- as.factor(as.numeric(sample_Data[,3]))
# sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
# sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
# sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
# sample_Data[c(1,3,4,9),1] <- NA
# sample_Data[c(3,10),2] <- NA
# sample_Data[c(3),3] <- NA
# sample_Data[c(1,3,6),4] <- NA
# sample_Data[c(2,3,7,10),5] <- NA
# sample_Data[c(3,8),6] <- NA
# 
# test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"))
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 1L, 2L, 1L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 2L, 1L, 1L, 1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=1, strata_variables=1, pd = different, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"))
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 1L, 2L, 1L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=1, strata_variables=2, pd = default, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# 
# test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 2L, 1L, 1L, 1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=1, strata_variables=2, pd = different, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# 
# test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"))
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 1L, 2L, 1L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=2, strata_variables=1, pd = default, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# 
# test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 2L, 1L, 1L, 1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=2, strata_variables=1, pd = different, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5"), pd = 0.1, alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"))
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 1L, 2L, 1L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=2, strata_variables=2, pd = default, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# 
# test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 2L, 1L, 1L, 1L, 1L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# test_that("data frame without missing, variables=2, strata_variables=2, pd = different, alpha = different)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var3", "Var4"), strata_variables=c("Var5", "Var6"), pd = 0.1, alpha = 0.1)
#   expect_equal(fr[[7]], structure(c(1L, 1L, NA, 1L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
# 
# 
# # data frame with more columns then rows with missing
# set.seed(3)
# sample_Data <- runif(20, 1, 100)
# sample_Data <- array(sample_Data, dim = c(4,5))
# sample_Data <- cbind(sample_Data, round(runif(4, 1, 2), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(4, 1, 4), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(4, 1, 6), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(4, 1, 3), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(4, 1, 5), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(4, 1, 2), digits = 0))
# sample_Data <- as.data.frame(sample_Data)
# colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7", "Var8", "Var9", "Var10", "Var11")
# sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
# sample_Data[,7] <- as.factor(as.numeric(sample_Data[,7]))
# sample_Data[,8] <- as.factor(as.numeric(sample_Data[,8]))
# sample_Data[,9] <- as.factor(as.numeric(sample_Data[,9]))
# sample_Data[,10] <- as.factor(as.numeric(sample_Data[,10]))
# sample_Data[,11] <- as.factor(as.numeric(sample_Data[,11]))
# sample_Data[c(1,3,4),6] <- NA
# sample_Data[c(2,3),7] <- NA
# sample_Data[c(3),8] <- NA
# sample_Data[c(1,3),9] <- NA
# sample_Data[c(2,3),10] <- NA
# sample_Data[c(3),11] <- NA
# 
# test_that("data frame without missing, variables=3, strata_variables=3, pd = default, alpha = default)", {
#   set.seed(123)
#   fr <- pram(sample_Data, variables = c("Var6", "Var7", "Var8"), strata_variables=c("Var9", "Var10", "Var11"))
#   expect_equal(fr[[12]], structure(c(NA, 1L, NA, NA), .Label = "1", class = "factor"))
#   expect_equal(fr[[13]], structure(c(1L, NA, NA, 3L), .Label = c("2", "3", "4"), class = "factor"))
#   expect_equal(fr[[14]], structure(c(2L, 3L, NA, 3L), .Label = c("3", "4", "5"), class = "factor"))
# })
# 
# 
# 
# # sdc
# # sdc without missing values
# set.seed(3)
# sample_Data <- runif(20, 1, 100)
# sample_Data <- array(sample_Data, dim = c(10,2))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 2), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 4), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 6), digits = 0))
# sample_Data <- cbind(sample_Data, round(runif(10, 1, 3), digits = 0))
# sample_Data <- as.data.frame(sample_Data)
# colnames(sample_Data) <- c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6")
# sample_Data[,3] <- as.factor(as.numeric(sample_Data[,3]))
# sample_Data[,4] <- as.factor(as.numeric(sample_Data[,4]))
# sample_Data[,5] <- as.factor(as.numeric(sample_Data[,5]))
# sample_Data[,6] <- as.factor(as.numeric(sample_Data[,6]))
# sample_Data <- createSdcObj(sample_Data, keyVars=c("Var1", "Var2"))
# 
# test_that("data frame without missing, variables=1, strata_variables=1, pd = default, alpha = default)", {
#   set.seed(123)
#   sample_Data <- pram(sample_Data, variables = c("Var3"), strata_variables=c("Var5"))
#   expect_equal(sample_Data@manipPramVars[,1], structure(c(1L, 1L, 2L, 2L, 1L, 2L, 2L, 2L, 2L, 2L), .Label = c("1", "2"), class = "factor"))
# })
# 
