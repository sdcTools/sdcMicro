# test <- data.frame(a=1:10000,b=1:10000)
# test_that("rankSwap P=0", {
#   swapped <-rankSwap(test, var = c("a"), P = 0,TopPercent=0, BottomPercent=0,R0=NULL)
#   expect_true(all(test$a==swapped$a))
# })
# 
# test_that("rankSwap R0>0", {
#   swapped <-rankSwap(test, var = c("a"), P = NULL,TopPercent=0, BottomPercent=0,R0=1)
#   expect_true(all(test$a==swapped$a))
# })
# 
# test_that("rankSwap K0>0", {
#   swapped <-rankSwap(test, var = c("a"), P = NULL,TopPercent=0, BottomPercent=0,R0=NULL,K0=0)
#   expect_true(all(test$a==swapped$a))
# })
