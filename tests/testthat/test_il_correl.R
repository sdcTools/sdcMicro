test_that("IL_correl()", {
  set.seed(1)
  data("Tarragona", package = "sdcMicro")
  res1 <- addNoise(obj = Tarragona, variables = colnames(Tarragona), noise = 100)
  
  expect_equivalent(as.numeric(IL_correl(x = as.data.frame(res1$x), xm = as.data.frame(res1$x))), 0)

  erg1 <- IL_correl(x = as.data.frame(res1$x), xm = as.data.frame(res1$xm))
  expect_equivalent(as.numeric(erg1), 0.462347410776)
  

  res2 <- addNoise(obj = Tarragona, variables = colnames(Tarragona), noise = 25) 
  erg2 <- IL_correl(x = as.data.frame(res2$x), xm = as.data.frame(res2$xm))
  expect_equivalent(as.numeric(erg2), 0.229297771144)
})
