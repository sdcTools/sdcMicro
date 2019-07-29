test_that("valtable data frame",{
  data("Tarragona")
  vT <- valTable(Tarragona[100:200,],
    method=c("simple","onedims","pca"))#,"addNoise: additive"))
  expect_true(is.data.frame(vT))
})

test_that("valtable more methods",{
  data("Tarragona")
  vT <- valTable(Tarragona[100:200,],
    method=c("simple","onedims","pca","clustpppca"))##,"mdav"))##"swappNum"))# "addNoise: additive", ))
  expect_true(is.data.frame(vT))
})
