context("valTable")
test_that("valTable data.frame",{
  data("Tarragona")
  vT <- valTable(Tarragona[100:200,],
    method=c("simple","onedims","pca"))#,"addNoise: additive"))
  expect_true(is.data.frame(vT))
})

test_that("valtable more methods",{
  data("Tarragona")
  expect_warning(vT <- valTable(Tarragona[100:200,],
    method=c("simple","onedims","pca","clustpppca")))##,"mdav"))##"swappNum"))# "addNoise: additive", ))
  expect_true(is.data.frame(vT))
})
