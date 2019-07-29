test_that("suda2 data frame",{
  data("Tarragona")
  valTable(Tarragona[100:200,],
    method=c("simple","onedims","pca","addNoise: additive"))
})

test_that("valtable more methods",{
  data("Tarragona")
  valTable(Tarragona[100:200,],
    method=c("simple","onedims","pca","clustpppca","mdav", "addNoise: additive", "swappNum"))


})