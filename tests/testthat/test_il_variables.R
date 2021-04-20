test_that("IL_variable()", {
  set.seed(1)
  n <- 150
  x <- xm <- data.frame(
    v1 = factor(sample(letters[1:5], n, replace = TRUE), levels = letters[1:5]),
    v2 = rnorm(n),
    v3 = runif(3),
    v4 = ordered(sample(c("bad", "mediocre", "good"), n, replace = TRUE), levels = c("bad", "mediocre", "good"))
  )
  
  expect_equivalent(as.numeric(IL_variables(x, xm)), 0)
  
  xm$v1[1:5] <- "a"
  xm$v2 <- rnorm(n, mean = 5)
  xm$v4[1:5] <- "mediocre"
  
  res <- IL_variables(x, xm)
  expect_equivalent(as.numeric(res), 0.223034535696)
})
