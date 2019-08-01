library(testthat)

# without missing
# weighted
sample_Data <- array(c(2, 3, 2, 2, 3, 3, 1, 2, 3, 3, 3, 3, 3, 3, 4, 3, 1, 3, 1, 2, 1, 1, 1, 1), dim = c(8,3))
colnames(sample_Data) <- c("A", "B", "C")
sample_Data <- cbind(sample_Data, "weight" = c(18.0, 45.5, 39.0, 17.0, 92.0, 8.0, 123.5, 51))
sample_Data <- as.data.frame(sample_Data)
fr <- freqCalc(sample_Data, keyVars = c(1,2,3), w = 4)

# method = approx
test_that("data frame with weight, method = approx, qual = default, survey = TRUE", {
  ind <- indivRisk(fr, method = "approx", qual = 1, survey = TRUE)
  expect_equal(ind$rk,c(0.01369863, 0.08579129, 0.01369863, 0.17707583, 0.01877883, 0.01877883, 0.03931625, 0.01369863))
})

test_that("data frame with weight, method = approx, qual = default, survey = FALSE", {
  ind <- indivRisk(fr, method = "approx", qual = 1, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.0000000, 0.33333, 1.0000000, 0.5000000, 0.5000000, 1.0000000, 0.33333))
})

test_that("data frame with weight, method = approx, qual = different, survey = TRUE", {
  ind <- indivRisk(fr, method = "approx", qual = 2, survey = TRUE)
  expect_equal(ind$rk,c(0.02739726, 0.17158258, 0.02739726, 0.35415167, 0.03755767, 0.03755767, 0.07863251, 0.02739726))
})

test_that("data frame with weight, method = approx, qual = different, survey = FALSE", {
  ind <- indivRisk(fr, method = "approx", qual = 2, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.0000000, 0.33333, 1.0000000, 0.5000000, 0.5000000, 1.0000000, 0.33333))
})

# method = exact
test_that("data frame with weight, method = exact, qual = default, survey = TRUE", {
  ind <- indivRisk(fr, method = "exact", qual = 1, survey = TRUE)
  expect_equal(ind$rk,c(0.01355297, 0.08579129, 0.01355297, 0.17707583, 0.01877883, 0.01877883, 0.03931625, 0.01355297))
})

test_that("data frame with weight, method = exact, qual = default, survey = FALSE", {
  ind <- indivRisk(fr, method = "exact", qual = 1, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.0000000, 0.33333, 1.0000000, 0.5000000, 0.5000000, 1.0000000, 0.33333))
})

test_that("data frame with weight, method = exact, qual = different, survey = TRUE", {
  ind <- indivRisk(fr, method = "exact", qual = 2, survey = TRUE)
  expect_equal(ind$rk,c(0.02710594, 0.17158258, 0.02710594, 0.35415167, 0.03755767, 0.03755767, 0.07863251, 0.02710594))
})

test_that("data frame with weight, method = exact, qual = different, survey = FALSE", {
  ind <- indivRisk(fr, method = "exact", qual = 2, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.0000000, 0.33333, 1.0000000, 0.5000000, 0.5000000, 1.0000000, 0.33333))
})





# non-weighted
sample_Data <- array(c(2, 3, 2, 2, 3, 3, 1, 2, 3, 3, 3, 3, 3, 3, 4, 3, 1, 3, 1, 2, 1, 1, 1, 1), dim = c(8,3))
colnames(sample_Data) <- c("A", "B", "C")
sample_Data <- cbind(sample_Data, "weight" = c(18.0, 45.5, 39.0, 17.0, 92.0, 8.0, 123.5, 51))
sample_Data <- as.data.frame(sample_Data)
fr <- freqCalc(sample_Data, keyVars = c(1,2,3))

# method = approx
test_that("data frame, method = approx, qual = default, survey = TRUE", {
  ind <- indivRisk(fr, method = "approx", qual = 1, survey = TRUE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.0000000, 0.33333, 1.0000000, 0.5000000, 0.5000000, 1.0000000, 0.33333))
})

test_that("data frame, method = approx, qual = default, survey = FALSE", {
  ind <- indivRisk(fr, method = "approx", qual = 1, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.0000000, 0.33333, 1.0000000, 0.5000000, 0.5000000, 1.0000000, 0.33333))
})

test_that("data frame, method = approx, qual = different, survey = TRUE", {
  ind <- indivRisk(fr, method = "approx", qual = 2, survey = TRUE)
  expect_equal(round(ind$rk, digits = 5), c(0.66667, 2.00000, 0.66667, 2.00000, 1.00000, 1.00000, 2.00000, 0.66667))
})

test_that("data frame, method = approx, qual = different, survey = FALSE", {
  ind <- indivRisk(fr, method = "approx", qual = 2, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.0000000, 0.33333, 1.0000000, 0.5000000, 0.5000000, 1.0000000, 0.33333))
})

# method = exact
test_that("data frame, method = exact, qual = default, survey = TRUE", {
  ind <- indivRisk(fr, method = "exact", qual = 1, survey = TRUE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.0000000, 0.33333, 1.0000000, 0.5000000, 0.5000000, 1.0000000, 0.33333))
})

test_that("data frame, method = exact, qual = default, survey = FALSE", {
  ind <- indivRisk(fr, method = "exact", qual = 1, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.0000000, 0.33333, 1.0000000, 0.5000000, 0.5000000, 1.0000000, 0.33333))
})

test_that("data frame, method = exact, qual = different, survey = TRUE", {
  ind <- indivRisk(fr, method = "exact", qual = 2, survey = TRUE)
  expect_equal(round(ind$rk, digits = 5), c(0.66667, 2.00000, 0.66667, 2.00000, 1.00000, 1.00000, 2.00000, 0.66667))
})

test_that("data frame, method = exact, qual = different, survey = FALSE", {
  ind <- indivRisk(fr, method = "exact", qual = 2, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.0000000, 0.33333, 1.0000000, 0.5000000, 0.5000000, 1.0000000, 0.33333))
})


# with missing
# weighted
sample_Data <- array(c(2, 3, 2, 2, 3, 3, 1, NA, 3, 3, 3, 3, 3, 3, 4, 3, 1, 3, NA, 2, 1, 1, NA, 1), dim = c(8,3))
colnames(sample_Data) <- c("A", "B", "C")
sample_Data <- cbind(sample_Data, "weight" = c(18.0, 45.5, 39.0, 17.0, 92.0, 8.0, 123.5, 51))
sample_Data <- as.data.frame(sample_Data)
fr <- freqCalc(sample_Data, keyVars = c(1,2,3), w = 4)

# method = approx
test_that("data frame with weight, method = approx, qual = default, survey = TRUE", {
  ind <- indivRisk(fr, method = "approx", qual = 1, survey = TRUE)
  expect_equal(ind$rk,c(0.013698630, 0.085791288, 0.010554090, 0.032466112, 0.009836066, 0.009836066, 0.039316254, 0.005973716))
})

test_that("data frame with weight, method = approx, qual = default, survey = FALSE", {
  ind <- indivRisk(fr, method = "approx", qual = 1, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.00000, 0.25000, 0.50000, 0.33333, 0.33333, 1.00000, 0.20000))
})

test_that("data frame with weight, method = approx, qual = different, survey = TRUE", {
  ind <- indivRisk(fr, method = "approx", qual = 2, survey = TRUE)
  expect_equal(ind$rk,c(0.02739726, 0.17158258, 0.02110818, 0.06493222, 0.01967213, 0.01967213, 0.07863251, 0.01194743))
})

test_that("data frame with weight, method = approx, qual = different, survey = FALSE", {
  ind <- indivRisk(fr, method = "approx", qual = 2, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.00000, 0.25000, 0.50000, 0.33333, 0.33333, 1.00000, 0.20000))
})

# method = exact
test_that("data frame with weight, method = exact, qual = default, survey = TRUE", {
  ind <- indivRisk(fr, method = "exact", qual = 1, survey = TRUE)
  expect_equal(ind$rk,c(0.013552968, 0.085791288, 0.010504889, 0.032466112, 0.009756889, 0.009756889, 0.039316254, 0.005962551))
})

test_that("data frame with weight, method = exact, qual = default, survey = FALSE", {
  ind <- indivRisk(fr, method = "exact", qual = 1, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.00000, 0.25000, 0.50000, 0.33333, 0.33333, 1.00000, 0.20000))
})

test_that("data frame with weight, method = exact, qual = different, survey = TRUE", {
  ind <- indivRisk(fr, method = "exact", qual = 2, survey = TRUE)
  expect_equal(ind$rk,c(0.02710594, 0.17158258, 0.02100978, 0.06493222, 0.01951378, 0.01951378, 0.07863251, 0.01192510))
})

test_that("data frame with weight, method = exact, qual = different, survey = FALSE", {
  ind <- indivRisk(fr, method = "exact", qual = 2, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.00000, 0.25000, 0.50000, 0.33333, 0.33333, 1.00000, 0.20000))
})





# non-weighted
sample_Data <- array(c(2, 3, 2, 2, 3, 3, 1, NA, 3, 3, 3, 3, 3, 3, 4, 3, 1, 3, NA, 2, 1, 1, NA, 1), dim = c(8,3))
colnames(sample_Data) <- c("A", "B", "C")
sample_Data <- cbind(sample_Data, "weight" = c(18.0, 45.5, 39.0, 17.0, 92.0, 8.0, 123.5, 51))
sample_Data <- as.data.frame(sample_Data)
fr <- freqCalc(sample_Data, keyVars = c(1,2,3))

# method = approx
test_that("data frame, method = approx, qual = default, survey = TRUE", {
  ind <- indivRisk(fr, method = "approx", qual = 1, survey = TRUE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.00000, 0.25000, 0.50000, 0.33333, 0.33333, 1.00000, 0.20000))
})

test_that("data frame, method = approx, qual = default, survey = FALSE", {
  ind <- indivRisk(fr, method = "approx", qual = 1, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.00000, 0.25000, 0.50000, 0.33333, 0.33333, 1.00000, 0.20000))
})

test_that("data frame, method = approx, qual = different, survey = TRUE", {
  ind <- indivRisk(fr, method = "approx", qual = 2, survey = TRUE)
  expect_equal(round(ind$rk, digits = 5), c(0.66667, 2.00000, 0.50000, 1.00000, 0.66667, 0.66667, 2.00000, 0.40000))
})

test_that("data frame, method = approx, qual = different, survey = FALSE", {
  ind <- indivRisk(fr, method = "approx", qual = 2, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.00000, 0.25000, 0.50000, 0.33333, 0.33333, 1.00000, 0.20000))
})

# method = exact
test_that("data frame, method = exact, qual = default, survey = TRUE", {
  ind <- indivRisk(fr, method = "exact", qual = 1, survey = TRUE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.00000, 0.25000, 0.50000, 0.33333, 0.33333, 1.00000, 0.20000))
})

test_that("data frame, method = exact, qual = default, survey = FALSE", {
  ind <- indivRisk(fr, method = "exact", qual = 1, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.00000, 0.25000, 0.50000, 0.33333, 0.33333, 1.00000, 0.20000))
})

test_that("data frame, method = exact, qual = different, survey = TRUE", {
  ind <- indivRisk(fr, method = "exact", qual = 2, survey = TRUE)
  expect_equal(round(ind$rk, digits = 5), c(0.66667, 2.00000, 0.50000, 1.00000, 0.66667, 0.66667, 2.00000, 0.40000))
})

test_that("data frame, method = exact, qual = different, survey = FALSE", {
  ind <- indivRisk(fr, method = "exact", qual = 2, survey = FALSE)
  expect_equal(round(ind$rk, digits = 5), c(0.33333, 1.00000, 0.25000, 0.50000, 0.33333, 0.33333, 1.00000, 0.20000))
})
