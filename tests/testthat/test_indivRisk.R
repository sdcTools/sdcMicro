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


# non-integer frequency counts (freqCalc() with 0 < alpha < 1 and missing values)
frac_Data <- data.frame(a = c(1, 1, 2, 2, 2, 2), b = c(1, NA, 1, 1, 1, 1),
                        w = c(50, 50, 20, 20, 20, 20))
bf1 <- function(p) (p / (1 - p)) * log(1 / p)
bf2 <- function(p) (p / (1 - p)) - ((p / (1 - p))^2) * log(1 / p)

test_that("risk for 1 < fk < 2 is positive and lies between the risks for fk = 1 and fk = 2", {
  fr <- freqCalc(frac_Data, keyVars = 1:2, w = 3, alpha = 0.5)
  expect_equal(fr$fk, c(1.5, 2, 4, 4, 4, 4))
  expect_equal(fr$Fk, c(75, 100, 80, 80, 80, 80))
  ind <- indivRisk(fr)
  # E(1/F | f) with F - f negative binomial (size f = 1.5, prob p = 0.02)
  expect_equal(ind$rk[1], 0.0324845523487, tolerance = 1e-9)
  expect_gt(ind$rk[1], bf2(0.02))
  expect_lt(ind$rk[1], bf1(0.02))
  # records with an integer fk are not touched
  expect_identical(ind$rk[2], bf2(0.02))
  expect_identical(ind$rk[3:6], rep(0.05 / (4 - (1 - 0.05)), 4))
  expect_equal(indivRisk(fr, qual = 2)$rk, 2 * ind$rk)
})

test_that("the integral form agrees with the closed forms for fk = 1 and fk = 2", {
  p <- c(1e-9, 1e-6, 1e-4, 0.001, 0.02, 0.1, 0.3, 0.45)
  expect_equal(sdcMicro:::bfRiskFrac(rep(1, 8), p), bf1(p), tolerance = 1e-9)
  expect_equal(sdcMicro:::bfRiskFrac(rep(2, 8), p), bf2(p), tolerance = 1e-9)
  # continuous in fk, also for very small sampling fractions
  expect_equal(sdcMicro:::bfRiskFrac(rep(2 - 1e-12, 8), p), bf2(p), tolerance = 1e-9)
  # risk falls as fk rises and never exceeds 1 / fk
  f <- c(1.01, 1.25, 1.5, 1.75, 1.99, 2.5, 7.3, 40.5, 1000.5)
  for (pp in c(1e-9, 1e-4, 0.02, 0.5, 0.9)) {
    r <- sdcMicro:::bfRiskFrac(f, rep(pp, length(f)))
    expect_true(all(diff(r) < 0))
    expect_true(all(r <= 1 / f & r >= pp / f))
  }
})

test_that("method = exact gives a finite risk for non-integer fk", {
  d <- data.frame(a = c(1, 1, 1, 1, 1, 1, 2, 2, 2), b = c(1, 1, 1, 1, NA, NA, 1, 1, NA),
                  w = c(10, 20, 30, 40, 25, 25, 15, 35, 50))
  fr <- freqCalc(d, keyVars = 1:2, w = 3, alpha = 0.25)
  expect_equal(fr$fk, c(4.5, 4.5, 4.5, 4.5, 6, 6, 2.25, 2.25, 3))
  ex <- indivRisk(fr, method = "exact")$rk
  ap <- indivRisk(fr, method = "approx")$rk
  expect_true(all(is.finite(ex) & ex > 0))
  nonint <- fr$fk != round(fr$fk)
  expect_equal(ex[nonint], sdcMicro:::bfRiskFrac(fr$fk, fr$fk / fr$Fk)[nonint], tolerance = 1e-12)
  # integer counts keep the closed form, which the integral reproduces
  expect_equal(ex[!nonint], sdcMicro:::bfRiskFrac(fr$fk, fr$fk / fr$Fk)[!nonint], tolerance = 1e-8)
  # the approximation for fk > 2 is close to the exact value and above it
  expect_equal(ap, ex, tolerance = 0.05)
  expect_true(all(ap > ex))
})

test_that("measure_risk() for a data.frame accepts alpha", {
  fr <- freqCalc(frac_Data, keyVars = 1:2, w = 3, alpha = 0.5)
  mr <- measure_risk(frac_Data, keyVars = c("a", "b"), w = "w", alpha = 0.5)
  expect_equal(unname(mr$Res[, "risk"]), indivRisk(fr)$rk)
  expect_equal(unname(mr$Res[, "fk"]), fr$fk)
  mr1 <- measure_risk(frac_Data, keyVars = c("a", "b"), w = "w")
  expect_equal(unname(mr1$Res[, "fk"]), freqCalc(frac_Data, keyVars = 1:2, w = 3)$fk)
})
