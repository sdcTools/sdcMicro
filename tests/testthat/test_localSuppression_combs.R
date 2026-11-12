# localSuppression()/kAnon() with 'combs' on a data.frame: the result must not
# depend on where the key variables are in the data.

library(sdcMicro)

kv <- c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex")

combs_data <- function() {
  data("testdata", package = "sdcMicro")
  testdata$ageG <- cut(testdata$age, 5, labels = paste0("AG", 1:5))
  testdata[, c(kv, "ageG", "income")]
}

# smallest frequency count (sdcMicro's own counter) over all subsets of 'm' key
# variables, within the strata if given
min_fk <- function(xa, m, strata = NULL) {
  parts <- if (is.null(strata)) list(xa) else split(xa, strata)
  cmb <- utils::combn(kv, m, simplify = FALSE)
  min(vapply(parts, function(p) {
    min(vapply(cmb, function(v) min(freqCalc(p, keyVars = v)$fk), numeric(1)))
  }, numeric(1)))
}

test_that("kAnon() with strata and combs does not depend on the column positions", {
  skip_on_cran()
  x <- combs_data()
  first <- kAnon(x, keyVars = kv, strataVars = "ageG", k = 3, combs = 6)
  last <- kAnon(x[, c("income", "ageG", kv)], keyVars = kv, strataVars = "ageG", k = 3, combs = 6)
  mixed <- kAnon(x[, c(kv[1:3], "ageG", kv[4:7], "income")], keyVars = kv, strataVars = "ageG",
                 k = 3, combs = 6)
  expect_identical(last$xAnon[, kv], first$xAnon[, kv])
  expect_identical(mixed$xAnon[, kv], first$xAnon[, kv])
  expect_equal(last$supps, first$supps)
  # the promised 3-anonymity holds for every subset of six key variables in every stratum
  expect_gte(min_fk(first$xAnon[, kv], 6, x$ageG), 3)
  expect_gte(min_fk(last$xAnon[, kv], 6, x$ageG), 3)
  # key variables given as indices
  idx <- kAnon(x[, c("income", "ageG", kv)], keyVars = 3:9, strataVars = 2, k = 3, combs = 6)
  expect_identical(idx$xAnon[, kv], first$xAnon[, kv])
})

test_that("kAnon() with combs (no strata) does not depend on the column positions", {
  skip_on_cran()
  x <- combs_data()
  imp <- c(7, 1, 2, 3, 4, 5, 6)
  first <- kAnon(x, keyVars = kv, k = c(3, 2), combs = c(6, 5), importance = imp)
  last <- kAnon(x[, c("income", "ageG", kv)], keyVars = kv, k = c(3, 2), combs = c(6, 5),
                importance = imp)
  expect_identical(last$xAnon[, kv], first$xAnon[, kv])
  expect_gte(min_fk(first$xAnon[, kv], 6), 3)
  expect_gte(min_fk(last$xAnon[, kv], 6), 3)
  # the importance is used: the most important variable is suppressed least
  default <- kAnon(x[, c("income", "ageG", kv)], keyVars = kv, k = c(3, 2), combs = c(6, 5))
  expect_identical(default$xAnon[, kv], kAnon(x, keyVars = kv, k = c(3, 2), combs = c(6, 5))$xAnon[, kv])
})

test_that("the sdcMicroObj method with combs gives the data.frame result", {
  skip_on_cran()
  x <- combs_data()
  sdc <- createSdcObj(x[, c("income", "ageG", kv)], keyVars = kv, numVars = "income")
  sdc <- kAnon(sdc, k = 3, combs = 6)
  ref <- kAnon(x, keyVars = kv, k = 3, combs = 6)
  expect_equal(get.sdcMicroObj(sdc, "manipKeyVars"), ref$xAnon[, kv], ignore_attr = TRUE)
})
