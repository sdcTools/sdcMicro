test_that("ai_utility_score returns 0 for unmodified data", {
  data(testdata2, package = "sdcMicro")
  sdc <- createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = c("expend", "income", "savings"),
    w = "sampling_weight")
  score <- ai_utility_score(sdc, sdc)
  expect_equal(score$total, 0)
  expect_equal(score$suppression_rate, 0)
  expect_equal(score$category_loss, 0)
})

test_that("ai_utility_score increases after localSuppression", {
  data(testdata2, package = "sdcMicro")
  sdc_orig <- createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = c("expend", "income", "savings"),
    w = "sampling_weight")
  sdc_anon <- localSuppression(sdc_orig, k = 3)
  score <- ai_utility_score(sdc_orig, sdc_anon)
  expect_gt(score$suppression_rate, 0)
  expect_equal(score$category_loss, 0)
  expect_gt(score$total, 0)
})

test_that("ai_utility_score increases after groupAndRename", {
  data(testdata2, package = "sdcMicro")
  testdata2$roof <- as.factor(testdata2$roof)
  sdc_orig <- createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = c("expend", "income", "savings"),
    w = "sampling_weight")
  sdc_anon <- groupAndRename(sdc_orig, var = "roof",
    before = c("2", "4", "5", "6", "9"),
    after  = c("2", "4", "5", "5", "9"))
  score <- ai_utility_score(sdc_orig, sdc_anon)
  expect_gt(score$category_loss, 0)
  expect_equal(score$suppression_rate, 0)
})

test_that("ai_utility_score respects custom weights", {
  data(testdata2, package = "sdcMicro")
  sdc_orig <- createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    numVars = c("expend", "income", "savings"),
    w = "sampling_weight")
  sdc_anon <- localSuppression(sdc_orig, k = 3)
  score_default <- ai_utility_score(sdc_orig, sdc_anon)
  score_supp_only <- ai_utility_score(sdc_orig, sdc_anon, weights = c(1, 0, 0))
  expect_equal(score_supp_only$total, score_supp_only$suppression_rate)
})

test_that("ai_utility_score works without numVars", {
  data(testdata2, package = "sdcMicro")
  sdc_orig <- createSdcObj(testdata2,
    keyVars = c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex"),
    w = "sampling_weight")
  sdc_anon <- localSuppression(sdc_orig, k = 3)
  score <- ai_utility_score(sdc_orig, sdc_anon)
  expect_equal(score$il1, 0)
  expect_gt(score$total, 0)
})
