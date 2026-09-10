# Smoke test only (placeholder until the first solver lands): the sdcMicro
# heuristic is the baseline every ls_* engine is compared against, so the
# test suite must be able to run it.
test_that("kAnon() baseline runs on francdat", {
  ls <- kAnon(francdat, keyVars = c(4, 5, 6), k = 2)
  expect_s3_class(ls, "localSuppression")
  # the data.frame method returns only the key columns in $xAnon (named)
  ff <- freqCalc(ls$xAnon, keyVars = ls$keyVars)
  expect_true(all(ff$fk >= 2))
})
