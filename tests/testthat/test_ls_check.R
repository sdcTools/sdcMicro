# TDD for the protection-target checker. ls_check() is the referee for every
# solver in the package: no solver result is ever trusted on its own counts.
# The wildcard path must BE freqCalc(), so agreement with sdcMicro is
# by construction, not by coincidence. Plan: docs/07-paper-P1-EJOR-plan.md A1.

test_that("wildcard fk agrees with freqCalc by construction", {
  x <- francdat
  kv <- c(4, 5, 6)
  chk <- ls_check(x, keyVars = kv, k = 2, semantics = "wildcard")
  ff <- freqCalc(x, keyVars = kv)
  expect_equal(chk$fk, as.numeric(ff$fk))
})

test_that("ls_check flags the violators of un-anonymised data", {
  chk <- ls_check(francdat, keyVars = c(4, 5, 6), k = 2)
  expect_false(chk$ok)
  expect_gt(length(chk$violators), 0)
  # violators are exactly the records below k
  expect_setequal(chk$violators, which(chk$fk < 2))
})

test_that("ls_check accepts the output of kAnon", {
  ls <- kAnon(francdat, keyVars = c(4, 5, 6), k = 2)
  chk <- ls_check(ls$xAnon, keyVars = ls$keyVars, k = 2, semantics = "wildcard")
  expect_true(chk$ok)
  expect_length(chk$violators, 0)
})

test_that("identical-tuple semantics is stricter than wildcard semantics", {
  # The S1/S2 separation the paper rests on, in its smallest form. Released
  # tuples (1, *) and (*, 1) are mutually compatible -- each NA matches the
  # other record's value -- so both records have fk = 2 under S1 and the file
  # is 2-anonymous in sdcMicro's sense. But the two released tuples are not
  # identical, so under S2 each forms a class of size 1. Two records, k = 2,
  # and the two criteria already disagree; this is why an S1 optimum can be
  # strictly cheaper than an S2 optimum. (Witness for plan section 2.2 / T1
  # item 5.)
  x <- data.frame(a = c("1", NA), b = c(NA, "1"), stringsAsFactors = FALSE)

  s1 <- ls_check(x, keyVars = c("a", "b"), k = 2, semantics = "wildcard")
  s2 <- ls_check(x, keyVars = c("a", "b"), k = 2, semantics = "identical")

  expect_equal(s1$fk, c(2, 2))
  expect_true(s1$ok)
  expect_equal(s2$fk, c(1, 1))
  expect_false(s2$ok)
})

test_that("ls_check counts identical tuples with NA as a value", {
  x <- data.frame(a = c("1", "1", "2"), b = c(NA, NA, "9"),
                  stringsAsFactors = FALSE)
  chk <- ls_check(x, keyVars = c("a", "b"), k = 2, semantics = "identical")
  # rows 1 and 2 share the released tuple (1, NA); row 3 is alone
  expect_equal(chk$fk, c(2, 2, 1))
  expect_equal(chk$violators, 3L)
})

test_that("wildcard path survives non-numeric character labels", {
  # Regression: freqCalc() coerces keys with as.numeric(), so labels like
  # "male" silently became NA (= wildcards) and every record looked safe.
  # fk_wildcard() now recodes injectively to integer codes first.
  x <- data.frame(K1 = c("male", "male", "female"),
                  K2 = c("blue", "blue", "blue"))
  chk <- ls_check(x, c("K1", "K2"), k = 2)
  expect_equal(chk$fk, c(2, 2, 1))
  expect_identical(chk$violators, 3L)

  # identical file after injective recoding must give identical fk
  xn <- data.frame(K1 = c(1, 1, 2), K2 = c(1, 1, 1))
  expect_equal(ls_check(xn, c("K1", "K2"), k = 2)$fk, chk$fk)
})

# --- alpha: the deployed criterion is a one-parameter family -----------------
# freqCalc()'s alpha weights how much a record carrying NAs contributes to
# other records' counts: alpha = 1 is full wildcard matching (S1), alpha = 0
# lets missing values form their own category. The 2026-09-04 review gate
# showed this prices the hub construction directly, so ls_check() must be able
# to verify a released file at the alpha an agency actually uses.

test_that("ls_check reproduces freqCalc for a given alpha", {
  skip_if_not_installed("sdcMicro")
  x <- data.frame(a = c("1", "1", "2", NA), b = c("1", "2", "2", NA),
                  stringsAsFactors = FALSE)
  for (al in c(1, 0.5, 0)) {
    xi <- x
    for (v in names(xi)) xi[[v]] <- as.integer(factor(xi[[v]]))
    expect_equal(ls_check(x, c("a", "b"), k = 2, alpha = al)$fk,
                 freqCalc(xi, keyVars = c("a", "b"), alpha = al)$fk)
  }
})

test_that("alpha < 1 withdraws the credit a hub supplies", {
  # one hub reduced to its first key serves three records at alpha = 1 and
  # stops serving them below it.
  x <- data.frame(a = rep("1", 4L), b = c("1", "2", "3", "4"),
                  stringsAsFactors = FALSE)
  h <- ls_hub(x, keyVars = c("a", "b"), k = 2)
  expect_true(ls_check(h$xAnon, c("a", "b"), k = 2, alpha = 1)$ok)
  expect_false(ls_check(h$xAnon, c("a", "b"), k = 2, alpha = 0.5)$ok)
})

test_that("alpha is refused where the models are not exact for it", {
  x <- data.frame(a = c("1", "1", "2"), b = c("1", "2", "2"),
                  stringsAsFactors = FALSE)
  expect_error(ls_optimal(x, c("a", "b"), k = 2, alpha = 0.5), "alpha")
})
