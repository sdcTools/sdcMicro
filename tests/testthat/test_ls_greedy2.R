# TDD for ls_greedy2() (docs/04 section 2): per-violator EXACT cheapest repair
# against the current data (2^p patterns, safe-class mass precomputed once,
# violator partners recounted), applied cheapest-first, fully deterministic.
# Unlike sdcMicro's kAnon it never suppresses "the first differing key" -- each
# step is the record's true minimum-cost repair at that moment.

test_that("greedy2 finds the optimum on the 3x2 witness", {
  # r1 suppresses b -> (1,*) which passively repairs r2; r3 suppresses a.
  # Two suppressions -- the proven exact optimum, where sdcMicro-style
  # first-differing-key logic needs three.
  x <- data.frame(a = c("1", "1", "2"), b = c("1", "2", "2"),
                  stringsAsFactors = FALSE)
  res <- ls_greedy2(x, keyVars = c("a", "b"), k = 2)
  expect_equal(res$objective, 2)
  expect_equal(res$nsupp, 2L)
  expect_true(ls_check(res$xAnon, c("a", "b"), k = 2)$ok)
})

test_that("greedy2 is never worse than sdcMicro on the reference data", {
  x <- francdat
  res <- ls_greedy2(x, keyVars = c(4, 5, 6), k = 2)
  expect_true(ls_check(res$xAnon, c(4, 5, 6), k = 2)$ok)
  h <- kAnon(x, keyVars = c(4, 5, 6), k = 2)
  nh <- sum(is.na(h$xAnon)) - sum(is.na(x[, c(4, 5, 6)]))
  expect_lte(res$nsupp, nh)
})

test_that("greedy2 respects the exact optimum as a floor", {
  skip_if_not_installed("highs")
  set.seed(20260902 + 5)
  tested <- 0L
  for (rep in 1:15) {
    x <- as.data.frame(lapply(1:2, function(j)
      as.character(sample.int(3, 7, replace = TRUE))),
      stringsAsFactors = FALSE)
    names(x) <- c("a", "b")
    if (rep %% 4 == 0) x[sample.int(7, 1), sample.int(2, 1)] <- NA
    if (length(ls_check(x, names(x), 2)$violators) == 0L) next
    g <- ls_greedy2(x, keyVars = names(x), k = 2)
    expect_true(ls_check(g$xAnon, names(x), 2)$ok, info = paste("rep", rep))
    e <- try(ls_optimal(x, keyVars = names(x), k = 2), silent = TRUE)
    if (inherits(e, "try-error") || !identical(e$status, "optimal")) next
    tested <- tested + 1L
    expect_gte(g$objective, e$objective)
  }
  expect_gt(tested, 5L)
})

test_that("greedy2 handles big safe classes, genuine NAs and importance", {
  # one cheap suppression against a big class
  x1 <- data.frame(a = c(rep("1", 10), "1"), b = c(rep("1", 10), "9"),
                   stringsAsFactors = FALSE)
  r1 <- ls_greedy2(x1, keyVars = c("a", "b"), k = 3)
  expect_equal(r1$nsupp, 1L)
  expect_true(is.na(r1$xAnon[11, "b"]))
  # genuine NA is a free wildcard
  x2 <- data.frame(a = c(rep("2", 5), "1"), b = c(rep("2", 5), NA),
                   stringsAsFactors = FALSE)
  r2 <- ls_greedy2(x2, keyVars = c("a", "b"), k = 2)
  expect_equal(r2$nsupp, 1L)
  expect_true(is.na(r2$xAnon[6, "a"]))
  # importance steers the choice between two single-cell repairs
  x3 <- data.frame(a = c("1", "2", "1", "2", "1"),
                   b = c("1", "1", "2", "1", "2"), stringsAsFactors = FALSE)
  keep_a <- ls_greedy2(x3, keyVars = c("a", "b"), k = 2, importance = c(1, 2))
  expect_true(is.na(keep_a$xAnon[1, "b"]))
  expect_false(is.na(keep_a$xAnon[1, "a"]))
})

test_that("greedy2 is deterministic and works on testdata", {
  data("testdata", package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water")
  a <- ls_greedy2(testdata, keyVars = kv, k = 3)
  b <- ls_greedy2(testdata, keyVars = kv, k = 3)
  expect_identical(a$xAnon, b$xAnon)
  expect_true(ls_check(a$xAnon, kv, k = 3)$ok)
  h <- kAnon(testdata, keyVars = kv, k = 3)
  nh <- sum(is.na(h$xAnon[, kv])) - sum(is.na(testdata[, kv]))
  expect_lte(a$nsupp, nh)
})

test_that("cpp engine reproduces the R engine exactly", {
  # The C++ core must be a faithful port: same repairs, same tie-breaks,
  # same recount semantics -- pinned by strict equality on random
  # instances with genuine NAs.
  set.seed(42)
  for (rep in 1:6) {
    n <- sample(20:40, 1)
    p <- sample(2:4, 1)
    x <- as.data.frame(
      replicate(p, sample(c(as.character(1:3), NA), n, TRUE,
                          prob = c(0.4, 0.3, 0.2, 0.1)),
                simplify = FALSE), stringsAsFactors = FALSE)
    names(x) <- paste0("K", seq_len(p))
    for (k in 2:3) {
      rR <- ls_greedy2(x, names(x), k = k, engine = "R")
      rC <- ls_greedy2(x, names(x), k = k, engine = "cpp")
      expect_identical(rC$xAnon, rR$xAnon)
      expect_equal(rC$objective, rR$objective)
      expect_equal(rC$steps, rR$steps)
    }
  }
})
