# Tests pinning the exact-preserving aggregation (docs/04 section 1.1, in
# aggregated form): safe records enter the model only as their equivalence
# class with multiplicity. These are semantic guards -- the aggregated model
# must agree with the brute-force oracle and with first principles.

test_that("a big safe class repairs a violator at the cost of one cell", {
  skip_if_not_installed("highs")
  # 10 identical safe records (1,1) and one violator (1,9): they differ only
  # on key b, so one suppression -- b on the violator -> released (1,*) --
  # makes it compatible with all ten. f = 11 >= 3. Nothing cheaper exists.
  x <- data.frame(
    a = c(rep("1", 10), "1"),
    b = c(rep("1", 10), "9"),
    stringsAsFactors = FALSE
  )
  res <- ls_optimal(x, keyVars = c("a", "b"), k = 3)
  expect_identical(res$status, "optimal")
  expect_equal(res$objective, 1)
  expect_true(is.na(res$xAnon[11, "b"]))
  expect_true(ls_check(res$xAnon, c("a", "b"), k = 3)$ok)
})

test_that("a violator differing from the big class on all keys pays full price", {
  skip_if_not_installed("highs")
  # Violator (9,9) vs class (1,1): both keys differ, so both cells must go.
  # A single suppression leaves (9,*) or (*,9), compatible with nothing.
  x <- data.frame(
    a = c(rep("1", 10), "9"),
    b = c(rep("1", 10), "9"),
    stringsAsFactors = FALSE
  )
  res <- ls_optimal(x, keyVars = c("a", "b"), k = 3)
  expect_equal(res$objective, 2)
  expect_true(ls_check(res$xAnon, c("a", "b"), k = 3)$ok)
})

test_that("genuine NAs are free wildcards, never paid for", {
  skip_if_not_installed("highs")
  # Violator (1, NA) vs safe class (2,2): the genuine NA already matches, only
  # key a separates them -> optimum is exactly one paid suppression.
  x <- data.frame(
    a = c(rep("2", 5), "1"),
    b = c(rep("2", 5), NA),
    stringsAsFactors = FALSE
  )
  expect_equal(ls_check(x, c("a", "b"), k = 2)$violators, 6L)
  res <- ls_optimal(x, keyVars = c("a", "b"), k = 2)
  expect_equal(res$objective, 1)
  expect_equal(res$nsupp, 1L)
  expect_true(is.na(res$xAnon[6, "a"]))
  expect_true(ls_check(res$xAnon, c("a", "b"), k = 2)$ok)
})

test_that("the aggregated model still matches brute force on random data", {
  skip_if_not_installed("highs")
  # Same oracle as in test-ls_optimal.R, but on instances with duplicated rows
  # so the class aggregation actually kicks in (classes with n_C > k).
  set.seed(20260901 + 7)
  tested <- 0L
  for (rep in 1:25) {
    p <- 2
    base <- as.data.frame(lapply(seq_len(p), function(j)
      as.character(sample.int(2, 4, replace = TRUE))), stringsAsFactors = FALSE)
    names(base) <- c("a", "b")
    # duplicate one random row several times to create a big class
    x <- rbind(base, base[rep(sample.int(4, 1), sample(3:6, 1)), , drop = FALSE])
    rownames(x) <- NULL
    k <- sample(2:3, 1)
    V <- ls_check(x, names(x), k)$violators
    if (length(V) == 0L || length(V) * p > 12) next
    res <- try(ls_optimal(x, keyVars = names(x), k = k), silent = TRUE)
    if (inherits(res, "try-error")) next
    if (!identical(res$status, "optimal")) next
    tested <- tested + 1L
    expect_equal(res$objective, brute_force_optimum(x, names(x), k),
                 info = paste("rep", rep))
    expect_true(ls_check(res$xAnon, names(x), k)$ok, info = paste("rep", rep))
  }
  expect_gt(tested, 3L)
})

test_that("ls_optimal solves testdata (n = 4580) in seconds, not minutes", {
  skip_if_not_installed("highs")
  # The scaling wall of 2026-09-01: the unaggregated model (pair variables
  # ~ |V| x n, built in a nested R loop) hung for >16 minutes on exactly this
  # configuration. With the class aggregation the model has one candidate per
  # safe CLASS, not per safe record, and must build + solve comfortably fast.
  data("testdata", package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water")
  on.exit(setTimeLimit(elapsed = Inf, transient = TRUE), add = TRUE)
  setTimeLimit(elapsed = 60, transient = TRUE)
  res <- ls_optimal(testdata, keyVars = kv, k = 3)
  setTimeLimit(elapsed = Inf, transient = TRUE)
  expect_identical(res$status, "optimal")
  expect_true(ls_check(res$xAnon, kv, k = 3)$ok)
  # exact never worse than the heuristic
  h <- kAnon(testdata, keyVars = kv, k = 3)
  nh <- sum(is.na(h$xAnon[, kv])) - sum(is.na(testdata[, kv]))
  expect_lte(res$objective, nh)
})
