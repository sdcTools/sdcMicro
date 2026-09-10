# TDD for ls_hub(): the closed-form hub construction of docs/08 Thm 4.5.
# Draft (k-1) violators per value of one key column and blank their other
# cells. Cost is O(k * d_j * p), independent of n -- the constructive reading
# of the hub family (Thm 4.3) that the 2026-09-04 review gate exposed as
# missing. It is an upper bound, not an optimiser: on small instances the
# exact optimum is far below it.

test_that("hub construction is feasible and cheap on the hub family", {
  # H_m: records (1,j) for j = 1..m plus (1,1). One hub suffices at k = 2.
  m <- 6
  x <- data.frame(a = rep("1", m + 1L),
                  b = c(as.character(seq_len(m)), "1"),
                  stringsAsFactors = FALSE)
  res <- ls_hub(x, keyVars = c("a", "b"), k = 2)
  expect_true(ls_check(res$xAnon, c("a", "b"), k = 2)$ok)
  # column a has a single value, so one hub costs p - 1 = 1 cell
  expect_equal(res$nsupp, 1L)
  expect_equal(res$column, "a")
})

test_that("hub construction respects the violator-only restriction", {
  # two values in column a, each carried by three records: enough mass for
  # one hub per value at k = 2.
  x <- data.frame(a = c("1", "1", "1", "2", "2", "2"),
                  b = c("1", "2", "3", "1", "2", "3"),
                  stringsAsFactors = FALSE)
  V <- ls_check(x, c("a", "b"), k = 2)$violators
  res <- ls_hub(x, keyVars = c("a", "b"), k = 2)
  expect_true(ls_check(res$xAnon, c("a", "b"), k = 2)$ok)
  touched <- which(rowSums(is.na(res$xAnon[, c("a", "b")])) > 0L)
  expect_true(all(touched %in% V))
  expect_equal(res$nsupp, 2L)          # one hub per value, p - 1 = 1 cell each
})

test_that("hub construction meets the stated bound (k-1) * d_j * (p-1)", {
  set.seed(11)
  for (trial in 1:8) {
    n <- 40L
    p <- 4L
    x <- as.data.frame(matrix(as.character(sample.int(5L, n * p, TRUE)),
                              n, p), stringsAsFactors = FALSE)
    names(x) <- paste0("K", seq_len(p))
    for (k in c(2L, 3L)) {
      res <- ls_hub(x, keyVars = names(x), k = k)
      if (is.null(res$xAnon)) next            # hypotheses violated: reported
      expect_true(ls_check(res$xAnon, names(x), k = k)$ok)
      d <- length(unique(x[[res$column]]))
      expect_lte(res$nsupp, (k - 1L) * d * (p - 1L))
    }
  }
})

test_that("hub construction is an upper bound, not an optimum", {
  # testdata, four keys, k = 3: the exact optimum is 3 (proved,
  # benchmarks/e1-pilot-testdata.txt), the construction pays 6.
  skip_if_not_installed("sdcMicro")
  data(testdata, package = "sdcMicro", envir = environment())
  kv <- c("urbrur", "roof", "walls", "water")
  res <- ls_hub(testdata, keyVars = kv, k = 3)
  expect_true(ls_check(res$xAnon, kv, k = 3)$ok)
  opt <- ls_optimal(testdata, keyVars = kv, k = 3)
  expect_gt(res$nsupp, opt$objective)   # strictly worse than the optimum
  expect_equal(opt$objective, 3)
})

test_that("hub construction reports infeasibility instead of a broken file", {
  # A value occurring in exactly one record cannot sustain a hub at k = 2:
  # the drafted hub would keep a value nobody else carries.
  x <- data.frame(a = c("1", "2", "3", "4"),
                  b = c("1", "2", "3", "4"),
                  stringsAsFactors = FALSE)
  res <- ls_hub(x, keyVars = c("a", "b"), k = 2)
  expect_null(res$xAnon)
  expect_false(res$feasible)
})

test_that("hub construction exploits genuine missing values", {
  # A violator that already misses a cell is a cheaper hub.
  x <- data.frame(a = c("1", "1", "1", "1"),
                  b = c("1", "2", "3", "4"),
                  c = c("1", "2", "3", NA),
                  stringsAsFactors = FALSE)
  res <- ls_hub(x, keyVars = c("a", "b", "c"), k = 2)
  expect_true(ls_check(res$xAnon, c("a", "b", "c"), k = 2)$ok)
  # the record with the genuine NA is the cheapest hub: one cell instead of two
  expect_equal(res$nsupp, 1L)
})

test_that("the bound must be minimised over ADMISSIBLE columns only", {
  # Counterexample from the 2026-09-04 EJOR review to the earlier statement
  # "OPT <= (k-1) * min_j |D_j| * (p-1)": the minimising column need not
  # satisfy the hypotheses, and then nothing is proved. Here d_min over all
  # columns is 1, the claimed bound 4, and the true optimum 5.
  x <- data.frame(K1 = c("v", "v", "w", "w", "w"),
                  K2 = c("1", "2", "5", "5", "5"),
                  K3 = c("1", "2", "5", "5", "5"), stringsAsFactors = FALSE)
  kv <- names(x); k <- 3
  V <- ls_check(x, kv, k = k)$violators
  expect_equal(V, 1:2)
  admissible <- vapply(kv, function(cn) {
    Dj <- unique(stats::na.omit(x[[cn]][V]))
    length(Dj) > 0 && all(vapply(Dj, function(v)
      sum(x[[cn]][V] == v, na.rm = TRUE) >= k - 1 &&
      sum(x[[cn]] %in% c(v, NA)) >= k, logical(1)))
  }, logical(1))
  expect_false(any(admissible))            # no column is admissible here
  expect_gt(brute_force_optimum(x, kv, k = k, cells = "all"),
            (k - 1) * min(vapply(kv, function(cn)
              length(unique(stats::na.omit(x[[cn]][V]))), integer(1))) *
              (length(kv) - 1))
  # and ls_hub() reports infeasibility rather than returning the bad bound
  expect_false(ls_hub(x, kv, k = k)$feasible)
})
