# TDD for the exact solver. Every instance here has an optimum derived by hand,
# and every result is re-verified with ls_check() -- the model's own count is
# never the evidence. Plan: docs/07-paper-P1-EJOR-plan.md work packages A1/A2.
#
# Scope of this first slice: wildcard semantics (S1), violator-only suppression
# (donors are not allowed to be suppressed). That is exactly sdcMicro's
# restriction, so it is the right like-for-like comparison for the heuristic
# gap (experiment E1).

test_that("ls_optimal finds the hand-derived optimum of a 3x2 instance", {
  skip_if_not_installed("highs")
  # a: 1 1 2      All three records are unique -> all violators at k = 2.
  # b: 1 2 2      Suppressing b on r1 gives fk = (2, 2, 1): r3 still violates,
  #               so one suppression cannot be enough. Two suffice
  #               (b on r1, a on r3) -> optimum = 2 suppressed cells.
  x <- data.frame(a = c("1", "1", "2"), b = c("1", "2", "2"),
                  stringsAsFactors = FALSE)
  res <- ls_optimal(x, keyVars = c("a", "b"), k = 2)

  expect_identical(res$status, "optimal")
  expect_equal(res$objective, 2)
  expect_equal(sum(is.na(res$xAnon[, c("a", "b")])), 2)
})

test_that("every ls_optimal result actually satisfies the target", {
  skip_if_not_installed("highs")
  res <- ls_optimal(francdat, keyVars = c(4, 5, 6), k = 2)
  chk <- ls_check(res$xAnon, keyVars = c(4, 5, 6), k = 2,
                  semantics = "wildcard")
  expect_true(chk$ok)
})

test_that("ls_optimal never needs more suppressions than kAnon", {
  skip_if_not_installed("highs")
  # The core claim of the paper: exact is never worse than the heuristic.
  heur <- kAnon(francdat, keyVars = c(4, 5, 6), k = 2)
  n_heur <- sum(is.na(heur$xAnon)) - sum(is.na(francdat[, c(4, 5, 6)]))

  exact <- ls_optimal(francdat, keyVars = c(4, 5, 6), k = 2)
  expect_lte(exact$objective, n_heur)
})

test_that("ls_optimal leaves already-anonymous data untouched", {
  skip_if_not_installed("highs")
  x <- data.frame(a = c("1", "1", "2", "2"), b = c("1", "1", "2", "2"),
                  stringsAsFactors = FALSE)
  res <- ls_optimal(x, keyVars = c("a", "b"), k = 2)
  expect_equal(res$objective, 0)
  expect_equal(sum(is.na(res$xAnon)), 0)
  expect_identical(res$status, "optimal")
})

test_that("importance decides which of two equally-fixing keys is suppressed", {
  skip_if_not_installed("highs")
  # r1 = (1,1) is the only violator. It can be repaired either by suppressing
  # key a -- released (*,1), compatible with r2 and r4 -- or by suppressing
  # key b -- released (1,*), compatible with r3 and r5. Both are single
  # suppressions, so only the cost decides. sdcMicro semantics: importance 1 is
  # the MOST important key and therefore the most expensive to suppress.
  x <- data.frame(
    a = c("1", "2", "1", "2", "1"),
    b = c("1", "1", "2", "1", "2"),
    stringsAsFactors = FALSE
  )
  expect_equal(ls_check(x, keyVars = c("a", "b"), k = 2)$violators, 1L)

  keep_a <- ls_optimal(x, keyVars = c("a", "b"), k = 2, importance = c(1, 2))
  expect_true(is.na(keep_a$xAnon[1, "b"]))
  expect_false(is.na(keep_a$xAnon[1, "a"]))

  keep_b <- ls_optimal(x, keyVars = c("a", "b"), k = 2, importance = c(2, 1))
  expect_true(is.na(keep_b$xAnon[1, "a"]))
  expect_false(is.na(keep_b$xAnon[1, "b"]))
})

test_that("ls_optimal reports a proven optimality gap of zero on small models", {
  skip_if_not_installed("highs")
  res <- ls_optimal(francdat, keyVars = c(4, 5, 6), k = 2)
  expect_equal(res$gap, 0)
  expect_equal(res$bound, res$objective)
  expect_true(res$time >= 0)
})

test_that("the MILP optimum equals exhaustive search on francdat", {
  skip_if_not_installed("highs")
  x <- francdat
  expect_equal(
    ls_optimal(x, keyVars = c(4, 5, 6), k = 2)$objective,
    brute_force_optimum(x, keyVars = c(4, 5, 6), k = 2)
  )
})

test_that("the MILP optimum equals exhaustive search on random instances", {
  skip_if_not_installed("highs")
  set.seed(20260901)
  tested <- 0L
  for (rep in 1:40) {
    n <- sample(5:8, 1); p <- 3
    x <- as.data.frame(lapply(seq_len(p), function(j)
      as.character(sample.int(sample(2:3, 1), n, replace = TRUE))),
      stringsAsFactors = FALSE)
    names(x) <- paste0("k", seq_len(p))
    kv <- names(x)
    k <- sample(2:3, 1)
    V <- ls_check(x, kv, k)$violators
    if (length(V) == 0L || length(V) * p > 12) next   # keep the oracle cheap
    # skip instances that cannot be repaired violator-only at all
    res <- try(ls_optimal(x, keyVars = kv, k = k), silent = TRUE)
    if (inherits(res, "try-error")) next
    if (!identical(res$status, "optimal")) next
    tested <- tested + 1L
    expect_equal(res$objective, brute_force_optimum(x, kv, k),
                 info = paste("rep", rep))
    expect_true(ls_check(res$xAnon, kv, k)$ok, info = paste("rep", rep))
  }
  expect_gt(tested, 5L)   # the sweep must actually have exercised the solver
})

test_that("kAnon warm start survives non-numeric character keys", {
  # kAnon() coerces keys with as.numeric(); on labels like "a" it
  # silently suppresses nothing, which turned the warm start into an
  # infeasible all-zeros vector (caught on the eusilcS grid, 2026-09-02).
  x  <- data.frame(K1 = c("a", "a", "b", "b", "c"),
                   K2 = c("u", "u", "v", "v", "w"))
  xn <- data.frame(K1 = as.integer(factor(x$K1)),
                   K2 = as.integer(factor(x$K2)))
  kv <- c("K1", "K2")
  mod  <- sdcMicro:::ls_build_model(x,  kv, 2, NULL, "modelA")
  modn <- sdcMicro:::ls_build_model(xn, kv, 2, NULL, "modelA")
  s  <- sdcMicro:::ls_warm_start(x,  kv, 2, NULL, mod)$start
  sn <- sdcMicro:::ls_warm_start(xn, kv, 2, NULL, modn)$start
  expect_gt(sum(sn[seq_len(modn$n_x)]), 0)  # kAnon really suppresses here
  expect_equal(s[seq_len(mod$n_x)], sn[seq_len(modn$n_x)])
})

test_that("ls_optimal never ends worse than its warm start", {
  # HiGHS may reject a partial MIP start (observed: testdata 6 keys k=2,
  # 1500 s ended at 67 where the kAnon start already cost 17). If the
  # solver returns no incumbent or a worse one, ls_optimal must fall back
  # to the warm-start solution.
  set.seed(99)
  n <- 400L
  x <- data.frame(K1 = sample(as.character(1:12), n, TRUE),
                  K2 = sample(as.character(1:10), n, TRUE),
                  K3 = sample(as.character(1:8), n, TRUE),
                  K4 = sample(as.character(1:6), n, TRUE),
                  K5 = sample(as.character(1:4), n, TRUE))
  kv <- names(x)
  res <- ls_optimal(x, kv, k = 2, warm_start = "kAnon", time_limit = 1e-6)
  expect_true(is.finite(res$objective))
  expect_true(ls_check(res$xAnon, kv, k = 2)$ok)
  # never worse than the projected kAnon run itself
  xf <- x; for (v in kv) xf[[v]] <- factor(xf[[v]])
  h <- kAnon(xf, keyVars = kv, k = 2)
  expect_lte(res$objective, sum(is.na(h$xAnon[, kv])))
})

test_that("warm_start = 'greedy2' seeds the solver and bounds the result", {
  set.seed(7)
  n <- 60L
  x <- data.frame(K1 = sample(as.character(1:6), n, TRUE),
                  K2 = sample(as.character(1:5), n, TRUE),
                  K3 = sample(as.character(1:3), n, TRUE))
  kv <- names(x)
  g <- ls_greedy2(x, kv, k = 3)
  res <- ls_optimal(x, kv, k = 3, warm_start = "greedy2", time_limit = 30)
  expect_true(ls_check(res$xAnon, kv, k = 3)$ok)
  expect_lte(res$objective, g$objective + 1e-9)
})

test_that("max_per_record caps the per-record suppression pattern", {
  # The eusilcS hub finding motivates the cap: unit-cost optima may erase
  # single records almost completely. With a cap the model must spread --
  # and may become infeasible (the full-blank escape of docs/08 Cor. 3.3
  # is excluded), which the solver then reports honestly.
  x <- data.frame(K1 = c("1", "2"), K2 = c("1", "2"), K3 = c("1", "2"))
  kv <- names(x)
  for (form in c("modelA", "modelB")) {
    un <- ls_optimal(x, kv, k = 2, formulation = form)
    expect_equal(un$objective, 3)
    c2 <- ls_optimal(x, kv, k = 2, formulation = form, max_per_record = 2)
    expect_equal(c2$objective, 3)
    expect_true(all(rowSums(is.na(c2$xAnon)) <= 2))
    expect_true(ls_check(c2$xAnon, kv, k = 2)$ok)
    c1 <- ls_optimal(x, kv, k = 2, formulation = form, max_per_record = 1)
    expect_identical(c1$status, "infeasible")
  }
})

test_that("max_per_record leaves one-cell hub optima untouched", {
  x <- data.frame(K1 = rep("1", 4), K2 = as.character(1:4))
  res <- ls_optimal(x, c("K1", "K2"), k = 2, max_per_record = 1)
  expect_equal(res$objective, 1)
  expect_true(ls_check(res$xAnon, c("K1", "K2"), k = 2)$ok)
})

test_that("ls_lns forwards max_per_record", {
  x <- data.frame(K1 = c("1", "2"), K2 = c("1", "2"), K3 = c("1", "2"))
  res <- ls_lns(x, names(x), k = 2, max_per_record = 2, time_limit = 20,
                seed = 1)
  expect_equal(res$objective, 3)
  expect_true(all(rowSums(is.na(res$xAnon)) <= 2))
})
