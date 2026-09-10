## The aggregated (tuple x pattern) model written down inside the proof of the
## fixed-p proposition but never built. Unlike ls_optimal() it is exact for the
## UNRESTRICTED problem -- donor suppressions included -- and it carries no
## |V|^2 pair block. Raised by the 2026-09-06 review as the paper's most
## consequential unbuilt experiment.

test_that("the aggregated model reproduces the hub family optimum", {
  ## H_m: tuples (1,j). One blanked cell makes a hub for everyone; the
  ## unrestricted optimum is 1 (manuscript Theorem 2.8).
  for (m in 3:5) {
    x <- data.frame(a = rep("1", m + 1),
                    b = as.character(seq_len(m + 1)), stringsAsFactors = TRUE)
    r <- ls_aggregate(x, keyVars = c("a", "b"), k = 2, solver = "highs")
    expect_identical(r$status, "optimal", info = paste("m =", m))
    expect_equal(r$objective, 1, info = paste("m =", m))
    expect_true(ls_check(r$xAnon, keyVars = c("a", "b"), k = 2)$ok)
  }
})

test_that("the aggregated model matches brute force over ALL cells", {
  ## the oracle searches every cell of every record, so it is the true
  ## unrestricted optimum; instances kept to <= 18 cells so 2^cells is feasible
  cases <- list(
    data.frame(a = c("1","1","2","2","3"), b = c("1","2","1","2","3"),
               c = c("1","1","1","1","2"), stringsAsFactors = TRUE),
    data.frame(a = c("1","2","3","9","9"), b = c("1","2","3","9","9"),
               c = c("5","5","5","5","5"), stringsAsFactors = TRUE),
    data.frame(a = c("1","1","2","3"), b = c("4","5","6","7"),
               c = c("8","8","9","9"), stringsAsFactors = TRUE)
  )
  for (i in seq_along(cases)) {
    x <- cases[[i]]; kv <- names(x)
    truth <- brute_force_optimum(x, kv, k = 2, cells = "all")
    r <- ls_aggregate(x, keyVars = kv, k = 2, solver = "highs")
    expect_identical(r$status, "optimal", info = paste("case", i))
    expect_equal(r$objective, truth, info = paste("case", i))
    expect_true(ls_check(r$xAnon, keyVars = kv, k = 2)$ok, info = paste("case", i))
  }
})

test_that("the aggregated optimum never exceeds the violator-only optimum", {
  ## the restriction is sound but not lossless, so <= must hold and the gap,
  ## where it is positive, is exactly what the manuscript could not quantify
  x <- data.frame(a = c("1","1","2","2","3","3"), b = c("1","2","1","2","3","4"),
                  c = c("7","7","7","8","8","8"), stringsAsFactors = TRUE)
  kv <- names(x)
  agg <- ls_aggregate(x, keyVars = kv, k = 2, solver = "highs")
  vio <- ls_optimal(x, keyVars = kv, k = 2, solver = "highs")
  expect_lte(agg$objective, vio$objective)
  expect_true(ls_check(agg$xAnon, keyVars = kv, k = 2)$ok)
})

test_that("the aggregated model honours per-column costs", {
  x <- data.frame(a = c("1","1","2","3"), b = c("4","5","6","7"),
                  stringsAsFactors = TRUE)
  kv <- names(x)
  ## column b made expensive: the optimiser should pay in a instead
  r <- ls_aggregate(x, keyVars = kv, k = 2, importance = c(2, 1), solver = "highs")
  expect_identical(r$status, "optimal")
  expect_true(ls_check(r$xAnon, keyVars = kv, k = 2)$ok)
  expect_equal(r$objective,
               brute_force_optimum(x, kv, k = 2, cost_j = c(2, 1), cells = "all"))
})

## --- what the exactness proof (manuscript Prop. 3.3, docs/08 Thm 4d.1) leans on ---
## Each of the next three tests loads one hypothesis of that proof; the last one
## sweeps them together against the oracle. The large sweep lives in
## benchmarks/aggregate-exactness.R, which is too slow for R CMD check.

test_that("the liveness correction of the count row fires", {
  ## an all-NA release credits every incomplete record and no complete one, so
  ## the complete record here is a violator although two records are compatible
  x <- data.frame(a = c("2", NA), b = c("2", NA), stringsAsFactors = TRUE)
  kv <- c("a", "b")
  expect_equal(ls_check(x, kv, k = 2)$fk, c(1, 2))    # the clause, as deployed
  truth <- brute_force_optimum(x, kv, k = 2, cells = "all")
  r <- ls_aggregate(x, kv, k = 2, solver = "highs")
  expect_identical(r$status, "optimal")
  expect_equal(r$objective, truth)
  expect_true(ls_check(r$xAnon, kv, k = 2)$ok)
})

test_that("pre-existing missings ride on the tuple", {
  ## O_t differs between tuples, so the pattern columns of different tuples have
  ## different lengths -- the step the proof needs for S_i subset of O_{t(i)}
  x <- data.frame(a = c("1", "1", "2", "3", NA),
                  b = c("4", NA, "5", "6", "7"),
                  c = c("8", "8", "9", NA, "9"), stringsAsFactors = TRUE)
  kv <- names(x)
  for (k in 2:3) {
    truth <- brute_force_optimum(x, kv, k = k, cells = "all")
    r <- ls_aggregate(x, kv, k = k, solver = "highs")
    expect_identical(r$status, "optimal", info = paste("k =", k))
    expect_equal(r$objective, truth, info = paste("k =", k))
    expect_true(ls_check(r$xAnon, kv, k = k)$ok, info = paste("k =", k))
  }
})

test_that("aggregation is exact on random instances with ties, gaps and column costs", {
  skip_on_cran()
  set.seed(907)
  for (it in 1:8) {
    p <- sample(2:3, 1); n <- sample(4:5, 1); a <- sample(2:3, 1)
    key <- as.data.frame(matrix(as.character(sample(seq_len(a), n * p, replace = TRUE)),
                                n, p), stringsAsFactors = FALSE)
    names(key) <- paste0("v", seq_len(p))
    key[matrix(runif(n * p) < 0.2, n, p)] <- NA
    imp <- if (it %% 2 == 0) sample(seq_len(p)) else NULL
    cost_j <- if (is.null(imp)) rep(1, p) else p + 1 - imp
    truth <- brute_force_optimum(key, names(key), k = 2, cost_j = cost_j, cells = "all")
    r <- ls_aggregate(key, names(key), k = 2, importance = imp, solver = "highs")
    expect_identical(r$status, "optimal", info = paste("iteration", it))
    expect_equal(r$objective, truth, info = paste("iteration", it))
    expect_true(ls_check(r$xAnon, names(key), k = 2)$ok, info = paste("iteration", it))
  }
})

## --- semantics beyond the wildcard reading -----------------------------------
## The count row is the only place the criterion enters the aggregated model, so
## the classical (identical-tuple) reading is the same model with compatibility
## replaced by equality, and the freqCalc weight alpha is the same model with a
## per-contributor coefficient. Both are checked against the same brute-force
## oracle, which delegates its feasibility test to sdcMicro.

test_that("the aggregated model solves classical (identical-tuple) k-anonymity", {
  cases <- list(
    data.frame(a = c("1","1","2","2","3"), b = c("1","2","1","2","3"),
               stringsAsFactors = TRUE),
    data.frame(a = c("1","1","2","3"), b = c("4","5","6","7"),
               c = c("8","8","9","9"), stringsAsFactors = TRUE)
  )
  for (i in seq_along(cases)) {
    x <- cases[[i]]; kv <- names(x)
    truth <- brute_force_optimum(x, kv, k = 2, cells = "all",
                                 semantics = "identical")
    r <- ls_aggregate(x, kv, k = 2, semantics = "identical", solver = "highs")
    expect_identical(r$status, "optimal", info = paste("case", i))
    expect_equal(r$objective, truth, info = paste("case", i))
    expect_true(ls_check(r$xAnon, kv, k = 2, semantics = "identical")$ok,
                info = paste("case", i))
  }
})

test_that("classical is never cheaper than wildcard (Theorem 2.9 chain)", {
  x <- data.frame(a = c("1","1","2","2","3","3"), b = c("1","2","1","2","3","4"),
                  c = c("7","7","7","8","8","8"), stringsAsFactors = TRUE)
  kv <- names(x)
  w <- ls_aggregate(x, kv, k = 2, semantics = "wildcard",  solver = "highs")
  s <- ls_aggregate(x, kv, k = 2, semantics = "identical", solver = "highs")
  expect_lte(w$objective, s$objective)
})

test_that("the aggregated model is exact for the freqCalc weight alpha < 1", {
  ## alpha prices a contributor that itself carries a missing value; it is a
  ## constant per released tuple, so the count row stays linear.  Section 7.4
  ## of the manuscript could previously only evaluate a released file at
  ## alpha < 1, not optimise for it.
  cases <- list(
    data.frame(a = c("1","1","2","3"), b = c("4","5","6","7"), stringsAsFactors = TRUE),
    data.frame(a = c("1","1","2","2","3"), b = c("1","2","1","2","3"),
               c = c("1","1","1","1","2"), stringsAsFactors = TRUE)
  )
  for (i in seq_along(cases)) for (al in c(1, 0.5)) {
    x <- cases[[i]]; kv <- names(x)
    truth <- brute_force_optimum(x, kv, k = 2, cells = "all", alpha = al)
    r <- ls_aggregate(x, kv, k = 2, alpha = al, solver = "highs")
    lab <- paste("case", i, "alpha", al)
    expect_identical(r$status, "optimal", info = lab)
    expect_equal(r$objective, truth, info = lab)
    expect_true(ls_check(r$xAnon, kv, k = 2, alpha = al)$ok, info = lab)
  }
})

test_that("a smaller alpha never makes protection cheaper", {
  x <- data.frame(a = c("1","1","2","2","3"), b = c("1","2","1","2","3"),
                  c = c("1","1","1","1","2"), stringsAsFactors = TRUE)
  kv <- names(x)
  o <- vapply(c(1, 0.75, 0.5), function(al)
    ls_aggregate(x, kv, k = 2, alpha = al, solver = "highs")$objective, numeric(1))
  expect_true(all(diff(o) >= 0))
})

## --- targets that are not k-anonymity ----------------------------------------
## Every count-based criterion is a vector of per-record thresholds (ls_target).
## Records sharing a tuple are interchangeable only if they also share a
## threshold, so the aggregation partitions by (tuple, threshold) and the count
## row is replicated once per distinct threshold that occupies a released tuple.

test_that("the aggregated model honours per-record thresholds", {
  x <- data.frame(a = c("1","1","2","2","3","3"), b = c("1","2","1","2","3","4"),
                  stringsAsFactors = TRUE)
  kv <- names(x)
  kvec <- c(3, 3, 2, 2, 2, 2)          # the first two records need more cover
  truth <- brute_force_optimum(x, kv, k = kvec, cells = "all")
  r <- ls_aggregate(x, kv, k = kvec, solver = "highs")
  expect_identical(r$status, "optimal")
  expect_equal(r$objective, truth)
  expect_true(ls_check(r$xAnon, kv, k = kvec)$ok)
})

test_that("a constant threshold vector reproduces the scalar model", {
  x <- data.frame(a = c("1","1","2","3"), b = c("4","5","6","7"),
                  c = c("8","8","9","9"), stringsAsFactors = TRUE)
  kv <- names(x)
  a <- ls_aggregate(x, kv, k = 2, solver = "highs")
  b <- ls_aggregate(x, kv, k = rep(2, nrow(x)), solver = "highs")
  expect_equal(a$objective, b$objective)
})

test_that("an individual-risk target is solved exactly", {
  x <- data.frame(a = c("1","1","2","2","3"), b = c("1","2","1","2","3"),
                  stringsAsFactors = TRUE)
  kv <- names(x)
  tg <- ls_target(x, kv, risk = 0.4)            # population case: k_i = ceiling(1/0.4) = 3
  expect_equal(unique(as.integer(tg)), 3L)
  truth <- brute_force_optimum(x, kv, k = tg, cells = "all")
  r <- ls_aggregate(x, kv, k = tg, solver = "highs")
  expect_identical(r$status, "optimal")
  expect_equal(r$objective, truth)
  expect_true(ls_check(r$xAnon, kv, k = tg)$ok)
})
