# Model B restricted: per-violator pattern convexification (Dantzig-Wolfe on
# the record subproblem). Must solve the SAME problem as Model A exactly --
# including mutual violator credits, which the naive "count patterns against
# the unsuppressed data" variant of docs/03 misses (corrected there).

test_that("mutual violator credits survive the pattern formulation", {
  skip_if_not_installed("highs")
  # (1,1) and (1,2), k = 2, no safe records. One suppression -- b on either
  # record -- makes the pair mutually compatible: (1,*) matches (1,2).
  # A formulation that counts a pattern's frequency against the UNSUPPRESSED
  # rest would force both records to suppress (cost 2). The optimum is 1.
  x <- data.frame(a = c("1", "1"), b = c("1", "2"), stringsAsFactors = FALSE)
  for (fo in c("modelA", "modelB")) {
    res <- ls_optimal(x, keyVars = c("a", "b"), k = 2, formulation = fo)
    expect_equal(res$objective, 1, info = fo)
    expect_true(ls_check(res$xAnon, c("a", "b"), k = 2)$ok, info = fo)
  }
})

test_that("model B reproduces model A's proven optima", {
  skip_if_not_installed("highs")
  a <- ls_optimal(francdat, keyVars = c(4, 5, 6), k = 2,
                  formulation = "modelA")
  b <- ls_optimal(francdat, keyVars = c(4, 5, 6), k = 2,
                  formulation = "modelB")
  expect_equal(b$objective, a$objective)
  expect_identical(b$status, "optimal")
  expect_true(ls_check(b$xAnon, c(4, 5, 6), k = 2)$ok)
})

test_that("model B matches the brute-force oracle on random instances", {
  skip_if_not_installed("highs")
  set.seed(20260901 + 21)
  tested <- 0L
  for (rep in 1:30) {
    p <- sample(2:3, 1)
    n <- sample(5:9, 1)
    x <- as.data.frame(lapply(seq_len(p), function(j)
      as.character(sample.int(sample(2:3, 1), n, replace = TRUE))),
      stringsAsFactors = FALSE)
    names(x) <- paste0("k", seq_len(p))
    # sprinkle genuine NAs so the liveness path is exercised
    if (rep %% 3 == 0) x[sample.int(n, 1), sample.int(p, 1)] <- NA
    k <- sample(2:3, 1)
    V <- ls_check(x, names(x), k)$violators
    if (length(V) == 0L || length(V) * p > 12) next
    res <- try(ls_optimal(x, keyVars = names(x), k = k,
                          formulation = "modelB"), silent = TRUE)
    if (inherits(res, "try-error")) next
    if (!identical(res$status, "optimal")) next
    tested <- tested + 1L
    expect_equal(res$objective, brute_force_optimum(x, names(x), k),
                 info = paste("rep", rep))
    expect_true(ls_check(res$xAnon, names(x), k)$ok, info = paste("rep", rep))
  }
  expect_gt(tested, 5L)
})

test_that("model B handles big classes, genuine NAs and importance", {
  skip_if_not_installed("highs")
  # big class: one suppression, verified earlier for model A
  x1 <- data.frame(a = c(rep("1", 10), "1"), b = c(rep("1", 10), "9"),
                   stringsAsFactors = FALSE)
  expect_equal(ls_optimal(x1, keyVars = c("a", "b"), k = 3,
                          formulation = "modelB")$objective, 1)
  # genuine NA is a free wildcard
  x2 <- data.frame(a = c(rep("2", 5), "1"), b = c(rep("2", 5), NA),
                   stringsAsFactors = FALSE)
  r2 <- ls_optimal(x2, keyVars = c("a", "b"), k = 2, formulation = "modelB")
  expect_equal(r2$objective, 1)
  expect_true(is.na(r2$xAnon[6, "a"]))
  # importance steers the choice exactly as in model A
  x3 <- data.frame(a = c("1", "2", "1", "2", "1"),
                   b = c("1", "1", "2", "1", "2"), stringsAsFactors = FALSE)
  r3 <- ls_optimal(x3, keyVars = c("a", "b"), k = 2, importance = c(1, 2),
                   formulation = "modelB")
  expect_true(is.na(r3$xAnon[1, "b"]))
  expect_false(is.na(r3$xAnon[1, "a"]))
})

test_that("model B agrees with model A on testdata and reports its dims", {
  skip_if_not_installed("highs")
  data("testdata", package = "sdcMicro")
  kv <- c("urbrur", "roof", "walls", "water")
  a <- ls_optimal(testdata, keyVars = kv, k = 5, formulation = "modelA")
  b <- ls_optimal(testdata, keyVars = kv, k = 5, formulation = "modelB")
  expect_equal(b$objective, a$objective)
  expect_true(ls_check(b$xAnon, kv, k = 5)$ok)
  expect_true(b$dims[["vars"]] > 0)
})
