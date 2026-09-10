# TDD for the de Waal (1998/2003, Ch. 17) MINUC hitting-set baseline.
# Safety there is STATIC: hit every minimum unsafe combination, frequencies
# always counted on the ORIGINAL file. The certificate is sufficient but never
# necessary for S1, so ls_minuc()'s optimum bounds ls_optimal()'s from above;
# the slack is the measured price of the 1998 assumption (docs/06).

test_that("MINUCs are found and hit at minimum cost on the 3x2 witness", {
  # a = (1,1,2), b = (1,2,2), k = 2. Marginal counts give MINUCs:
  # r1 {b}, r2 {a,b}, r3 {a}. Each needs one suppression -> objective 3.
  x <- data.frame(a = c("1", "1", "2"), b = c("1", "2", "2"),
                  stringsAsFactors = FALSE)
  res <- ls_minuc(x, keyVars = c("a", "b"), k = 2)
  expect_equal(res$objective, 3)
  expect_equal(res$nsupp, 3L)
  expect_true(is.na(res$xAnon[1, "b"]))   # r1's only MINUC is {b}
  expect_true(is.na(res$xAnon[3, "a"]))   # r3's only MINUC is {a}
})

test_that("the de Waal certificate implies S1 safety (strict upper bound)", {
  x <- data.frame(a = c("1", "1", "2"), b = c("1", "2", "2"),
                  stringsAsFactors = FALSE)
  minuc <- ls_minuc(x, keyVars = c("a", "b"), k = 2)
  expect_true(ls_check(minuc$xAnon, c("a", "b"), k = 2)$ok)
  skip_if_not_installed("highs")
  exact <- ls_optimal(x, keyVars = c("a", "b"), k = 2)
  expect_equal(exact$objective, 2)                 # wildcard does it with two
  expect_gte(minuc$objective, exact$objective)     # and MINUC pays more
})

test_that("MINUC solutions are S1-safe and bound the optimum on random data", {
  skip_if_not_installed("highs")
  set.seed(20260901 + 42)
  tested <- 0L
  for (rep in 1:30) {
    p <- sample(2:3, 1)
    n <- sample(5:9, 1)
    x <- as.data.frame(lapply(seq_len(p), function(j)
      as.character(sample.int(3, n, replace = TRUE))),
      stringsAsFactors = FALSE)
    names(x) <- paste0("k", seq_len(p))
    k <- 2
    m <- ls_minuc(x, keyVars = names(x), k = k)
    expect_true(ls_check(m$xAnon, names(x), k)$ok, info = paste("rep", rep))
    e <- try(ls_optimal(x, keyVars = names(x), k = k), silent = TRUE)
    if (inherits(e, "try-error") || !identical(e$status, "optimal")) next
    tested <- tested + 1L
    expect_gte(m$objective, e$objective)
  }
  expect_gt(tested, 10L)
})

test_that("records without unsafe combinations are untouched", {
  x <- data.frame(a = c("1", "1", "2", "2"), b = c("1", "1", "2", "2"),
                  stringsAsFactors = FALSE)
  res <- ls_minuc(x, keyVars = c("a", "b"), k = 2)
  expect_equal(res$objective, 0)
  expect_identical(res$xAnon, x)
})

test_that("importance weights steer the hitting choice", {
  # r2's only MINUC is {a,b}: either key hits it; importance decides.
  x <- data.frame(a = c("1", "1", "2"), b = c("1", "2", "2"),
                  stringsAsFactors = FALSE)
  keep_a <- ls_minuc(x, keyVars = c("a", "b"), k = 2, importance = c(1, 2))
  expect_true(is.na(keep_a$xAnon[2, "b"]))
  expect_false(is.na(keep_a$xAnon[2, "a"]))
  keep_b <- ls_minuc(x, keyVars = c("a", "b"), k = 2, importance = c(2, 1))
  expect_true(is.na(keep_b$xAnon[2, "a"]))
  expect_false(is.na(keep_b$xAnon[2, "b"]))
})
