# TDD for the LNS matheuristic (A4): fix the incumbent outside a neighbourhood
# of violators, re-solve the small MILP exactly, accept improvements, repeat.
# Monotone by construction; equals the exact solve when the neighbourhood
# covers all violators.

test_that("LNS with a full-width neighbourhood is the exact solve", {
  skip_if_not_installed("highs")
  res <- ls_lns(francdat, keyVars = c(4, 5, 6), k = 2,
                neighborhood = 100, time_limit = 60, seed = 1)
  expect_equal(res$objective, 2)               # the proven francdat optimum
  expect_true(res$proven)
  expect_true(ls_check(res$xAnon, c(4, 5, 6), k = 2)$ok)
})

test_that("LNS never worsens and its trajectory is monotone", {
  skip_if_not_installed("highs")
  res <- ls_lns(francdat, keyVars = c(4, 5, 6), k = 2,
                neighborhood = 2, iterations = 8, per_solve_limit = 5,
                time_limit = 60, seed = 7)
  expect_true(all(diff(res$trajectory) <= 0))
  expect_lte(res$objective, res$trajectory[1])
  expect_true(ls_check(res$xAnon, c(4, 5, 6), k = 2)$ok)
})

test_that("LNS respects the exact optimum as a floor on random instances", {
  skip_if_not_installed("highs")
  set.seed(20260902)
  tested <- 0L
  for (rep in 1:10) {
    x <- as.data.frame(lapply(1:3, function(j)
      as.character(sample.int(3, 8, replace = TRUE))),
      stringsAsFactors = FALSE)
    names(x) <- c("a", "b", "c")
    if (length(ls_check(x, names(x), 2)$violators) == 0L) next
    e <- try(ls_optimal(x, keyVars = names(x), k = 2), silent = TRUE)
    if (inherits(e, "try-error") || !identical(e$status, "optimal")) next
    l <- ls_lns(x, keyVars = names(x), k = 2, neighborhood = 3,
                iterations = 6, per_solve_limit = 5, time_limit = 30,
                seed = rep)
    tested <- tested + 1L
    expect_gte(l$objective, e$objective)
    expect_true(ls_check(l$xAnon, names(x), 2)$ok, info = paste("rep", rep))
  }
  expect_gt(tested, 3L)
})

test_that("LNS is reproducible under a seed", {
  skip_if_not_installed("highs")
  a <- ls_lns(francdat, keyVars = c(4, 5, 6), k = 2,
              neighborhood = 2, iterations = 5, time_limit = 30, seed = 42)
  b <- ls_lns(francdat, keyVars = c(4, 5, 6), k = 2,
              neighborhood = 2, iterations = 5, time_limit = 30, seed = 42)
  expect_equal(a$objective, b$objective)
  expect_identical(a$xAnon, b$xAnon)
})
