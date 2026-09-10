# TDD for the solver backend shim. Every test states a model whose optimum is
# known by hand, so a wrong backend translation cannot hide behind a plausible
# number. See docs/07-paper-P1-EJOR-plan.md work package A1.

skip_if_no_solver <- function(solver) {
  skip_if_not_installed(solver)
}

test_that("solve_milp finds the optimum of a tiny binary covering model", {
  skip_if_no_solver("highs")
  # min x1 + x2  s.t.  x1 + x2 >= 1,  x binary   ->  optimum 1
  res <- solve_milp(
    obj   = c(1, 1),
    A     = matrix(c(1, 1), nrow = 1),
    sense = ">=",
    rhs   = 1,
    types = c("B", "B")
  )
  expect_equal(res$objective, 1)
  expect_equal(sum(res$solution), 1)
  expect_identical(res$status, "optimal")
})

test_that("solve_milp honours all three row senses", {
  skip_if_no_solver("highs")
  # min x1 + 2 x2  s.t.  x1 + x2 >= 2,  x1 <= 1,  x binary  ->  x = (1,1), obj 3
  res <- solve_milp(
    obj   = c(1, 2),
    A     = rbind(c(1, 1), c(1, 0)),
    sense = c(">=", "<="),
    rhs   = c(2, 1),
    types = "B"
  )
  expect_equal(res$objective, 3)
  expect_equal(res$solution, c(1, 1))
})

test_that("solve_milp handles equality rows and integer variables", {
  skip_if_no_solver("highs")
  # min x1 + x2  s.t.  x1 + x2 == 3,  x integer >= 0  ->  obj 3
  res <- solve_milp(
    obj   = c(1, 1),
    A     = matrix(c(1, 1), nrow = 1),
    sense = "==",
    rhs   = 3,
    types = "I"
  )
  expect_equal(res$objective, 3)
  expect_equal(sum(res$solution), 3)
})

test_that("solve_milp reports bound, gap and time", {
  skip_if_no_solver("highs")
  res <- solve_milp(
    obj = c(1, 1), A = matrix(c(1, 1), nrow = 1),
    sense = ">=", rhs = 1, types = "B"
  )
  expect_equal(res$bound, 1)
  expect_equal(res$gap, 0)
  expect_true(is.numeric(res$time) && res$time >= 0)
  expect_identical(res$solver, "highs")
})

test_that("solve_milp reports infeasibility rather than a bogus optimum", {
  skip_if_no_solver("highs")
  # x binary, x1 >= 1 and x1 <= 0 simultaneously -> infeasible
  res <- solve_milp(
    obj = c(1), A = rbind(1, 1), sense = c(">=", "<="),
    rhs = c(1, 0), types = "B"
  )
  expect_identical(res$status, "infeasible")
})

test_that("the scip backend agrees with highs on the same model", {
  skip_if_no_solver("highs"); skip_if_no_solver("scip")
  args <- list(
    obj = c(3, 2, 4), A = rbind(c(1, 1, 0), c(0, 1, 1), c(1, 0, 1)),
    sense = ">=", rhs = c(1, 1, 1), types = "B"
  )
  h <- do.call(solve_milp, c(args, list(solver = "highs")))
  s <- do.call(solve_milp, c(args, list(solver = "scip")))
  expect_equal(h$objective, s$objective)
  expect_identical(s$solver, "scip")
})

test_that("solve_milp honours explicit variable bounds", {
  skip_if_no_solver("highs")
  # min -x  s.t. x >= 0, x continuous in [0, 5]  ->  objective -5.
  # Without a working upper bound the model would be unbounded, which is
  # exactly the failure mode that would make the pair variables of Model A
  # satisfy the count constraint for free.
  res <- solve_milp(
    obj = -1, A = matrix(1, nrow = 1), sense = ">=", rhs = 0,
    types = "C", lower = 0, upper = 5
  )
  expect_equal(res$objective, -5)
  expect_equal(res$solution, 5)
})

test_that("solve_milp accepts a warm-start incumbent", {
  skip_if_no_solver("highs")
  # A feasible (non-optimal) start must be accepted and must not change the
  # optimum: min x1+x2, x1+x2 >= 1, binary; start at the feasible (1,1).
  res <- solve_milp(
    obj = c(1, 1), A = matrix(c(1, 1), nrow = 1), sense = ">=", rhs = 1,
    types = "B", start = c(1, 1)
  )
  expect_identical(res$status, "optimal")
  expect_equal(res$objective, 1)
})

test_that("scip bound recovery inverts SCIP's relative-gap convention", {
  # SCIP reports gap = |primal - dual| / min(|primal|, |dual|). For
  # minimisation with 0 < dual <= primal the denominator is the dual bound,
  # so dual = primal / (1 + gap) -- NOT primal - |primal| * gap, which went
  # negative on the hard rows (benchmarks/e-t2-lp-gap.txt, obj 20 with gap
  # 1.995 gave "bound" -19.9 instead of 6.68).
  expect_equal(scip_bound_from_gap(20, 1.995), 20 / 2.995, tolerance = 1e-12)
  expect_equal(scip_bound_from_gap(455, 173.871), 455 / 174.871,
               tolerance = 1e-12)
  expect_equal(scip_bound_from_gap(10, 0), 10)
  # negative objective: |dual| >= |primal|, denominator is |primal|
  expect_equal(scip_bound_from_gap(-10, 0.5), -15)
  # no finite gap -> no recoverable bound
  expect_true(is.na(scip_bound_from_gap(20, Inf)))
  expect_equal(scip_bound_from_gap(0, 0), 0)
})

test_that("the gurobi backend agrees with highs on the same model", {
  skip_if_no_solver("highs"); skip_if_no_solver("gurobi")
  args <- list(
    obj = c(3, 2, 4), A = rbind(c(1, 1, 0), c(0, 1, 1), c(1, 0, 1)),
    sense = ">=", rhs = c(1, 1, 1), types = "B"
  )
  h <- do.call(solve_milp, c(args, list(solver = "highs")))
  g <- do.call(solve_milp, c(args, list(solver = "gurobi")))
  expect_equal(h$objective, g$objective)
  expect_identical(g$solver, "gurobi")
  expect_identical(g$status, "optimal")
  expect_equal(g$bound, g$objective)
  expect_equal(g$gap, 0)
})

test_that("gurobi handles sparse input, bounds, senses and warm starts", {
  skip_if_no_solver("gurobi")
  A <- Matrix::sparseMatrix(i = c(1, 1, 2), j = c(1, 2, 2), x = c(1, 1, 1))
  res <- solve_milp(obj = c(1, 5), A = A, sense = c(">=", "<="),
                    rhs = c(2, 1), types = c("I", "I"),
                    lower = c(0, 0), upper = c(5, 1),
                    start = c(2, 0), solver = "gurobi")
  expect_equal(res$objective, 2)
  expect_identical(res$status, "optimal")
})

test_that("ls_optimal solves end-to-end through gurobi", {
  skip_if_no_solver("gurobi")
  x <- data.frame(K1 = rep("1", 4), K2 = as.character(1:4))
  res <- ls_optimal(x, c("K1", "K2"), k = 2, solver = "gurobi")
  expect_equal(res$objective, 1)          # hub-family optimum
  expect_true(ls_check(res$xAnon, c("K1", "K2"), k = 2)$ok)
})
