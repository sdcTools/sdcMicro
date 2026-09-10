#' Large-neighbourhood search on the exact suppression model
#'
#' The matheuristic of `docs/04-scaling-and-heuristics.md` section 2.4: keep
#' the incumbent suppression pattern, repeatedly *free* the cells of a random
#' neighbourhood of violators while fixing everything else, re-solve that
#' small MILP exactly, and accept the (never worse) result. Because every
#' restricted solve is warm-started with the incumbent, the trajectory is
#' monotone by construction; when the neighbourhood covers all violators the
#' restricted solve *is* the exact solve, and a closed solve then proves
#' optimality.
#'
#' Motivation, measured (`benchmarks/e1-modelB-hard-rows.txt`): on `testdata`
#' at 6 keys plain branch-and-bound stalls with large MIP gaps after 300 s,
#' and a mediocre warm start can even suppress primal progress. LNS attacks
#' exactly that primal side.
#'
#' @param x A data.frame of microdata.
#' @param keyVars Key variables, as column names or indices.
#' @param k Required minimum frequency.
#' @param alpha Wildcard weight of the deployed counter, as in
#'   [ls_check()]. The models here are exact for `alpha = 1` (the deployed
#'   default) only; other values are refused rather than silently ignored,
#'   because intermediate `alpha` gives fractional count coefficients.
#' @param importance As in [ls_optimal()].
#' @param neighborhood How many violators are freed per iteration. Values
#'   `>=` the number of violators make the first iteration the exact solve.
#' @param iterations Maximum LNS iterations.
#' @param per_solve_limit Time limit (seconds) for each restricted solve.
#' @param time_limit Overall wall-clock budget in seconds.
#' @param seed Optional seed for the neighbourhood sampling (local RNG).
#' @param solver Passed to [solve_milp()]; `"highs"` recommended.
#' @param max_per_record As in [ls_optimal()]: cap on suppressed cells per
#'   record; the search then only visits capped patterns.
#' @return As [ls_optimal()], plus `iterations` (restricted solves run),
#'   `trajectory` (incumbent objective after each iteration, starting with
#'   the initial incumbent) and `proven` (`TRUE` when a full-neighbourhood
#'   solve closed, so the result is the exact optimum). `bound`/`gap` are
#'   taken from the last full-model information available (`NA` when the
#'   model was never solved unrestricted).
#' @seealso [ls_optimal()] for the exact solve this search embeds.
#' @export
ls_lns <- function(x, keyVars, k = 2, alpha = 1, importance = NULL,
                   neighborhood = 20, iterations = 50,
                   per_solve_limit = 10, time_limit = 60,
                   seed = NULL, solver = c("highs", "scip", "gurobi"),
                   max_per_record = NULL) {
  solver <- match.arg(solver)
  stopifnot(is.data.frame(x), neighborhood >= 1L)
  if (!isTRUE(all.equal(alpha, 1))) {
    stop("the solvers are exact for alpha = 1 only; use ls_check(alpha = ) ",
         "to evaluate a released file at another alpha.")
  }

  mod <- ls_build_model(x, keyVars, k, importance, formulation = "modelA",
                        max_per_record = max_per_record)
  if (is.null(mod)) {
    return(list(xAnon = x, objective = 0, bound = 0, gap = 0,
                status = "optimal", time = 0, solver = solver,
                violators = integer(0), nsupp = 0L,
                dims = c(vars = 0L, cons = 0L, classes = NA_integer_),
                iterations = 0L, trajectory = numeric(0), proven = TRUE))
  }

  rng <- if (!is.null(seed)) {
    old_seed <- if (exists(".Random.seed", envir = globalenv()))
      get(".Random.seed", envir = globalenv()) else NULL
    on.exit(if (!is.null(old_seed))
      assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
    set.seed(seed)
  }

  t0 <- proc.time()[["elapsed"]]
  left <- function() time_limit - (proc.time()[["elapsed"]] - t0)

  ## ---- initial incumbent: one budgeted unrestricted solve ------------------
  sol <- solve_milp(obj = mod$obj, A = mod$A, sense = mod$sense,
                    rhs = mod$rhs, types = mod$types,
                    lower = mod$lower, upper = mod$upper,
                    time_limit = min(per_solve_limit, max(left(), 1)),
                    solver = solver)
  if (!sol$status %in% c("optimal", "time_limit") ||
      anyNA(sol$solution[seq_len(mod$n_x)])) {
    stop("LNS found no initial incumbent (status: ", sol$status, ").")
  }
  inc <- sol$solution
  inc_obj <- sol$objective
  bound <- sol$bound
  gap <- sol$gap
  trajectory <- inc_obj
  proven <- identical(sol$status, "optimal")
  iters <- 0L

  full_width <- neighborhood >= mod$nV
  while (!proven && iters < iterations && left() > 1) {
    iters <- iters + 1L
    free <- if (full_width) seq_len(mod$nV) else
      sort(sample.int(mod$nV, min(neighborhood, mod$nV)))
    fixed <- setdiff(seq_len(mod$nV), free)

    lo <- mod$lower
    up <- mod$upper
    for (a in fixed) {
      cols <- (a - 1L) * mod$p + seq_len(mod$p)
      val <- round(inc[cols])
      lo[cols] <- pmax(lo[cols], val)
      up[cols] <- pmin(up[cols], val)
    }

    s <- solve_milp(obj = mod$obj, A = mod$A, sense = mod$sense,
                    rhs = mod$rhs, types = mod$types,
                    lower = lo, upper = up, start = inc,
                    time_limit = min(per_solve_limit, max(left(), 1)),
                    solver = solver)
    if (s$status %in% c("optimal", "time_limit") &&
        !anyNA(s$solution[seq_len(mod$n_x)]) &&
        s$objective <= inc_obj + 1e-9) {
      inc <- s$solution
      inc_obj <- s$objective
      if (full_width) {
        bound <- s$bound
        gap <- s$gap
        proven <- identical(s$status, "optimal")
      }
    }
    trajectory <- c(trajectory, inc_obj)
  }

  fake_sol <- list(status = if (proven) "optimal" else "time_limit",
                   solution = inc)
  applied <- ls_apply_solution(x, keyVars, mod, fake_sol)
  list(xAnon = applied$x, objective = inc_obj,
       bound = if (proven) inc_obj else bound,
       gap = if (proven) 0 else gap,
       status = if (proven) "optimal" else "time_limit",
       time = proc.time()[["elapsed"]] - t0, solver = solver,
       violators = mod$V, nsupp = applied$nsupp,
       dims = c(vars = mod$nvar, cons = mod$ncons, classes = mod$m),
       iterations = iters, trajectory = trajectory, proven = proven)
}
