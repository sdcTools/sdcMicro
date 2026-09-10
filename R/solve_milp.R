#' Solve a mixed-integer linear program
#'
#' Thin backend shim over the MILP solvers the ls_* engines can use. Every model in the
#' package is built in this one canonical form, so switching solver never
#' changes the model. See `docs/07-paper-P1-EJOR-plan.md` (work package A1).
#'
#' Backends: `"highs"` (HiGHS, MIT licence, CRAN binaries) is the default and
#' the only one intended to ship with sdcMicro; `"scip"` (Apache-2.0, builds
#' from source) is the opt-in backend for hard instances.
#'
#' @param obj Numeric objective coefficients, length `n`. Always **minimised**.
#' @param A Constraint matrix with `n` columns.
#' @param sense Row senses, `">="`, `"<="` or `"=="`; recycled to `nrow(A)`.
#' @param rhs Numeric right-hand side; recycled to `nrow(A)`.
#' @param types Variable types, `"C"` continuous, `"I"` integer, `"B"` binary;
#'   recycled to `n`. Binary variables default to bounds `[0, 1]`.
#' @param lower,upper Optional variable bounds, recycled to `n`. Defaults:
#'   `lower = 0`, and `upper = 1` for binary variables, `Inf` otherwise.
#' @param start Optional numeric warm-start solution of length `n` (an
#'   incumbent upper bound; `NA` entries are completed by the solver).
#'   Supported by the `highs` backend; ignored with a message by `scip`.
#' @param time_limit Solver time limit in seconds; `Inf` for none.
#' @param gap Relative MIP gap to stop at; `0` proves optimality.
#' @param solver Backend to use. `"gurobi"` requires the `gurobi` R package
#'   shipped with the Gurobi distribution (academic licence; used for the
#'   papers' exact reference runs only).
#' @return A list with `objective` (incumbent), `solution`, `bound` (best dual
#'   bound), `gap` (relative), `status`, `time` (seconds) and `solver`.
#'   `status` is `"optimal"`, `"infeasible"`, `"unbounded"`, `"time_limit"` or
#'   the backend's own lower-cased message.
#' @export
solve_milp <- function(obj, A, sense, rhs, types = "C",
                       lower = NULL, upper = NULL, start = NULL,
                       time_limit = Inf, gap = 0,
                       solver = c("highs", "scip", "gurobi")) {
  solver <- match.arg(solver)
  stopifnot(is.numeric(obj), length(obj) >= 1L, ncol(A) == length(obj))

  n     <- length(obj)
  types <- rep_len(types, n)
  sense <- rep_len(sense, nrow(A))
  rhs   <- rep_len(rhs, nrow(A))
  if (!all(sense %in% c(">=", "<=", "=="))) {
    stop("'sense' entries must be '>=', '<=' or '=='.")
  }
  if (!all(types %in% c("C", "I", "B"))) {
    stop("'types' entries must be 'C', 'I' or 'B'.")
  }

  lower <- if (is.null(lower)) rep(0, n) else rep_len(lower, n)
  upper <- if (is.null(upper)) ifelse(types == "B", 1, Inf) else rep_len(upper, n)
  is_mip <- any(types != "C")

  t0  <- proc.time()[["elapsed"]]
  out <- switch(
    solver,
    highs = milp_highs(obj, A, sense, rhs, types, lower, upper,
                       time_limit, gap, is_mip, start),
    scip  = milp_scip(obj, A, sense, rhs, types, lower, upper,
                      time_limit, gap, start),
    gurobi = milp_gurobi(obj, A, sense, rhs, types, lower, upper,
                         time_limit, gap, is_mip, start)
  )
  out$time   <- proc.time()[["elapsed"]] - t0
  out$solver <- solver
  out[c("objective", "solution", "bound", "gap", "status", "time", "solver")]
}

milp_highs <- function(obj, A, sense, rhs, types, lower, upper,
                       time_limit, gap, is_mip, start = NULL) {
  if (!requireNamespace("highs", quietly = TRUE)) {
    stop("Backend 'highs' requires the 'highs' package.")
  }
  res <- highs::highs_solve(
    L = obj, lower = lower, upper = upper, A = A,
    lhs = ifelse(sense %in% c(">=", "=="), rhs, -Inf),
    rhs = ifelse(sense %in% c("<=", "=="), rhs, Inf),
    types = ifelse(types == "C", "C", "I"), start = start,
    control = highs::highs_control(time_limit = time_limit, mip_rel_gap = gap)
  )
  status <- switch(
    res$status_message,
    "Optimal"          = "optimal",
    "Infeasible"       = "infeasible",
    "Unbounded"        = "unbounded",
    "Time limit reached" = "time_limit",
    tolower(res$status_message)
  )
  feasible <- status %in% c("optimal", "time_limit")
  list(
    objective = if (feasible) res$objective_value else NA_real_,
    solution  = if (feasible) res$primal_solution else rep(NA_real_, length(obj)),
    bound     = if (!feasible) NA_real_ else if (is_mip) res$info$mip_dual_bound
                else res$objective_value,
    gap       = if (!feasible) NA_real_ else if (is_mip) res$info$mip_gap else 0,
    status    = status
  )
}

milp_scip <- function(obj, A, sense, rhs, types, lower, upper,
                      time_limit, gap, start = NULL) {
  if (!is.null(start)) message("Backend 'scip' ignores 'start' (no warm-start API).")
  if (!requireNamespace("scip", quietly = TRUE)) {
    stop("Backend 'scip' requires the 'scip' package.")
  }
  ctrl <- scip::scip_control(
    verbose = FALSE,
    time_limit = if (is.finite(time_limit)) time_limit else 1e20,
    gap_limit = gap
  )
  # scip_solve() accepts dgCMatrix directly -- never densify: as.matrix() on
  # the hard-row models (200k x 500k) exceeded R's 32 Gb vector limit
  # (benchmarks/e-t2-lp-gap.txt, 2026-09-02).
  res <- utils::capture.output(
    sol <- scip::scip_solve(obj = obj, A = A, b = rhs, sense = sense,
                            vtype = types, lb = lower, ub = upper,
                            control = ctrl)
  )
  status   <- tolower(sol$status)
  feasible <- status %in% c("optimal", "timelimit", "time_limit", "gaplimit")
  if (status %in% c("timelimit", "gaplimit")) status <- "time_limit"
  list(
    objective = if (feasible) sol$objval else NA_real_,
    solution  = if (feasible) sol$x else rep(NA_real_, length(obj)),
    bound     = if (!feasible) NA_real_
                else scip_bound_from_gap(sol$objval, sol$gap),
    gap       = if (!feasible) NA_real_ else sol$gap,
    status    = status
  )
}

# SCIP reports gap = |primal - dual| / min(|primal|, |dual|) and exposes no
# dual bound, so invert the convention. Minimisation: dual <= primal. For
# primal > 0 the denominator is the (smaller) dual, for primal < 0 it is
# |primal|; an infinite gap carries no bound information.
scip_bound_from_gap <- function(objval, gap) {
  if (!is.finite(gap)) return(NA_real_)
  if (objval > 0) objval / (1 + gap)
  else if (objval < 0) objval * (1 + gap)
  else 0
}


milp_gurobi <- function(obj, A, sense, rhs, types, lower, upper,
                        time_limit, gap, is_mip, start = NULL) {
  if (!requireNamespace("gurobi", quietly = TRUE)) {
    stop("Backend 'gurobi' requires the 'gurobi' package ",
         "(shipped with the Gurobi distribution, not on CRAN).")
  }
  model <- list(
    A = A, obj = obj, modelsense = "min", rhs = rhs,
    sense = ifelse(sense == "==", "=", sense),   # gurobi: "<=", ">=", "="
    vtype = types, lb = lower, ub = upper
  )
  if (!is.null(start)) model$start <- start      # NA entries are completed
  params <- list(OutputFlag = 0, MIPGap = gap)
  if (is.finite(time_limit)) params$TimeLimit <- time_limit
  res <- gurobi::gurobi(model, params = params)
  status <- switch(res$status,
                   OPTIMAL = "optimal", TIME_LIMIT = "time_limit",
                   INFEASIBLE = "infeasible", UNBOUNDED = "unbounded",
                   tolower(res$status))
  feasible <- status %in% c("optimal", "time_limit") && !is.null(res$x)
  list(
    objective = if (feasible) res$objval else NA_real_,
    solution  = if (feasible) res$x else rep(NA_real_, length(obj)),
    bound     = if (!feasible) NA_real_
                else if (is_mip && !is.null(res$objbound)) res$objbound
                else res$objval,
    gap       = if (!feasible) NA_real_
                else if (is_mip && !is.null(res$mipgap)) res$mipgap else 0,
    status    = status
  )
}

#' MILP backends available in this session
#'
#' Which of the optional solver packages behind [solve_milp()] are installed,
#' in the order [localSuppression()] prefers them. `sdcApp` uses this to decide
#' whether the exact engines can be offered at all and to fill its solver
#' selector; `character(0)` means only the solver-free engines (`"heuristic"`
#' and `"greedy2"`) can run.
#'
#' @return Character vector, a subset of `c("highs", "scip", "gurobi")`.
#' @seealso [solve_milp()], [localSuppression()]
#' @export
#' @examples
#' ls_solvers()
ls_solvers <- function() {
  cand <- c("highs", "scip", "gurobi")
  cand[vapply(cand, requireNamespace, logical(1), quietly = TRUE)]
}
