#' Suppress one set of key columns with one engine
#'
#' The engine dispatcher behind `localSuppression(method = )`: key columns in,
#' key columns with added `NA` out, nothing else. Stratification, `combs`,
#' ghost variables and the suppression accounting live one level up in
#' `localSuppression()`, which is why this signature carries none of them --
#' `suppSubset()` calls this for every engine but the original sweep, so all
#' of that machinery serves every engine unchanged.
#'
#' @param x Data frame of key columns only.
#' @param k Required minimum frequency, length 1.
#' @param importance Optional cost vector, one per column, `1` = most
#'   important; `NULL` means unit costs. Note that the sweep's `importance` is
#'   a tie-break *order*, and `localSuppression()` only forwards it here when
#'   the user supplied one.
#' @param alpha Wildcard weight of [freqCalc()]. `"aggregate"` is exact for
#'   every `alpha`; the other engines are exact for `alpha = 1` only and refuse
#'   anything else.
#' @param method Engine: `"greedy2"` (cost-effectiveness greedy with exact
#'   per-record steps, no solver needed), `"optimal"` (pair-based MILP over the
#'   violator-only restriction), `"aggregate"` (tuple-indexed MILP, exact for
#'   the unrestricted problem) or `"lns"` (large-neighbourhood search around
#'   the pair-based model). The last three need a MILP backend, see
#'   [solve_milp()].
#' @param control Named list of engine arguments (for example `solver`,
#'   `time_limit`, `max_per_record`); entries the engine does not accept are
#'   dropped.
#' @return A list with `xAnon` (the suppressed frame, column types preserved)
#'   and `info` (engine diagnostics: `method`, `objective`, `nsupp`, `bound`,
#'   `gap`, `status`, `time`, `solver`).
#' @seealso [localSuppression()], which is where users reach this.
#' @export
ls_supp_subset <- function(x, k, importance = NULL, alpha = 1,
                           method = c("greedy2", "optimal", "aggregate", "lns"),
                           control = list()) {
  method <- match.arg(method)
  if (length(k) != 1L || k < 1) {
    stop("argument 'k' must be of length 1 and > 0.", call. = FALSE)
  }
  if (method != "aggregate" && !isTRUE(all.equal(alpha, 1))) {
    stop("method '", method, "' is exact for alpha = 1 only; use method = ",
         "'aggregate', which takes any alpha, or ls_check(alpha = ) to ",
         "evaluate a released file at another alpha.", call. = FALSE)
  }
  x <- as.data.frame(x)
  if (nrow(x) < k) {
    stop("k is larger or equal the group size (in at least one stratum).",
         call. = FALSE)
  }

  engine <- switch(method, greedy2 = ls_greedy2, optimal = ls_optimal,
                   aggregate = ls_aggregate, lns = ls_lns)
  args <- c(list(x = x, keyVars = names(x), k = k, importance = importance,
                 alpha = alpha), control)
  args <- args[names(args) %in% names(formals(engine))]
  res <- do.call(engine, args)
  ls_verify_release(res$xAnon, keyVars = names(x), k = k, alpha = alpha,
                    method = method)

  pick <- function(nm) if (is.null(res[[nm]])) NA_real_ else res[[nm]]
  list(
    xAnon = res$xAnon,
    info = list(method = method, objective = res$objective,
                nsupp = res$nsupp, bound = pick("bound"), gap = pick("gap"),
                status = if (is.null(res$status)) NA_character_ else res$status,
                time = pick("time"),
                solver = if (is.null(res$solver)) NA_character_ else res$solver)
  )
}

#  Internal: never hand on a file the model only claims is protected.
#  A solver that finds no incumbent within its time limit can return the input
#  unchanged (ls_aggregate does), and a cap can make an instance infeasible;
#  both would otherwise reach the caller as a silently unprotected release.
#  The check is freqCalc()'s own count, at the alpha the file will be judged at.
ls_verify_release <- function(x, keyVars, k, alpha = 1, method = "") {
  chk <- ls_check(x, keyVars = keyVars, k = k, alpha = alpha)
  if (!isTRUE(chk$ok)) {
    stop("method '", method, "' returned a file that does not meet the target: ",
         length(chk$violators), " record(s) still below k = ", k,
         ". No file is returned rather than a partially protected one.",
         call. = FALSE)
  }
  invisible(TRUE)
}

#  Internal: one line for the audit trail -- what the run achieved, not what it
#  was asked for. Silent for the original sweep, which claims nothing.
ls_method_note <- function(x) {
  if (is.null(x) || is.null(x$method) || identical(x$method, "heuristic")) {
    return(NULL)
  }
  num <- function(v, digits = 2) {
    if (is.null(v) || length(v) != 1L || !is.finite(v)) return("NA")
    format(round(v, digits), trim = TRUE, scientific = FALSE)
  }
  pick <- function(nm) if (is.null(x[[nm]])) NA else x[[nm]]
  status <- pick("status")

  claim <- if (length(status) != 1L || is.na(status)) {
    "heuristic, no bound on the minimum"
  } else if (identical(status, "optimal")) {
    paste0("proved optimal, objective ", num(pick("objective")))
  } else if (identical(status, "time_limit")) {
    paste0("stopped at the time limit, objective ", num(pick("objective")),
           ", best bound ", num(pick("bound")),
           ", remaining gap ", num(100 * pick("gap"), 1), "%")
  } else {
    paste0("solver status ", status)
  }

  tail <- character(0)
  solver <- pick("solver")
  if (length(solver) == 1L && !is.na(solver)) {
    tail <- c(tail, paste0("solver ", solver))
  }
  tm <- pick("time")
  if (length(tm) == 1L && is.finite(tm)) tail <- c(tail, paste0(num(tm, 1), " s"))
  tail <- if (length(tail) == 0L) "" else paste0(" (", paste(tail, collapse = ", "), ")")

  paste0("Engine ", x$method, ": ", claim, tail, ".")
}
