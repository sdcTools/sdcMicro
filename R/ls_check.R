#' Check a protection target on (possibly suppressed) microdata
#'
#' The referee for every local-suppression engine in this package. A solver's own count of what it
#' achieved is never trusted: results are verified here, and the wildcard path
#' *is* [freqCalc()], so agreement with sdcMicro holds by
#' construction rather than by coincidence.
#'
#' Two semantics, both needed (see `docs/03-formulations.md` section 1):
#' \describe{
#'   \item{`"wildcard"` (S1)}{What `freqCalc()` counts: a suppressed
#'     cell (`NA`) matches every category, so `fk[i]` is the number of records
#'     *compatible* with record `i`. This is the criterion `kAnon()`
#'     enforces today. Compatibility is reflexive and symmetric but **not**
#'     transitive, so it induces no equivalence classes.}
#'   \item{`"identical"` (S2)}{Classical k-anonymity: records sharing the same
#'     released tuple (with `NA` treated as a value) form an equivalence class,
#'     and every class must have at least `k` members. Strictly stronger, and
#'     in general strictly more expensive, than S1.}
#' }
#'
#' @param x A data.frame of microdata.
#' @param keyVars Key variables (quasi-identifiers), as column names or indices.
#' @param k Required minimum frequency: a single threshold, or one per
#'   record (a vector of length `nrow(x)`), as produced by [ls_target()] for
#'   criteria such as an individual-risk bound.
#' @param alpha How much a record carrying `NA`s contributes to other
#'   records' counts, exactly as in [freqCalc()]: `alpha = 1`
#'   (the default, and the deployed default) is full wildcard matching,
#'   `alpha = 0` lets missing values form their own category. The criterion
#'   the software enforces is therefore a one-parameter family, and `alpha`
#'   prices the hub constructions of [ls_hub()] directly -- a solution that
#'   is feasible at `alpha = 1` need not be at `alpha < 1`. The solvers in
#'   this package are exact for `alpha = 1` only.
#' @param semantics Which criterion to check; see Details.
#' @param strataVars Optional stratum variables (names or indices). sdcMicro's
#'   semantics is strictly per stratum: the criterion is evaluated inside each
#'   stratum separately, exactly as `kAnon(strataVars = )` does.
#' @return A list with `ok` (logical, target met everywhere), `fk` (per-record
#'   frequency under the chosen semantics, within stratum if stratified),
#'   `violators` (integer row indices with `fk < k`), `k` and `semantics`.
#' @export
ls_check <- function(x, keyVars, k = 2, alpha = 1,
                     semantics = c("wildcard", "identical"),
                     strataVars = NULL) {
  semantics <- match.arg(semantics)
  stopifnot(is.data.frame(x), length(keyVars) >= 1L, all(k >= 1L),
            length(alpha) == 1L, alpha >= 0, alpha <= 1)
  if (length(k) != 1L && length(k) != nrow(x)) {
    stop("'k' must be a single threshold or one per record (see ls_target()).")
  }

  if (!is.null(strataVars)) {
    grp <- interaction(x[, strataVars, drop = FALSE], drop = TRUE)
    fk <- numeric(nrow(x))
    for (g in levels(grp)) {
      idx <- which(grp == g)
      fk[idx] <- ls_check(x[idx, , drop = FALSE], keyVars = keyVars,
                          k = if (length(k) == 1L) k else k[idx],
                          alpha = alpha, semantics = semantics)$fk
    }
    violators <- which(fk < k)
    return(list(ok = length(violators) == 0L, fk = fk, violators = violators,
                k = k, alpha = alpha, semantics = semantics))
  }

  fk <- switch(
    semantics,
    wildcard  = fk_wildcard(x, keyVars, alpha),
    identical = fk_identical(x, keyVars)
  )

  violators <- which(fk < k)
  list(
    ok        = length(violators) == 0L,
    fk        = fk,
    violators = violators,
    k         = k,
    semantics = semantics
  )
}

# S1: delegate to sdcMicro so the criterion is theirs, not a reimplementation.
# freqCalc() coerces key columns with as.numeric(), which silently turns
# non-numeric character labels into NA -- i.e. into wildcards. Recode every
# key column to integer codes first: the criterion only tests per-column
# equality and NA-ness, both invariant under injective recoding, so fk is
# unchanged and the algorithm remains freqCalc's.
fk_wildcard <- function(x, keyVars, alpha = 1) {
  x <- as.data.frame(x)
  for (v in keyVars) x[[v]] <- as.integer(factor(x[[v]]))
  as.numeric(freqCalc(x, keyVars = keyVars, alpha = alpha)$fk)
}

# S2: class size of the released tuple, NA treated as an ordinary value.
# data.table groups on the column values themselves, so there is no string
# concatenation and therefore no chance of ("1","23") colliding with ("12","3").
fk_identical <- function(x, keyVars) {
  key <- as.data.frame(x[, keyVars, drop = FALSE])
  key[] <- lapply(key, as.character)
  dt <- data.table::as.data.table(key)
  dt[, ".ls_row" := .I]
  cols <- names(key)
  dt[, ".ls_n" := .N, by = cols]
  as.numeric(dt[order(dt[[".ls_row"]])][[".ls_n"]])
}
