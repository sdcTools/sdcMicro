#' Turn a protection criterion into per-record frequency thresholds
#'
#' Every *count-based* protection target says the same thing in different
#' words: record \eqn{i} may be released only once its frequency reaches
#' some threshold \eqn{k_i}. \eqn{k}-anonymity is the case where all
#' thresholds are equal; an individual-risk bound is the case where they are
#' not, because with survey weights the frequency needed to push a record's
#' risk below \eqn{\tau} depends on that record's weighted count. All solvers
#' in this package accept `k` either as a single number or as a vector of
#' per-record thresholds, so a new count-based criterion needs no new solver
#' -- only a rule for computing the vector, which is what this function is.
#'
#' Risk thresholds are **not** computed here from first principles: every
#' evaluation is delegated to [indivRisk()], so the numbers are by
#' construction the ones the deployed software would report --- but the key
#' columns are recoded to integer codes before the call, because
#' [freqCalc()] coerces them with `as.numeric()` and would otherwise
#' read character labels as missing, that is as wildcards. Without weights
#' the risk is \eqn{1/f} and the threshold is \eqn{\lceil 1/\tau \rceil}. With
#' weights the threshold is the smallest frequency at which `indivRisk()`
#' drops to \eqn{\tau} given the record's weighted count in the *original*
#' file. Since suppression can only raise weighted counts and the risk falls
#' in them, that threshold is **conservative**: enforcing it protects at
#' least as much as the criterion demands.
#'
#' Criteria that are not frequency thresholds -- \eqn{l}-diversity and SUDA
#' hitting sets -- are families of extra rows over the same variables rather
#' than a change of `k`; they are formulated in the paper and are not yet
#' implemented. `ls_target()` refuses them rather than approximating.
#'
#' @param x A data.frame of microdata.
#' @param keyVars Key variables, as column names or indices.
#' @param kanon Threshold for \eqn{k}-anonymity.
#' @param risk Upper bound \eqn{\tau} on each record's individual risk.
#' @param w Optional survey weight, as a column name or index. When given,
#'   risks follow the Benedetti--Franconi model as implemented by
#'   `sdcMicro`; when `NULL`, the population case \eqn{r = 1/f} applies.
#' @param ldiv,suda Not implemented; supplying them raises an informative
#'   error.
#' @return An integer vector of per-record thresholds, capped at `nrow(x)`,
#'   carrying a `"criterion"` attribute.
#' @seealso [ls_check()], [ls_optimal()], [ls_greedy2()], [ls_hub()], which
#'   all accept the result as their `k`.
#' @export
ls_target <- function(x, keyVars, kanon = NULL, risk = NULL, w = NULL,
                      ldiv = NULL, suda = NULL) {
  stopifnot(is.data.frame(x))
  n <- nrow(x)
  if (!is.null(ldiv) || !is.null(suda)) {
    stop("l-diversity and SUDA are constraint families over the model's ",
         "variables, not frequency thresholds; they are formulated in the ",
         "paper but not implemented. Use 'kanon' or 'risk'.")
  }
  if (is.null(kanon) == is.null(risk)) {
    stop("supply exactly one of 'kanon' or 'risk'.")
  }

  if (!is.null(kanon)) {
    stopifnot(length(kanon) == 1L, kanon >= 1L)
    out <- rep(as.integer(kanon), n)
    attr(out, "criterion") <- "kanon"
    return(out)
  }

  stopifnot(length(risk) == 1L, risk > 0, risk <= 1)
  if (is.null(w)) {
    out <- rep(min(as.integer(ceiling(1 / risk)), n), n)
    attr(out, "criterion") <- "risk (population, r = 1/f)"
    return(out)
  }

  # freqCalc() coerces key columns with as.numeric(), turning non-numeric
  # character labels into NA -- i.e. into wildcards -- and inflating the
  # weighted counts. Recode to integer codes first, exactly as ls_check() does:
  # the risk depends on the key only through per-column equality and NA-ness,
  # both invariant under injective recoding.
  xr <- as.data.frame(x)
  for (v in keyVars) xr[[v]] <- as.integer(factor(xr[[v]]))
  fc <- freqCalc(xr, keyVars = keyVars, w = w)
  Fk <- fc$Fk
  out <- integer(n)
  for (i in seq_len(n)) {
    out[i] <- ls_risk_threshold(Fk[i], risk, n, names(x)[1L])
  }
  attr(out, "criterion") <- paste0("risk <= ", risk, " (Benedetti-Franconi)")
  out
}

#' Smallest frequency at which sdcMicro's individual risk drops to tau
#' @keywords internal
#' @noRd
ls_risk_threshold <- function(Fk_i, tau, n, nm) {
  f <- seq_len(n)
  o <- list(freqCalc = data.frame(V = f), keyVars = "V",
            fk = f, Fk = rep(Fk_i, n))
  class(o) <- "freqCalc"
  rk <- indivRisk(o)$rk
  hit <- which(rk <= tau)
  if (!length(hit)) n else min(hit)
}
