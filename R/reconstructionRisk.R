#' Disclosure risk for key variables containing missing values
#'
#' Estimates individual re-identification risk when one or more categorical key
#' variables contain missing values (\code{NA}) - for example suppressed cells
#' produced by \code{\link{localSuppression}}, or genuine item non-response.
#' A missing value in a key is compatible with every category of that key, so the
#' standard frequency count is ambiguous. \code{reconstructionRisk} models an
#' intruder who tries to \emph{reconstruct} the missing cells from the remaining
#' key variables and reports three quantities per record:
#'
#' \describe{
#'   \item{\code{risk_lower}}{the \emph{optimistic} bound - a \dQuote{marginalising}
#'     intruder who treats a missing value as genuine ambiguity and never
#'     reconstructs it. This equals the current \code{\link{freqCalc}} /
#'     \code{\link{indivRisk}} behaviour with \code{alpha = 1}.}
#'   \item{\code{risk_upper}}{the \emph{conservative} bound - a perfect-reconstruction
#'     intruder who recovers every missing cell, obtained by modal imputation of the
#'     key variables (the released file is returned to its un-suppressed equivalence
#'     classes).}
#'   \item{\code{risk}}{the \emph{reference} risk of a realistic reconstruct-and-attack
#'     intruder, interpolating between the bounds by the per-key reconstruction
#'     accuracy \code{accuracy} (the diagonal of the implied misclassification
#'     matrix). For a record with missing keys \eqn{M}, \code{risk = max(prod(accuracy[M])
#'     * risk_upper, risk_lower)}.}
#' }
#'
#' The construction follows the misclassification-risk framework of Shlomo and
#' Skinner (2010), with a missing value treated as the limiting case of maximal
#' classification uncertainty. By construction \code{risk_lower <= risk <= risk_upper}.
#'
#' Unlike the \code{alpha} argument of \code{\link{freqCalc}} - which leaves the risk
#' of the suppressed records themselves unchanged and only re-weights neighbouring
#' complete records - this function assigns a missing-aware risk to the suppressed
#' records directly.
#'
#' @param x a \code{data.frame} (or \code{matrix}) holding the key variables; key
#'   columns may contain \code{NA}.
#' @param keyVars column names or indices of the categorical key variables.
#' @param w optional column name or index of a sampling-weight variable, passed to
#'   \code{\link{freqCalc}} for the population frequency estimate \code{Fk}.
#' @param accuracy per-key reconstruction accuracy of the intruder (the probability
#'   the intruder correctly recovers a missing value of that key from the others),
#'   a number in \eqn{[0, 1]}. Either \code{NULL} (the default - estimated from the
#'   data by cell-wise modal prediction of each key from the others, i.e. the
#'   strongest resubstitution reconstructor), a single value used for all keys, or a
#'   numeric vector of length \code{length(keyVars)}.
#' @param survey \code{TRUE} for survey data (Benedetti-Franconi individual risk via
#'   \code{\link{indivRisk}}), \code{FALSE} for a population (risk \code{= 1 / fk}).
#'   Defaults to \code{TRUE} when \code{w} is supplied.
#' @param method \code{"approx"} (default) or \code{"exact"}, passed to
#'   \code{\link{indivRisk}}.
#'
#' @return An object of class \code{"reconstructionRisk"}: a list with per-record
#'   vectors \code{risk}, \code{risk_lower}, \code{risk_upper}, \code{fk}, \code{Fk},
#'   the \code{accuracy} used, the number of records with at least one missing key
#'   \code{n_missing}, and bookkeeping (\code{knames}, \code{method}, \code{survey}).
#'
#' @references
#' Shlomo, N. and Skinner, C. (2010). Assessing the protection provided by
#' misclassification-based disclosure limitation methods for survey microdata.
#' \emph{The Annals of Applied Statistics}, \strong{4} (3), 1291--1310.
#' \doi{10.1214/09-AOAS317}
#'
#' Skinner, C. J. and Elliot, M. J. (2002). A measure of disclosure risk for
#' microdata. \emph{Journal of the Royal Statistical Society: Series B}, \strong{64}
#' (4), 855--867. \doi{10.1111/1467-9868.00365}
#'
#' @seealso \code{\link{freqCalc}}, \code{\link{indivRisk}}, \code{\link{measure_risk}},
#'   \code{\link{localSuppression}}
#' @author Matthias Templ, Bernhard Meindl
#' @keywords manip
#' @export
#' @examples
#' set.seed(123)
#' n <- 200
#' dat <- data.frame(
#'   age  = sample(1:5, n, replace = TRUE),
#'   sex  = sample(1:2, n, replace = TRUE),
#'   reg  = sample(1:4, n, replace = TRUE),
#'   occ  = sample(1:6, n, replace = TRUE)
#' )
#' ## suppress a few cells in 'occ'
#' dat$occ[sample(n, 20)] <- NA
#' rr <- reconstructionRisk(dat, keyVars = c("age", "sex", "reg", "occ"),
#'                          survey = FALSE)
#' rr
#' ## bounds bracket the reference risk by construction:
#' stopifnot(all(rr$risk_lower <= rr$risk + 1e-9),
#'           all(rr$risk <= rr$risk_upper + 1e-9))
reconstructionRisk <- function(x, keyVars, w = NULL, accuracy = NULL,
                               survey = !is.null(w), method = "approx") {
  if (is.matrix(x)) x <- as.data.frame(x)
  if (!is.data.frame(x)) stop("'x' must be a data.frame or matrix")
  if (nrow(x) == 0L) stop("'x' has no rows")

  ## resolve key-variable names / indices
  if (is.character(keyVars)) {
    if (!all(keyVars %in% names(x))) stop("not all 'keyVars' are columns of 'x'")
    kidx <- match(keyVars, names(x))
  } else {
    kidx <- as.integer(keyVars)
    if (!all(kidx %in% seq_len(ncol(x)))) stop("'keyVars' indices out of range")
  }
  knames <- names(x)[kidx]
  K <- x[, kidx, drop = FALSE]
  nkey <- length(kidx)
  miss_mat <- is.na(K)
  has_na <- rowSums(miss_mat) > 0L

  ## 1. per-key reconstruction accuracy (intruder's chance of recovering a missing cell)
  if (is.null(accuracy)) {
    acc <- vapply(seq_len(nkey), function(j) .reconstr_acc(K, j), numeric(1))
  } else if (length(accuracy) == 1L) {
    acc <- rep(as.numeric(accuracy), nkey)
  } else {
    if (length(accuracy) != nkey)
      stop("'accuracy' must be NULL, a single value, or one value per key variable")
    acc <- as.numeric(accuracy)
  }
  if (any(acc < 0 | acc > 1)) stop("'accuracy' must lie in [0, 1]")
  names(acc) <- knames

  ## 2. optimistic bound (marginalise): wildcard counts on the released data
  fc_lo <- freqCalc(x, keyVars = kidx, w = w, alpha = 1)
  rk_lo <- indivRisk(fc_lo, method = method, survey = survey)$rk

  ## 3. conservative bound (perfect reconstruction): modal-impute the keys, then count
  x_imp <- x
  x_imp[, kidx] <- .impute_modal(K)
  fc_hi <- freqCalc(x_imp, keyVars = kidx, w = w, alpha = 1)
  rk_hi <- indivRisk(fc_hi, method = method, survey = survey)$rk

  ## 4. reference risk: reconstruct-and-attack, scaled by per-record reconstruction success
  prod_acc <- rep(1, nrow(x))
  if (any(has_na)) {
    prod_acc[has_na] <- vapply(which(has_na),
      function(i) prod(acc[miss_mat[i, ]]), numeric(1))
  }
  rk_ref <- pmax(prod_acc * rk_hi, rk_lo)

  res <- list(
    risk = rk_ref, risk_lower = rk_lo, risk_upper = rk_hi,
    fk = fc_lo$fk, Fk = fc_lo$Fk,
    accuracy = acc, n_missing = sum(has_na), N = nrow(x),
    knames = knames, method = method, survey = survey, call = match.call()
  )
  class(res) <- "reconstructionRisk"
  res
}

## modal prediction accuracy of key 'j' from the other keys (resubstitution)
.reconstr_acc <- function(K, j) {
  y <- as.character(K[[j]])
  obs <- !is.na(y)
  if (!any(obs)) return(0)
  sig <- .keysig(K[, -j, drop = FALSE])
  modes <- tapply(y[obs], sig[obs], function(v) names(sort(table(v), decreasing = TRUE))[1L])
  pred <- modes[sig[obs]]
  mean(pred == y[obs], na.rm = TRUE)
}

## modal imputation of every NA in the key columns, cell-wise on the other keys
.impute_modal <- function(K) {
  for (j in seq_len(ncol(K))) {
    y <- K[[j]]; na <- is.na(y)
    if (!any(na)) next
    yc <- as.character(y)
    sig <- .keysig(K[, -j, drop = FALSE])
    modes <- tapply(yc[!na], sig[!na], function(v) names(sort(table(v), decreasing = TRUE))[1L])
    gmode <- names(sort(table(yc[!na]), decreasing = TRUE))[1L]
    fill <- modes[sig[na]]
    fill[is.na(fill)] <- gmode
    yc[na] <- fill
    K[[j]] <- if (is.factor(y)) factor(yc, levels = levels(y))
              else if (is.numeric(y)) as.numeric(yc) else yc
  }
  K
}

## row signature of a set of key columns (NA -> ".NA.")
.keysig <- function(M) {
  M <- as.data.frame(M)
  if (ncol(M) == 0L) return(rep("", nrow(M)))
  do.call(paste, c(lapply(M, function(z) {
    z <- as.character(z); z[is.na(z)] <- ".NA."; z
  }), sep = "\r"))
}

#' @rdname reconstructionRisk
#' @param object an object of class \code{"reconstructionRisk"}.
#' @param \dots further arguments (currently ignored).
#' @method print reconstructionRisk
#' @export
print.reconstructionRisk <- function(x, ...) {
  hi <- sum(x$risk > 0.1)
  cat("Reconstruction risk for", x$N, "records (",
      x$n_missing, "with >=1 missing key )\n")
  cat("key variables:", paste(x$knames, collapse = ", "), "\n")
  cat("reconstruction accuracy:",
      paste(sprintf("%s=%.2f", x$knames, x$accuracy), collapse = "  "), "\n")
  cat(sprintf("mean risk: lower=%.4f  reference=%.4f  upper=%.4f\n",
              mean(x$risk_lower), mean(x$risk), mean(x$risk_upper)))
  cat(sprintf("%d record(s) with reference risk > 0.1\n", hi))
  invisible(x)
}

#' @rdname reconstructionRisk
#' @method summary reconstructionRisk
#' @export
summary.reconstructionRisk <- function(object, ...) {
  out <- list(
    risk = summary(object$risk),
    risk_lower = summary(object$risk_lower),
    risk_upper = summary(object$risk_upper),
    accuracy = object$accuracy, n_missing = object$n_missing, N = object$N
  )
  class(out) <- "summary.reconstructionRisk"
  out
}

#' @rdname reconstructionRisk
#' @method print summary.reconstructionRisk
#' @export
print.summary.reconstructionRisk <- function(x, ...) {
  cat("Reconstruction risk -", x$N, "records,", x$n_missing, "with missing keys\n\n")
  m <- rbind(lower = x$risk_lower, reference = x$risk, upper = x$risk_upper)
  print(round(m, 4))
  cat("\nreconstruction accuracy per key:\n")
  print(round(x$accuracy, 3))
  invisible(x)
}
