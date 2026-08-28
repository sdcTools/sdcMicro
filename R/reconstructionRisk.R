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
#'     intruder, interpolating between the bounds by a reconstruction weight. In
#'     scenario B (argument \code{original} supplied, e.g. for an \code{sdcMicroObj})
#'     the weight is the \emph{per-record} predictive probability of the record's true
#'     value under \code{model}; otherwise (scenario A) it is the average per-key
#'     \code{accuracy}. For a record \code{i},
#'     \code{risk = max(weight_i * risk_upper_i, risk_lower_i)}.}
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
#' @param x a \code{data.frame}, \code{matrix}, or \code{\linkS4class{sdcMicroObj}}
#'   object. For an \code{sdcMicroObj} the (possibly suppressed) manipulated key
#'   variables and the survey weight are used; key columns may contain \code{NA}.
#' @param keyVars column names or indices of the categorical key variables. Ignored
#'   (taken from the object) when \code{x} is an \code{sdcMicroObj}.
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
#'   Defaults (\code{NULL}) to \code{TRUE} when a weight is available.
#' @param method \code{"approx"} (default) or \code{"exact"}, passed to
#'   \code{\link{indivRisk}}.
#' @param original optional original (pre-suppression) values of the key variables,
#'   one column per key variable, same rows as \code{x} and without missing values
#'   (scenario B, where the agency knows the suppressed truth). When supplied, the
#'   conservative bound is computed on the true cells and the reconstruction weight is
#'   each record's predictive probability of its true value. For an \code{sdcMicroObj}
#'   the original key variables are used automatically.
#' @param model predictive model for the per-record reconstruction probability in
#'   scenario B (ignored without \code{original}). For each missing key of a record the
#'   probability of the record's true value is evaluated under \code{"conditional"} (the
#'   empirical distribution of that key given the record's other keys, estimated from the
#'   complete data), \code{"marginal"} (the empirical marginal of that key), or
#'   \code{"envelope"} (the default: the maximum of the two, i.e. the hit probability of
#'   the better of the two reconstruction strategies); the per-key probabilities are
#'   multiplied over the record's missing keys. The own-cell conditional alone degenerates
#'   to the optimistic bound in a population (its probability is the ratio of the true-cell
#'   to the observed-key-cell frequency), which is why the envelope is the default.
#'
#' @return An object of class \code{"reconstructionRisk"}: a list with per-record
#'   vectors \code{risk}, \code{risk_lower}, \code{risk_upper}, \code{reconstruction_prob}
#'   (the per-record reconstruction weight), \code{fk}, \code{Fk}, the per-key
#'   \code{accuracy}, the \code{scenario}, the \code{model} used for the reconstruction
#'   weight (\code{NA} in scenario A), the number of records with at least one missing key
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
#'
#' ## sdcMicroObj method: uses the suppressed manipulated keys and the weight
#' \donttest{
#' data(testdata)
#' sdc <- createSdcObj(testdata,
#'   keyVars = c("urbrur", "roof", "walls", "water", "sex"),
#'   w = "sampling_weight")
#' sdc <- localSuppression(sdc)
#' reconstructionRisk(sdc)
#' }
reconstructionRisk <- function(x, keyVars = NULL, w = NULL, accuracy = NULL,
                               survey = NULL, method = "approx", original = NULL,
                               model = c("envelope", "conditional", "marginal")) {
  model <- match.arg(model)
  ## sdcMicroObj method: use the (possibly suppressed) manipulated key variables.
  ## The agency knows the suppressed truth (scenario B), so the original key values
  ## are passed on for the per-record reconstruction probability and the true cells.
  if (inherits(x, "sdcMicroObj")) {
    if (!is.null(keyVars))
      warning("'keyVars' is ignored for 'sdcMicroObj' input; the object's key variables are used")
    manip <- get.sdcMicroObj(x, type = "manipKeyVars")
    kvIdx <- get.sdcMicroObj(x, type = "keyVars")
    orig_keys <- get.sdcMicroObj(x, type = "origData")[, kvIdx, drop = FALSE]
    wv <- get.sdcMicroObj(x, type = "weightVar")
    dat <- manip
    ww <- NULL
    if (length(wv) > 0) {
      dat[[".reconstructionWeight"]] <- get.sdcMicroObj(x, type = "origData")[, wv]
      ww <- ncol(dat)
    }
    if (is.null(survey)) survey <- length(wv) > 0
    return(reconstructionRisk(dat, keyVars = seq_len(ncol(manip)), w = ww,
                              accuracy = accuracy, survey = survey, method = method,
                              original = orig_keys, model = model))
  }
  if (is.matrix(x)) x <- as.data.frame(x)
  if (!is.data.frame(x)) stop("'x' must be a 'data.frame', 'matrix' or 'sdcMicroObj'")
  if (nrow(x) == 0L) stop("'x' has no rows")
  if (is.null(keyVars)) stop("'keyVars' must be supplied for 'data.frame' input")
  if (is.null(survey)) survey <- !is.null(w)

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

  ## 3. conservative bound (perfect reconstruction) and 4. reference risk.
  ## With 'original' (scenario B: the agency knows the suppressed truth) the
  ## conservative bound uses the TRUE cells and the reconstruction weight is the
  ## per-record predictive probability of the record's true value, pi_i(c_i).
  ## Otherwise (scenario A: the truth is unknown) modal imputation and the average
  ## per-key 'accuracy' are used as an approximation.
  if (!is.null(original)) {
    original <- as.data.frame(original)
    if (ncol(original) != nkey || nrow(original) != nrow(x))
      stop("'original' must have one column per key variable and the same rows as 'x'")
    x_true <- x; x_true[, kidx] <- original
    fc_hi <- freqCalc(x_true, keyVars = kidx, w = w, alpha = 1)
    rk_hi <- indivRisk(fc_hi, method = method, survey = survey)$rk
    rec_prob <- .recon_prob_record(original, miss_mat, model)
  } else {
    x_imp <- x
    x_imp[, kidx] <- .impute_modal(K)
    fc_hi <- freqCalc(x_imp, keyVars = kidx, w = w, alpha = 1)
    rk_hi <- indivRisk(fc_hi, method = method, survey = survey)$rk
    rec_prob <- rep(1, nrow(x))
    if (any(has_na)) {
      rec_prob[has_na] <- vapply(which(has_na),
        function(i) prod(acc[miss_mat[i, ]]), numeric(1))
    }
  }
  rk_ref <- pmax(rec_prob * rk_hi, rk_lo)

  res <- list(
    risk = rk_ref, risk_lower = rk_lo, risk_upper = rk_hi,
    fk = fc_lo$fk, Fk = fc_lo$Fk,
    accuracy = acc, reconstruction_prob = rec_prob,
    scenario = if (is.null(original)) "A (modal imputation)" else "B (known truth)",
    model = if (is.null(original)) NA_character_ else model,
    n_missing = sum(has_na), N = nrow(x),
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

## per-key predictive probability of each record's TRUE value (scenario B), as an
## n x nkey matrix with 1 where the key is observed:
##   "conditional": P(X_j = c_ij | the record's other keys at their true values),
##                  the empirical conditional estimated from the complete data;
##   "marginal":    P(X_j = c_ij), the empirical marginal of key j.
## A record whose suppressed value is rare (in its cell, or in the file) gets a low
## probability: it is hard to reconstruct.
.recon_prob_matrix <- function(orig, miss_mat, member) {
  n <- nrow(orig); nkey <- ncol(orig)
  P <- matrix(1, n, nkey)
  for (j in seq_len(nkey)) {
    mi <- which(miss_mat[, j])
    if (!length(mi)) next
    yj <- as.character(orig[[j]])
    if (member == "marginal") {
      pm <- table(yj) / n
      P[mi, j] <- as.numeric(pm[yj[mi]])
    } else {
      others <- setdiff(seq_len(nkey), j)
      so <- if (length(others)) .keysig(orig[others]) else rep("", n)
      tab <- table(so, yj)
      csz <- rowSums(tab)
      P[mi, j] <- mapply(function(s, v) tab[s, v] / csz[s], so[mi], yj[mi])
    }
  }
  P
}

## per-record reconstruction probability (scenario B): the product over a record's
## missing keys of the per-key probability under 'model'; "envelope" takes, per key,
## the maximum over the library {conditional, marginal} -- the hit probability of the
## better of the two reconstruction strategies.
## Note: for records with several missing keys each conditional factor conditions on
## ALL other keys at their true values (a product of full conditionals). This
## coincides with the exact chain-rule probability when one key is missing; with
## several missing keys the direction of the discrepancy depends on their dependence
## given the observed keys and is not uniformly conservative.
.recon_prob_record <- function(orig, miss_mat, model = "envelope") {
  orig <- as.data.frame(orig)
  P <- switch(model,
    conditional = .recon_prob_matrix(orig, miss_mat, "conditional"),
    marginal    = .recon_prob_matrix(orig, miss_mat, "marginal"),
    envelope    = pmax(.recon_prob_matrix(orig, miss_mat, "conditional"),
                       .recon_prob_matrix(orig, miss_mat, "marginal")),
    stop("unknown 'model'"))
  unname(apply(P, 1, prod))
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
  cat("scenario:", x$scenario,
      if (!is.na(x$model)) paste0("| reconstruction model: ", x$model) else "", "\n")
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
