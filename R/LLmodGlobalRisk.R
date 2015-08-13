#' Global risk using log-linear models.
#'
#' The sample frequencies are assumed to be independent and following a Poisson
#' distribution. The parameters of the corresponding parameters are estimated
#' by a log-linear model including the main effects and possible interactions.
#'
#' This measure aims to (1) calculate the number of sample uniques that are
#' population uniques with a probabilistic Poisson model and (2) to estimate
#' the expected number of correct matches for sample uniques.
#'
#' ad 1) this risk measure is defined over all sample uniques (SU) as \deqn{
#' \tau_1 = \sum\limits_{SU} P(F_k=1 | f_k=1) \quad , } i.e. the expected
#' number of sample uniques that are population uniques.
#'
#' ad 2) this risk measure is defined over all sample uniques (SU) as \deqn{
#' \tau_2 = \sum\limits_{SU} P(F_k=1 | f_k=1) \quad , CORRECT! }
#'
#' Since population frequencies \eqn{F_k} are unknown, they has to be
#' estimated.
#'
#' The iterative proportional fitting method is used to fit the parameters of
#' the Poisson distributed frequency counts related to the model specified to
#' fit the frequency counts.  The obtained parameters are used to estimate a
#' global risk, defined in Skinner and Holmes (1998).
#'
#' @name LLmodGlobalRisk
#' @aliases LLmodGlobalRisk-methods LLmodGlobalRisk,ANY-method
#' LLmodGlobalRisk,data.frame-method LLmodGlobalRisk,matrix-method
#' LLmodGlobalRisk,sdcMicroObj-method LLmodGlobalRisk
#' @docType methods
#' @param obj An object of class sdcMicroObj or a numeric matrix or data frame
#' containing the categorical key variables.
#' @param method At this time, only iterative proportional fitting
#' (\dQuote{IPF}) can be used.
#' @param inclProb Inclusion probabilites (experimental)
#' @param form A formula specifying the model.
#' @param modOutput If TRUE, additional output is given.
#' @return Two global risk measures or the modified risk in the \code{\link{sdcMicroObj-class}} object.
#' @author Matthias Templ
#' @seealso \code{\link{loglm}}, \code{\link{measure_risk}}
#' @references Skinner, C.J. and Holmes, D.J. (1998) \emph{Estimating the
#' re-identification risk per record in microdata}. Journal of Official
#' Statistics, 14:361-372, 1998.
#'
#' Rinott, Y. and Shlomo, N. (1998). \emph{A Generalized Negative Binomial
#' Smoothing Model for Sample Disclosure Risk Estimation}. Privacy in
#' Statistical Databases. Lecture Notes in Computer Science.  Springer-Verlag,
#' 82--93.
#' @keywords manip
#' @note LLmodGlobalRisk is depcrecated for \code{\link{modRisk}} and is only
#' provided for compatibility with older versions of this package. It may be removed
#' in future versions.
#' @seealso \code{\link{modRisk}}
#' @export
setGeneric("LLmodGlobalRisk", function(obj, method = "IPF", inclProb = NULL, form = NULL, modOutput = FALSE) {
  standardGeneric("LLmodGlobalRisk")
})

setMethod(f = "LLmodGlobalRisk", signature = c("sdcMicroObj"),
definition = function(obj, method = "IPF", inclProb = NULL, form = NULL, modOutput = FALSE) {
  .Deprecated("modRisk")
  if (is.null(form)) {
    x <- get.sdcMicroObj(obj, type = "manipKeyVars")
    form <- as.formula(paste(" ~ ", paste(colnames(x), collapse = "+")))
  } else {
    vars <- labels(terms(form))
    mk <- get.sdcMicroObj(obj, type = "manipKeyVars")
    mn <- get.sdcMicroObj(obj, type = "manipNumVars")
    ok <- get.sdcMicroObj(obj, type = "origData")
    ok <- ok[, !colnames(ok) %in% c(colnames(mk), colnames(mn)), drop = FALSE]
    if (any(colnames(mk) %in% vars)) {
      x <- mk[, colnames(mk) %in% vars, drop = FALSE]
    } else x <- NULL
    if (any(colnames(mn) %in% vars)) {
      if (is.null(x))
        x <- mn[, colnames(mn) %in% vars, drop = FALSE] else x <- data.frame(x, mn[, colnames(mn) %in% vars, drop = FALSE])
    }
    if (any(colnames(ok) %in% vars)) {
      if (is.null(x))
        x <- ok[, colnames(ok) %in% vars, drop = FALSE] else x <- data.frame(x, ok[, colnames(ok) %in% vars, drop = FALSE])
    }
  }
  if (is.null(inclProb) && !is.null(get.sdcMicroObj(obj, type = "weightVar"))) {
    inclProb <- 1/get.sdcMicroObj(obj, type = "origData")[, get.sdcMicroObj(obj, type = "weightVar")]
  }
  risk <- get.sdcMicroObj(obj, type = "risk")
  risk$model <- LLmodGlobalRiskWORK(x = x, method = method, inclProb = inclProb, form = form, modOutput = modOutput)
  risk$model$inclProb <- inclProb
  obj <- set.sdcMicroObj(obj, type = "risk", input = list(risk))
  obj
})

setMethod(f = "LLmodGlobalRisk", signature = c("data.frame"),
definition = function(obj, method = "IPF", inclProb = NULL, form = NULL, modOutput = FALSE) {
  .Deprecated("modRisk")
  if (is.null(form)) {
    form <- as.formula(paste(" ~ ", paste(colnames(obj), collapse = "+")))
  }
  LLmodGlobalRiskWORK(x = obj, method = method, inclProb = inclProb, form = form, modOutput = modOutput)
})

setMethod(f = "LLmodGlobalRisk", signature = c("matrix"),
definition = function(obj, method = "IPF", inclProb = NULL, form = NULL, modOutput = FALSE) {
  .Deprecated("modRisk")
  if (is.null(form)) {
    form <- as.formula(paste(" ~ ", paste(colnames(obj), collapse = "+")))
  }
  LLmodGlobalRiskWORK(x = obj, method = method, inclProb = inclProb, form = form, modOutput = modOutput)
})

LLmodGlobalRiskWORK <- function(x, method = "IPF", inclProb = NULL, form = as.formula(paste(" ~ ",
  paste(colnames(x), collapse = "+"))), modOutput = FALSE) {

  if (is.null(inclProb)) {
    warn_sw <- paste0("Please provide the inclusion probabilities, eg.\n");
    warn_sw <- paste0(warn_sw, "approx by 1/sampling weights.\n")
    warn_sw <- paste0(warn_sw, "They are now set to 0.1 which is simple a wrong assumption.")
    warning(warn_sw)
    inclProb = 0.1
  }
  # x risk functions P(F_k=r | f_k = r)
  risk1 <- function(l, p) {
    v = (1 - p) * l
    exp(-v)
  }
  # E(1/F_k | f_k = 1)
  risk2 <- function(l, p) {
    v = (1 - p) * l
    (1 - exp(-v))/v
  }
  # file level risk measure
  file_risk <- function(freq, risk) {
    sum(as.numeric(freq == 1) * risk)
  }

  ## sample frequencies
  tab <- xtabs(form, x)
  x <- data.frame(x, inclProb = inclProb)
  form2 <- as.formula(paste(c("inclProb", as.character(form)), collapse = ""))
  tabP <- xtabs(form2, x)

  ## IPF
  mod <- loglm(form, data = tab, fitted = TRUE)
  lambda <- fitted(mod)

  ## Risk
  r1 <- risk1(lambda, tabP)
  r2 <- risk2(lambda, tabP)
  gr1 <- file_risk(tab, r1)/nrow(x)
  gr2 <- file_risk(tab, r2)/nrow(x)
  if (modOutput) {
    res <- list(gr1 = gr1, gr2 = gr2, gr1perc = gr1 * 100, gr2perc = gr2 * 100, tab = tab, fitted = lambda)
  } else {
    res <- list(gr1 = gr1, gr2 = gr2, gr1perc = gr1 * 100, gr2perc = gr2 * 100)
  }
  res
}
