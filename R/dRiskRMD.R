#' RMD based disclosure risk
#'
#' Distance-based disclosure risk estimation via robust Mahalanobis Distances.
#'
#' This method is an extension of method SDID because it accounts for the
#' \dQuote{outlyingness} of each observations. This is a quite natural approach
#' since outliers do have a higher risk of re-identification and therefore
#' these outliers should have larger disclosure risk intervals as observations
#' in the center of the data cloud.
#'
#' The algorithm works as follows:
#'
#' 1. Robust Mahalanobis distances are estimated in order to get a robust
#' multivariate distance for each observation.
#'
#' 2. Intervals are estimated for each observation around every data point of
#' the original data points where the length of the interval is
#' defined/weighted by the squared robust Mahalanobis distance and the
#' parameter $k$.  The higher the RMD of an observation the larger the
#' interval.
#'
#' 3. Check if the corresponding masked values fall into the intervals around
#' the original values or not.  If the value of the corresponding observation
#' is within such an interval the whole observation is considered unsafe.  So,
#' we get a whole vector indicating which observation is save or not, and we
#' are finished already when using method RMDID1).
#'
#' 4. For method RMDID1w: we return the weighted (via RMD) vector of disclosure
#' risk.
#'
#' 5. For method RMDID2: whenever an observation is considered unsafe it is
#' checked if $m$ other observations from the masked data are very close
#' (defined by a parameter $k2$ for the length of the intervals as for SDID or
#' RSDID) to such an unsafe observation from the masked data, using Euclidean
#' distances.  If more than $m$ points are in such a small interval, we
#' conclude that this observation is ``save''.
#'
#' @name dRiskRMD
#' @aliases dRiskRMD-methods dRiskRMD,data.frame-method dRiskRMD,matrix-method
#' dRiskRMD,sdcMicroObj-method dRiskRMD
#' @docType methods
#' @param obj original data or object of class \code{\link{sdcMicroObj-class}}
#' @param ... see possible arguments below
#' \itemize{
#' \item{}{xm masked data}
#' \item{k}{weight for adjusting the influence of the robust Mahalanobis
#' distances, i.e. to increase or decrease each of the disclosure risk intervals.}
#' \item{k2}{parameter for method RMDID2 to choose a small interval around each
#' masked observation.}}
#' @return The disclosure risk or the modified \code{\link{sdcMicroObj-class}}
#' \item{risk1}{percentage of sensitive observations according to method RMDID1.}
#' \item{risk2}{standardized version of risk1}
#' \item{wrisk1}{amount of sensitive observations according to RMDID1 weighted
#' by their corresponding robust Mahalanobis distances.}
#' \item{wrisk2}{RMDID2 measure}
#' \item{indexRisk1}{index of observations with high risk according to risk1 measure}
#' \item{indexRisk2}{index of observations with high risk according to wrisk2 measure}
#' @section Methods: \describe{
#' \item{list("signature(obj = \"data.frame\")")}{}
#' \item{list("signature(obj = \"matrix\")")}{}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @author Matthias Templ
#' @seealso \code{\link{dRisk}}
#' @references Templ, M. and Meindl, B., \emph{Robust Statistics Meets SDC: New
#' Disclosure Risk Measures for Continuous Microdata Masking}, Lecture Notes in
#' Computer Science, Privacy in Statistical Databases, vol. 5262, pp. 113-126,
#' 2008.
#'
#' Templ, M. \emph{New Developments in Statistical Disclosure Control and
#' Imputation: Robust Statistics Applied to Official Statistics},
#' Suedwestdeutscher Verlag fuer Hochschulschriften, 2009, ISBN: 3838108280,
#' 264 pages.
#' @keywords manip
#' @export
#' @examples
#'
#' data(Tarragona)
#' x <- Tarragona[, 5:7]
#' y <- addNoise(x)$xm
#' dRiskRMD(x, xm=y)
#' dRisk(x, xm=y)
#'
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' ## this is already made internally:
#' ## sdc <- dRiskRMD(sdc)
#' ## and already stored in sdc
#'
setGeneric("dRiskRMD", function(obj, ...) {
  standardGeneric("dRiskRMD")
})

setMethod(f = "dRiskRMD", signature = c("sdcMicroObj"),
definition = function(obj, ...) {
  numVars <- get.sdcMicroObj(obj, type = "numVars")
  x <- get.sdcMicroObj(obj, type = "origData")[, numVars, drop = F]
  xm <- get.sdcMicroObj(obj, type = "manipNumVars")
  risk <- get.sdcMicroObj(obj, type = "risk")
  optionss <- get.sdcMicroObj(obj, type = "options")
  if ("risk_k" %in% names(optionss)) {
    risk$numericRMD <- dRiskRMDWORK(x = x, xm = xm, k = optionss$risk_k, k2 = optionss$risk_k2,
      ...)
  } else risk$numericRMD <- dRiskRMDWORK(x = x, xm = xm, ...)
  obj <- set.sdcMicroObj(obj, type = "risk", input = list(risk))
  obj
})

setMethod(f = "dRiskRMD", signature = c("data.frame"),
definition = function(obj, ...) {
  dRiskRMDWORK(x = obj, ...)
})
setMethod(f = "dRiskRMD", signature = c("matrix"),
definition = function(obj, ...) {
  dRiskRMDWORK(x = obj, ...)
})

dRiskRMDWORK <- function(x, xm, k = 0.01, k2 = 0.05) {
  if (dim(x)[1] != dim(xm)[1]) {
    xm <- xm[1:dim(x)[1], ]
  }
  x <- scale(x)
  xm <- scale(xm)
  cent <- colMeans(x)
  covs <- covMcd(x)$cov
  rmd <- mahalanobis(x, center = cent, cov = covs)
  rmd <- sqrt(rmd) * 0.05  ##rmd/max(rmd) * 2
  mi <- x - k * rmd
  ma <- x + k * rmd
  w <- which(apply(xm < ma & xm > mi, 1, any) == TRUE)

  xd <- as.matrix(dist(xm))
  diag(xd) <- NA
  if (length(w) > 0) {
    ind <- apply(xd[w, , drop = FALSE], 1, function(x, k = k2) {
      min(x, na.rm = TRUE) > k2
    })
  } else {
    ind <- FALSE
  }
  riskvec1 <- riskvec2 <- rep(0, nrow(x))
  riskvec1[w] <- rmd[w]
  if (length(which(ind == TRUE)) > 0) {
    w2 <- as.integer(names(ind)[which(ind == TRUE)])
    riskvec2[w2] <- rmd[w2]
  } else {
    w2 <- NULL
  }
  w2 <- if (length(w) > 0) {
    w2
  } else {
    NULL
  }
  risk <- if (length(w) > 0) {
    length(w2)/dim(x)[1]
  } else 0
  wrisk <- if (length(w) > 0) {
    (length(w2) * sum((rmd[w2])))/dim(x)[1]
  } else 0
  list(risk1 = length(w)/dim(x)[1], risk2 = risk, wrisk1 = if (length(w) > 0) (length(w) *
    sum((rmd[w])))/dim(x)[1] else 0, wrisk2 = wrisk, indexRisk1 = w, indexRisk2 = w2, riskvec1 = riskvec1,
    riskvec2 = riskvec2)
}
