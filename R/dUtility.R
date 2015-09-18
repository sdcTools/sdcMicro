#' data utility
#'
#' IL1s data utility.
#'
#' The standardised distances of the perturbed data values to the original ones
#' are measured.  Measure IL1 measures the distances between the original
#' values and the perturbed ones, scaled by the standard deviation.  Method
#' \sQuote{eigen} and \sQuote{robeigen} compares the eigenvalues and robust
#' eigenvalues form the original data and the perturbed data.
#'
#' @name dUtility
#' @aliases dUtility-methods dUtility,data.frame-method dUtility,matrix-method
#' dUtility,sdcMicroObj-method dUtility
#' @docType methods
#' @param obj original data or object of class \code{\link{sdcMicroObj-class}}
#' @param ... see arguments below
#' \itemize{
#' \item{xm}{perturbed data}
#' \item{method}{method IL1 or eigen. More methods are implemented in
#' summary.micro()}}
#' @return data utility or modified entry for data utility the \code{\link{sdcMicroObj-class}}.
#' @section Methods: \describe{
#' \item{list("signature(obj = \"data.frame\")")}{}
#' \item{list("signature(obj = \"matrix\")")}{}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @author Matthias Templ
#' @seealso \code{\link{dRisk}}, \code{\link{dRiskRMD}}
#' @references for IL1s: see
#' \url{http://vneumann.etse.urv.es/webCrises/publications/isijcr/lncs3050Outlier.pdf},
#'
#' Templ, M. and Meindl, B., \emph{Robust Statistics Meets SDC: New Disclosure
#' Risk Measures for Continuous Microdata Masking}, Lecture Notes in Computer
#' Science, Privacy in Statistical Databases, vol. 5262, pp. 113-126, 2008.
#' @keywords manip
#' @export
#' @examples
#'
#' data(free1)
#' m1 <- microaggregation(free1[, 31:34], method="onedims", aggr=3)
#' m2 <- microaggregation(free1[, 31:34], method="pca", aggr=3)
#' dRisk(obj=free1[, 31:34], xm=m1$mx)
#' dRisk(obj=free1[, 31:34], xm=m2$mx)
#' dUtility(obj=free1[, 31:34], xm=m1$mx)
#' dUtility(obj=free1[, 31:34], xm=m2$mx)
#' data(Tarragona)
#' x <- Tarragona[, 5:7]
#' y <- addNoise(x)$xm
#' dRiskRMD(x, xm=y)
#' dRisk(x, xm=y)
#' dUtility(x, xm=y)
#' dUtility(x, xm=y, method="eigen")
#' dUtility(x, xm=y, method="robeigen")
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' ## this is already made internally:
#' ## sdc <- dUtility(sdc)
#' ## and already stored in sdc
#'
setGeneric("dUtility", function(obj, ...) {
  standardGeneric("dUtility")
})

setMethod(f = "dUtility", signature = c("sdcMicroObj"),
definition = function(obj, ...) {
  numVars <- get.sdcMicroObj(obj, type = "numVars")
  x <- get.sdcMicroObj(obj, type = "origData")[, numVars, drop = F]
  xm <- get.sdcMicroObj(obj, type = "manipNumVars")
  utility <- get.sdcMicroObj(obj, type = "utility")
  utility$il1 <- dUtilityWORK(x = x, xm = xm, method = "IL1", ...)
  utility$eigen <- dUtilityWORK(x = x, xm = xm, method = "eigen", ...)
  # utility$robeigen <- dUtilityWORK(x=x, xm=xm, method='robeigen',...)
  obj <- set.sdcMicroObj(obj, type = "utility", input = list(utility))
  obj
})

setMethod(f = "dUtility", signature = c("data.frame"), definition = function(obj, ...) {
  dUtilityWORK(x = obj, ...)
})

setMethod(f = "dUtility", signature = c("matrix"), definition = function(obj, ...) {
  dUtilityWORK(x = obj, ...)
})

dUtilityWORK <- function(x, xm, method = "IL1") {
  if (dim(x)[1] != dim(xm)[1]) {
    warning("dimension of perturbed data and original data are different")
    xm <- xm[1:dim(x)[1], ]
  }
  if (method == "IL1") {
    a <- x
    for (i in 1:dim(x)[2]) {
      a[, i] <- abs((x[, i] - xm[, i])/sd(x[, i], na.rm = TRUE) * sqrt(2))
    }
    infLoss1 <- 1/(dim(x)[2] * dim(x)[1]) * sum(a, na.rm = TRUE)
    return(infLoss1)
  }
  if (method == "eigen") {
    e1 <- eigen(var(scale(x), na.rm = TRUE, use = "pairwise.complete.obs"))$values
    e2 <- eigen(var(scale(xm), na.rm = TRUE, use = "pairwise.complete.obs"))$values
    d <- sum(abs(e1 - e2)/e1)
    return(d)
  }
  if (method == "robeigen") {
    e1 <- eigen(covMcd(scale(x))$cov)$values
    e2 <- eigen(covMcd(scale(xm))$cov)$values
    d <- sum(abs(e1 - e2)/e1)
    return(d)
  }
}
