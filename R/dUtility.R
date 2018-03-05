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
#' @docType methods
#' @param obj original data or object of class \code{\link{sdcMicroObj-class}}
#' @param ... see arguments below
#' \itemize{
#' \item{xm: }{perturbed data}
#' \item{method: }{method IL1, IL1s or eigen. More methods are implemented in
#' summary.micro()}}
#' @return data utility or modified entry for data utility the \code{\link{sdcMicroObj-class}}.
#' @author Matthias Templ
#' @seealso \code{\link{dRisk}}, \code{\link{dRiskRMD}}
#' @references for IL1 and IL1s: see
#' \url{http://vneumann.etse.urv.es/webCrises/publications/isijcr/lncs3050Outlier.pdf},
#'
#' Templ, M. and Meindl, B., \emph{Robust Statistics Meets SDC: New Disclosure
#' Risk Measures for Continuous Microdata Masking}, Lecture Notes in Computer
#' Science, Privacy in Statistical Databases, vol. 5262, pp. 113-126, 2008.
#' @keywords manip
#' @export
#' @examples
#' data(free1)
#' free1 <- as.data.frame(free1)
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
#' dUtility(x, xm = y, method = " IL1")
#' dUtility(x, xm = y, method = " IL1s")
#' dUtility(x, xm = y, method = "eigen")
#' dUtility(x, xm = y, method = "robeigen")
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
dUtility <- function(obj, ...) {
  dUtilityX(obj=obj, ...)
}
setGeneric("dUtilityX", function(obj, ...) {
  standardGeneric("dUtilityX")
})

setMethod(f="dUtilityX", signature=c("sdcMicroObj"), definition=function(obj, ...) {
  numVars <- get.sdcMicroObj(obj, type = "numVars")
  x <- get.sdcMicroObj(obj, type = "origData")[, numVars, drop = F]
  xm <- get.sdcMicroObj(obj, type = "manipNumVars")
  utility <- get.sdcMicroObj(obj, type = "utility")
  utility$il1 <- dUtilityWORK(x = x, xm = xm, method = "IL1", ...)
  utility$il1s <- dUtilityWORK(x = x, xm = xm, method = "IL1s", ...)
  utility$eigen <- dUtilityWORK(x = x, xm = xm, method = "eigen", ...)
  # utility$robeigen <- dUtilityWORK(x=x, xm=xm, method='robeigen',...)
  obj <- set.sdcMicroObj(obj, type = "utility", input = list(utility))
  obj
})

setMethod(f="dUtilityX", signature=c("data.frame"), definition=function(obj, ...) {
  dUtilityWORK(x = obj, ...)
})

dUtilityWORK <- function(x, xm, method = "IL1s") {
  if (dim(x)[1] != dim(xm)[1]) {
    warnMsg <- "dimension of perturbed data and original data are different\n"
    obj <- addWarning(obj, warnMsg=warnMsg, method="dUtility", variable=NA)
    warning(warnMsg)
    xm <- xm[1:dim(x)[1], ]
  }
  if (method == "IL1") {
    a <- x
    for (i in 1:dim(x)[2]) {
      a[[i]] <- 100 * abs(x[[i]] - xm[[i]]) / (abs(x[[i]]))
    }
    infLoss1 <- sum(a, na.rm = TRUE)
    return(infLoss1)
  }
  if (method == "IL1old") {
    a <- x
    for (i in 1:dim(x)[2]) {
      a[[i]] <- abs(x[[i]] - xm[[i]])/(sd(x[[i]], na.rm = TRUE) * sqrt(2))
    }
    infLoss1 <- 1/(dim(x)[2] * dim(x)[1]) * sum(a, na.rm = TRUE)
    return(infLoss1)
  }
  if (method == "IL1s") {
    a <- x
    for (i in 1:dim(x)[2]) {
      a[[i]] <- abs(x[[i]] - xm[[i]]) / (sd(x[[i]], na.rm = TRUE) * sqrt(2))
    }
    infLoss1 <- sum(a, na.rm = TRUE)
    return(infLoss1)
  }
  if (method == "eigen") {
    e1 <- try(eigen(var(scale(x), na.rm = TRUE, use = "pairwise.complete.obs"))$values, silent = TRUE)
    e2 <- try(eigen(var(scale(xm), na.rm = TRUE, use = "pairwise.complete.obs"))$values, silent = TRUE)
    d <- try(sum(abs(e1 - e2)/e1), silent = TRUE)
    if("try-error" %in% class(d)){
      d <- NA
    }
    return(d)
  }
  if (method == "robeigen") {
    e1 <- try(eigen(covMcd(scale(x))$cov)$values, silent = TRUE)
    e2 <- try(eigen(covMcd(scale(xm))$cov)$values, silent = TRUE)
    d <- try(sum(abs(e1 - e2)/e1), silent = TRUE)
    if("try-error" %in% class(d)){
      d <- NA
    }
    return(d)
  }
}
