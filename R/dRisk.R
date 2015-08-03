#' overal disclosure risk
#'
#' Distance-based disclosure risk estimation via standard deviation-based
#' intervals around observations.
#'
#' An interval (based on the standard deviation) is built around each value of
#' the perturbed value.  Then we look if the original values lay in these
#' intervals or not. With parameter k one can enlarge or down scale the
#' interval.
#'
#' @name dRisk
#' @aliases dRisk-methods dRisk,data.frame-method dRisk,matrix-method
#' dRisk,sdcMicroObj-method dRisk
#' @docType methods
#' @param obj original data or object of class \code{\link{sdcMicroObj-class}}
#' @param ... possible arguments are:
#' \itemize{
#' \item {xm}{perturbed data}
#' \item {k}{percentage of the standard deviation}}
#' @return The disclosure risk or/and the modified \code{\link{sdcMicroObj-class}}
#' @section Methods: \describe{
#' \item{list("signature(obj = \"data.frame\")")}{}
#' \item{list("signature(obj = \"matrix\")")}{}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @author Matthias Templ
#' @seealso \code{\link{dUtility}}, \code{\link{dUtility}}
#' @references see method SDID in
#' \url{http://vneumann.etse.urv.es/webCrises/publications/isijcr/lncs3050Outlier.pdf}
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
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' ## this is already made internally: sdc <- dRisk(sdc)
#' ## and already stored in sdc
#'
setGeneric("dRisk", function(obj, ...) {
  standardGeneric("dRisk")
})

setMethod(f = "dRisk", signature = c("sdcMicroObj"),
definition = function(obj, ...) {
  numVars <- get.sdcMicroObj(obj, type = "numVars")
  x <- get.sdcMicroObj(obj, type = "origData")[, numVars, drop = F]
  xm <- get.sdcMicroObj(obj, type = "manipNumVars")
  risk <- get.sdcMicroObj(obj, type = "risk")
  risk$numeric <- dRiskWORK(x = x, xm = xm, ...)
  obj <- set.sdcMicroObj(obj, type = "risk", input = list(risk))
  obj
})

setMethod(f = "dRisk", signature = c("data.frame"), definition = function(obj, ...) {
  dRiskWORK(x = obj, ...)
})

setMethod(f = "dRisk", signature = c("matrix"), definition = function(obj, ...) {
  dRiskWORK(x = obj, ...)
})

dRiskWORK <- function(x, xm, k = 0.05) {
  if (dim(x)[1] != dim(xm)[1]) {
    warning("dimension of perturbed data and original data are different")
    xm <- xm[1:dim(x)[1], ]
  }
  sds <- apply(xm, 2, sd, na.rm = TRUE)
  mi <- t(t(xm) - k * sds)
  ma <- t(t(xm) + k * sds)
  w <- which(rowSums(x < mi | x > ma, na.rm = TRUE) %in% 0:1)
  as.numeric(length(w)/nrow(x))
}
