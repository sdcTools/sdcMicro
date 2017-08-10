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
#' @param obj a \code{data.frame} or object of class \code{\link{sdcMicroObj-class}}
#' @param ... possible arguments are:
#' \itemize{
#' \item {\code{xm}: }{perturbed data}
#' \item {\code{k}: }{percentage of the standard deviation}}
#' @return The disclosure risk or/and the modified \code{\link{sdcMicroObj-class}}
#' @author Matthias Templ
#' @seealso \code{\link{dUtility}}
#' @references see method SDID in
#' \url{http://vneumann.etse.urv.es/webCrises/publications/isijcr/lncs3050Outlier.pdf}
#' 
#' Templ, M. Statistical Disclosure Control for Microdata: Methods and Applications in R.
#' \emph{Springer International Publishing}, 287 pages, 2017. ISBN 978-3-319-50272-4.
#' \doi{10.1007/978-3-319-50272-4}
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
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' ## this is already made internally: sdc <- dRisk(sdc)
#' ## and already stored in sdc
dRisk <- function(obj, ...) {
  dRiskX(obj, ...)
}

setGeneric("dRiskX", function(obj, ...) {
  standardGeneric("dRiskX")
})

setMethod(f = "dRiskX", signature = c("sdcMicroObj"),
definition = function(obj, ...) {
  numVars <- get.sdcMicroObj(obj, type = "numVars")
  x <- get.sdcMicroObj(obj, type = "origData")[, numVars, drop = F]
  xm <- get.sdcMicroObj(obj, type = "manipNumVars")
  risk <- get.sdcMicroObj(obj, type = "risk")
  risk$numeric <- dRiskWORK(x = x, xm = xm, ...)
  obj <- set.sdcMicroObj(obj, type = "risk", input = list(risk))
  obj
})

setMethod(f = "dRiskX", signature = c("data.frame"), definition = function(obj, ...) {
  dRiskWORK(x = obj, ...)
})

dRiskWORK <- function(x, xm, k = 0.05) {
  if (dim(x)[1] != dim(xm)[1]) {
    warnMsg <- "dimension of perturbed data and original data are different\n"
    obj <- addWarning(obj, warnMsg=warnMsg, method="dRisk", variable=NA)
    warning(warnMsg)
    xm <- xm[1:dim(x)[1], ]
  }
  sds <- apply(xm, 2, sd, na.rm = TRUE)
  mi <- t(t(xm) - k * sds)
  ma <- t(t(xm) + k * sds)
  w <- which(rowSums(x < mi | x > ma, na.rm = TRUE) %in% 0:1)
  as.numeric(length(w)/nrow(x))
}
