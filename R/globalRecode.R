#' Global Recoding
#'
#' Global recoding
#'
#' If a labels parameter is specified, its values are used to name the factor
#' levels.  If none is specified, the factor level labels are constructed.
#'
#' @name globalRecode
#' @aliases globalRecode-methods globalRecode,ANY-method
#' globalRecode,sdcMicroObj-method globalRecode
#' @docType methods
#' @param obj vector of class numeric or of class factor with integer labels
#' for recoding or an object of class \code{\link{sdcMicroObj-class}}
#' @param ... see possible arguments below
#' \itemize{
#' \item{column}{which keyVar should be changed}
#' \item{breaks}{either a numeric vector of cut points or number giving the
#' number of intervals which x is to be cut into.}
#' \item{labels}{labels for the levels of the resulting category. By default,
#' labels are constructed using "(a,b]" interval notation.  If labels = FALSE,
#' simple integer codes are returned instead of a factor.}
#' \item{method}{method \dQuote{equidistant} for equal sized intervalls;
#' method \dQuote{logEqui} for equal sized intervalls for log-transformed data;
#' method \dQuote{equalAmount} for intervalls with approxiomately the same amount of observations}}
#' @return the modified \code{\link{sdcMicroObj-class}} or a factor, unless labels = FALSE
#' which results in the mere integer level codes.
#' @section Methods: \describe{
#' \item{list("signature(obj = \"ANY\")")}{}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @seealso \code{\link{cut}}
#' @keywords manip
#' @export
#' @examples
#'
#' data(free1)
#' head(globalRecode(free1[,"AGE"], breaks=c(1,9,19,29,39,49,59,69,100), labels=1:8))
#' table(globalRecode(free1[,"AGE"], breaks=c(1,9,19,29,39,49,59,69,100), labels=1:8))
#' table(globalRecode(free1[,"AGE"], breaks=c(1,9,19,29,39,49,59,69,100)))
#' table(globalRecode(free1[,"AGE"], breaks=6))
#' table(globalRecode(free1[,"AGE"], breaks=6, method="logEqui"))
#' table(globalRecode(free1[,"AGE"], breaks=6, method="equalAmount"))
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- globalRecode(sdc, column="urbrur", breaks=5)
#'
setGeneric("globalRecode", function(obj, ...) {
  standardGeneric("globalRecode")
})

setMethod(f = "globalRecode", signature = c("sdcMicroObj"),
definition = function(obj, column, ...) {
  x <- get.sdcMicroObj(obj, type = "manipKeyVars")
  x[, column] <- globalRecodeWORK(x[, column], ...)
  obj <- nextSdcObj(obj)
  obj <- set.sdcMicroObj(obj, type = "manipKeyVars", input = list(x))
  obj <- calcRisks(obj)
  obj
})

setMethod(f = "globalRecode", signature = c("ANY"),
definition = function(obj, ...) {
  globalRecodeWORK(x = obj, ...)
})

globalRecodeWORK <- function(x, breaks, labels = NULL, method = "equidistant") {
  ## aequidistant
  equidistant <- function(x, b = breaks) {
    # b1 <- seq(min(x), max(x), length.out=b+1) b1 <- round(b1) b1
    b
  }
  ## aequidistant, log(data)
  logEqui <- function(x, b = breaks) {
    b1 <- log(x)
    b1 <- seq(min(b1), max(b1), length.out = b + 1)
    b1 <- round(exp(b1))
    b1
  }
  ## same-size groups
  equalAmount <- function(x, b = breaks) {
    SEQ <- c(0, (1:b)/b)
    b1 <- quantile(x, SEQ)
    b1 <- round(b1)
    b1
  }
  if (class(x) == "factor") {
    x <- as.numeric(as.character(x))
    if (length(breaks) == 1) {
      gr <- cut(x, breaks = get(method)(x), labels = labels, dig.lab = 8)
    } else gr <- cut(x, breaks = breaks, labels = labels, dig.lab = 8)
  } else {
    if (length(breaks) == 1) {
      gr <- cut(x, breaks = get(method)(x), labels = labels, dig.lab = 8)
    } else gr <- cut(x, breaks = breaks, labels = labels, dig.lab = 8)
  }
  invisible(gr)
}
