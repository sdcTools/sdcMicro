#' Global Recoding
#'
#' Global recoding of variables
#'
#' If a labels parameter is specified, its values are used to name the factor
#' levels.  If none is specified, the factor level labels are constructed.
#'
#' @name globalRecode
#' @docType methods
#' @param obj a numeric vector, a \code{data.frame} or an object of class
#' \code{\link{sdcMicroObj-class}}
#' @param ... see possible arguments below
#' \itemize{
#' \item{column: }{which keyVar should be changed. Character vector of length 1 specifying the variable name that
#' should be recoded (required if \code{obj} is a \code{data.frame} or
#' an object of class \code{\link{sdcMicroObj-class}}.}
#' \item{breaks: }{either a numeric vector of cut points or number giving the
#' number of intervals which x is to be cut into.}
#' \item{labels: }{labels for the levels of the resulting category. By default,
#' labels are constructed using "(a,b]" interval notation.  If labels = FALSE,
#' simple integer codes are returned instead of a factor.}
#' \item{method: }{The following arguments are supported:
#' \itemize{
#' \item \dQuote{equidistant:} for equal sized intervalls
#' \item \dQuote{logEqui:} for equal sized intervalls for log-transformed data
#' \item \dQuote{equalAmount:} for intervalls with approxiomately the same amount
#' of observations
#' }}}
#' @return the modified \code{\link{sdcMicroObj-class}} or a factor, unless labels = FALSE
#' which results in the mere integer level codes.
#' @seealso \code{\link{cut}}
#' @note \code{globalRecode} can not be applied to vectors stored as factors from sdcMicro >= 4.7.0!
#' @keywords manip
#' @author Matthias Templ and Bernhard Meindl
#' @references 
#' #' Templ, M. and Kowarik, A. and Meindl, B. 
#' Statistical Disclosure Control for Micro-Data Using the R Package sdcMicro. 
#' \emph{Journal of Statistical Software}, \strong{67} (4), 1--36, 2015. \doi{10.18637/jss.v067.i04}
#' 
#' Templ, M. Statistical Disclosure Control for Microdata: Methods and Applications in R.
#' \emph{Springer International Publishing}, 287 pages, 2017. ISBN 978-3-319-50272-4. \doi{10.1007/978-3-319-50272-4}
#' \doi{10.1007/978-3-319-50272-4}
#' 
#' @export
#' @examples
#' data(free1)
#' free1 <- as.data.frame(free1)
#'
#' ## application to a vector
#' head(globalRecode(free1$AGE, breaks=c(1,9,19,29,39,49,59,69,100), labels=1:8))
#' table(globalRecode(free1$AGE, breaks=c(1,9,19,29,39,49,59,69,100), labels=1:8))
#'
#' ## application to a data.frame
#' # automatic labels
#' table(globalRecode(free1, column="AGE", breaks=c(1,9,19,29,39,49,59,69,100))$AGE)
#'
#' ## calculation of brea-points using different algorithms
#' table(globalRecode(free1$AGE, breaks=6))
#' table(globalRecode(free1$AGE, breaks=6, method="logEqui"))
#' table(globalRecode(free1$AGE, breaks=6, method="equalAmount"))
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- globalRecode(sdc, column="water", breaks=3)
#' table(get.sdcMicroObj(sdc, type="manipKeyVars")$water)
globalRecode <- function(obj, ...) {
  globalRecodeX(obj=obj, ...)
}
setGeneric("globalRecodeX", function(obj, ...) {
  standardGeneric("globalRecodeX")
})

setMethod(f="globalRecodeX", signature = c("sdcMicroObj"),
definition=function(obj, column, ...) {
  x <- get.sdcMicroObj(obj, type="manipKeyVars")
  x <- globalRecode(x, column=column, ... )
  obj <- nextSdcObj(obj)
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(x))
  obj <- calcRisks(obj)
  obj
})

setMethod(f="globalRecodeX", signature=c("data.frame"),
definition = function(obj, column, ...) {
  if (!column %in% colnames(obj)) {
    stop("The variable specified in 'column' is not a valid key variable!\n")
  }
  if (length(column)!=1) {
    stop("more than one variable specified in 'column'!\n")
  }
  obj[[column]] <- globalRecodeWORK(x=obj[[column]], ...)
  obj
})

setMethod(f="globalRecodeX", signature=c("numeric"),
definition = function(obj, ...) {
  globalRecodeWORK(x=obj, ...)
})

globalRecodeWORK <- function(x, breaks, labels=NULL, method="equidistant") {
  ## aequidistant
  equidistant <- function(x, b=breaks) {
    b1 <- seq(min(x, na.rm=TRUE)-1,max(x, na.rm=TRUE)+1, length.out=b+1)
    # only round if no duplicated breaks result
    rb <- round(b1)
    if (!any(duplicated(rb))) {
      return(rb)
    }
    b1
  }
  ## aequidistant, log(data)
  logEqui <- function(x, b=breaks) {
    b1 <- log(x)
    b1 <- seq(min(b1, na.rm=TRUE)-1, max(b1, na.rm=TRUE)+1, length.out=b+1)
    b1 <- exp(b1)
    # only round if no duplicated breaks result
    rb <- round(b1)
    if (!any(duplicated(rb))) {
      return(rb)
    }
    b1
  }
  ## same-size groups
  equalAmount <- function(x, b=breaks) {
    SEQ <- c(0, ((1:b)/b))
    b1 <- quantile(na.omit(x), SEQ)
    if (any(duplicated(b1))) {
      b1 <- jitter(b1)
    }
    b1
  }
  
  stopifnot(method %in% c("equidistant","logEqui","equalAmount"))
  
  if (length(breaks)==1) {
    breaks <- round(breaks)
    stopifnot(breaks>=1)
    if (breaks==1) {
      gr <- cut(x, breaks=c(min(x)-1, max(x)+1), labels=labels, dig.lab=8)
    } else {
      gr <- cut(x, breaks=get(method)(x), labels=labels, dig.lab=8, include.lowest = TRUE)
    }
  }  else {
    gr <- cut(x, breaks=breaks, labels=labels, dig.lab=8)
  }
  invisible(gr)
}

