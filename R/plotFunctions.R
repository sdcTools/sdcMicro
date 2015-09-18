#' Plotfunctions for objects of class \code{\link{sdcMicroObj-class}}
#'
#' Descriptive plot function for Frequencies and local Supression, Recoding,
#' categorical risk and numerical risk.
#'
#' Possible values for the type argument of the print function are: "freq": for
#' Frequencies, "ls": for Local Supression output, "pram": for results of
#' post-randomization "recode":for Recodes, "risk": forCategorical risk and
#' "numrisk": for Numerical risk.
#'
#' Possible values for the type argument of the freq function are: "fk": Sample
#' frequencies and "Fk": weighted frequencies.
#'
#' @name plot.sdcMicroObj
#' @aliases plot-methods plot,sdcMicroObj-method plot
#' @docType methods
#' @param x An object of class \code{\link{sdcMicroObj-class}}
#' @param type specified what kind of plot will be generated
#' \itemize{
#' \item 'ls': plot of local suppressions in key variables
#' }
#' @param ... currently ignored
#' @author Bernhard Meindl
#' @keywords classes
#' @export
#' @examples
#'
#' data(testdata)
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('urbrur','roof','walls','relat','sex'),
#'   pramVars=c('water','electcon'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- kAnon(sdc, k=5)
#' plot(sdc, type="ls")
#'
setMethod(f = "plot", signature = c("sdcMicroObj"),
definition = function(x, type = "ls", ...) {
  if ( !type %in% c("ls") ) {
    stop("unsupported plot-type for sdcMicroObj!\n")
  }
  if ( type == "ls" ) {
    ls <- get.sdcMicroObj(x, type="localSuppression")
    if ( !is.null(ls)) {
      class(ls) <- "localSuppression"
      plot(ls)
    } else {
      return(NULL)
    }
  }
})
