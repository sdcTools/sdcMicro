#' Plotfunctions for objects of class \code{\link{sdcMicroObj-class}}
#'
#' Descriptive plot function for \code{\link{sdcMicroObj-class}}-objects. Currently
#' only visualization of local supression is implemented.
#'
#' @param x An object of class \code{\link{sdcMicroObj-class}}
#' @param type specified what kind of plot will be generated
#' \itemize{
#' \item 'ls': plot of local suppressions in key variables
#' }
#' @param ... currently ignored
#' @author Bernhard Meindl
#' @keywords classes
#' @method plot sdcMicroObj
#' @export
#' @examples
#'
#' data(testdata)
#' \dontrun{
#' # dontrun because Examples with CPU time > 2.5 times elapsed time
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('urbrur','roof','walls','relat','sex'),
#'   pramVars=c('water','electcon'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- kAnon(sdc, k=5)
#' plot(sdc, type="ls")
#' }
plot.sdcMicroObj <- function(x, type="ls", ...) {
  if (!type %in% c("ls")) {
    stop("unsupported plot-type for sdcMicroObj!\n")
  }
  if (type == "ls") {
    ls <- get.sdcMicroObj(x, type="localSuppression")
    if (!is.null(ls)) {
      class(ls) <- "localSuppression"
      plot(ls)
    } else {
      invisible(NULL)
    }
  }
  if(type == "indiv_risk" | type == "indivRisk" | type == "indivrisk"){
    irisk <- x@risk$individual[, 1]
    
  }
}
