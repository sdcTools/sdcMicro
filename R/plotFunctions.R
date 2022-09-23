#' Plotfunctions for objects of class [sdcMicroObj-class]
#'
#' Descriptive plot function for [sdcMicroObj-class]-objects. Currently
#' only visualization of local supression is implemented.
#'
#' @md
#' @param x An object of class [sdcMicroObj-class]
#' @param type specified what kind of plot will be generated
#' - `"ls"`: plot of local suppressions in key variables
#' @param ... currently ignored
#' @author Bernhard Meindl
#' @keywords classes
#' @method plot sdcMicroObj
#' @return a `ggplot` plot object or (invisible) `NULL` if local suppression
#' using [kAnon()] has not been applied
#' @export
#' @examples
#' \donttest{
#' data(testdata)
#' sdc <- createSdcObj(testdata,
#'   keyVars = c("urbrur", "roof", "walls", "relat", "sex"),
#'   w = "sampling_weight")
#' sdc <- kAnon(sdc, k = 3)
#' plot(sdc, type = "ls")
#' }
plot.sdcMicroObj <- function(x, type="ls", ...) {
  if (!type %in% c("ls")) {
    stop("unsupported plot-type for sdcMicroObj!", call. = FALSE)
  }
  if (type == "ls") {
    ls <- get.sdcMicroObj(x, type="localSuppression")
    if (!is.null(ls)) {
      class(ls) <- "localSuppression"
      return(plot(ls))
    } else {
      return(invisible(NULL))
    }
  }
  if (type == "indiv_risk" | type == "indivRisk" | type == "indivrisk") {
    irisk <- x@risk$individual[, 1]
  }
}
