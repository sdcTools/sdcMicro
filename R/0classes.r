### class sdcMicroObj ###
#' @useDynLib sdcMicro, .registration=TRUE
#' @import methods
#' @import Rcpp
#' @import robustbase
#' @import MASS
#' @import carData
#' @import cluster
#' @import tools
#' @import knitr
#' @import xtable
#' @import data.table
#' @import ggplot2
#' @import shinyBS
#' @import shiny
#' @import rhandsontable
#' @importFrom DT datatable
#' @importFrom prettydoc html_pretty
#' @importFrom rmarkdown pandoc_available
#' @importFrom rmarkdown render
#' @importFrom graphics axis
#' @importFrom graphics box
#' @importFrom graphics boxplot
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
#' @importFrom graphics rect
#' @importFrom graphics segments
#' @importFrom graphics strwidth
#' @importFrom graphics text
#' @importFrom stats as.formula
#' @importFrom stats coef
#' @importFrom stats cor
#' @importFrom stats cov
#' @importFrom stats formula
#' @importFrom stats glm
#' @importFrom stats lm
#' @importFrom stats mad
#' @importFrom stats median
#' @importFrom stats na.omit
#' @importFrom stats sd
#' @importFrom stats terms
#' @importFrom stats var
#' @importFrom stats runif
#' @importFrom utils data
setClassUnion("dataframeOrNULL", c("data.frame", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("logicalOrNULL", c("logical", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("factorOrNULL", c("factor", "NULL"))
setClassUnion("sdcmicroOrNULL", c("NULL"))

#' Class \code{"sdcMicroObj"}
#'
#' Class to save all information about the SDC process
#'
#' @name sdcMicroObj-class
#' @aliases sdcMicroObj-class
#' createSdcObj
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("sdcMicroObj", ...)}.
#' @author Bernhard Meindl, Alexander Kowarik, Matthias Templ, Elias Rut
#' @keywords classes
#' @export
setClass(Class = "sdcMicroObj",
  representation = representation(
    origData = "dataframeOrNULL",
    keyVars = "numericOrNULL",
    pramVars = "numericOrNULL",
    numVars = "numericOrNULL",
    ghostVars = "listOrNULL",
    weightVar = "numericOrNULL",
    hhId = "numericOrNULL",
    strataVar = "numericOrNULL",
    sensibleVar = "numericOrNULL",
    manipKeyVars = "dataframeOrNULL",
    manipPramVars = "dataframeOrNULL",
    manipNumVars = "dataframeOrNULL",
    manipGhostVars = "dataframeOrNULL",
    manipStrataVar = "factorOrNULL",
    originalRisk = "listOrNULL",
    risk = "listOrNULL",
    utility = "listOrNULL",
    pram = "listOrNULL",
    localSuppression = "listOrNULL",
    options = "listOrNULL",
    additionalResults = "listOrNULL",
    set = "listOrNULL",
    prev = "sdcmicroOrNULL",
    deletedVars = "characterOrNULL"),
  prototype = prototype(
    origData = NULL,
    keyVars = NULL,
    pramVars = NULL,
    numVars = NULL,
    ghostVars = NULL,
    weightVar = NULL,
    hhId = NULL,
    strataVar = NULL,
    sensibleVar = NULL,
    manipKeyVars = NULL,
    manipPramVars = NULL,
    manipNumVars = NULL,
    manipGhostVars = NULL,
    manipStrataVar = NULL,
    originalRisk = NULL,
    risk = NULL,
    utility = NULL,
    pram = NULL,
    localSuppression = NULL,
    options = NULL,
    additionalResults = NULL,
    set = NULL,
    prev = NULL,
    deletedVars = NULL),
  validity = function(object) {
    if (!is.null(object@manipKeyVars) && ncol(object@manipKeyVars) != length(object@keyVars)) {
      stop("wrong dimension of slot 'manipKeyVars'!\n")
    }
    if (!is.null(object@manipNumVars) && ncol(object@manipNumVars) != length(object@numVars)) {
      stop("wrong dimension of slot 'manipNumVars'!\n")
    }
    if (!is.null(object@strataVar) && object@strataVar %in% object@keyVars) {
      stop("stratification variable cant be a categorical key variable!\n")
    }
    return(TRUE)
  })

setIs("sdcMicroObj", "sdcmicroOrNULL")
