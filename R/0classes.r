### class sdcMicroObj ###

#' @useDynLib sdcMicro
#' @import 'methods'
#' @import 'Rcpp'
#' @import 'robustbase'
#' @import 'MASS'
#' @import 'car'
#' @import 'cluster'
#' @import 'e1071'
#' @import 'tools'
#' @import 'knitr'
#' @import 'xtable'
#' @import 'data.table'
#' @import 'ggplot2'
#' @importFrom 'sets' 'set_power'
#' @importFrom 'rmarkdown' 'render'
#' @importFrom 'graphics' 'axis'
#' @importFrom 'graphics' 'box'
#' @importFrom 'graphics' 'boxplot'
#' @importFrom 'graphics' 'legend'
#' @importFrom 'graphics' 'lines'
#' @importFrom 'graphics' 'mtext'
#' @importFrom 'graphics' 'par'
#' @importFrom 'graphics' 'plot.new'
#' @importFrom 'graphics' 'plot.window'
#' @importFrom 'graphics' 'rect'
#' @importFrom 'graphics' 'segments'
#' @importFrom 'graphics' 'strwidth'
#' @importFrom 'graphics' 'text'
#' @importFrom 'stats' 'as.formula'
#' @importFrom 'stats' 'biplot'
#' @importFrom 'stats' 'coef'
#' @importFrom 'stats' 'cor'
#' @importFrom 'stats' 'cov'
#' @importFrom 'stats' 'density'
#' @importFrom 'stats' 'dist'
#' @importFrom 'stats' 'fitted'
#' @importFrom 'stats' 'formula'
#' @importFrom 'stats' 'glm'
#' @importFrom 'stats' 'kmeans'
#' @importFrom 'stats' 'lm'
#' @importFrom 'stats' 'mad'
#' @importFrom 'stats' 'mahalanobis'
#' @importFrom 'stats' 'median'
#' @importFrom 'stats' 'model.matrix'
#' @importFrom 'stats' 'na.omit'
#' @importFrom 'stats' 'poisson'
#' @importFrom 'stats' 'predict'
#' @importFrom 'stats' 'princomp'
#' @importFrom 'stats' 'qchisq'
#' @importFrom 'stats' 'qnorm'
#' @importFrom 'stats' 'quantile'
#' @importFrom 'stats' 'rnorm'
#' @importFrom 'stats' 'runif'
#' @importFrom 'stats' 'sd'
#' @importFrom 'stats' 'terms'
#' @importFrom 'stats' 'var'
#' @importFrom 'stats' 'weighted.mean'
#' @importFrom 'stats' 'xtabs'
#' @importFrom 'utils' 'combn'
#' @importFrom 'utils' 'data'
#' @importFrom 'utils' 'flush.console'
#' @importFrom 'utils' 'tail'

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
#' @aliases sdcMicroObj-class get.sdcMicroObj nextSdcObj set.sdcMicroObj
#' undolast get.sdcMicroObj,sdcMicroObj,character-method
#' set.sdcMicroObj,sdcMicroObj,character,listOrNULL-method
#' undolast,sdcMicroObj-method show show,sdcMicroObj-method
#' nextSdcObj,sdcMicroObj-method, createSdcObj
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("sdcMicroObj", ...)}.
#' @author Bernhard Meindl, Alexander Kowarik, Matthias Templ, Elias Rut
#' @keywords classes
#' @export
#' @examples
#'
#' showClass("sdcMicroObj")
#' \dontrun{
#' data(testdata)
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' head(sdc@@manipNumVars)
#' ### Display Risks
#' sdc@@risk$global
#' sdc <- dRisk(sdc)
#' sdc@@risk$numeric
#' ### use addNoise without Parameters
#' sdc <- addNoise(sdc,variables=c("expend","income"))
#' head(sdc@@manipNumVars)
#' sdc@@risk$numeric
#' ### undolast
#' sdc <- undolast(sdc)
#' head(sdc@@manipNumVars)
#' sdc@@risk$numeric
#' ### redo addNoise with Parameter
#' sdc <- addNoise(sdc, noise=0.2)
#' head(sdc@@manipNumVars)
#' sdc@@risk$numeric
#' ### dataGen
#' #sdc <- undolast(sdc)
#' #head(sdc@@risk$individual)
#' #sdc@@risk$global
#' #sdc <- dataGen(sdc)
#' #head(sdc@@risk$individual)
#' #sdc@@risk$global
#' ### LocalSuppression
#' sdc <- undolast(sdc)
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' sdc <- localSuppression(sdc)
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' ### microaggregation
#' sdc <- undolast(sdc)
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' sdc <- microaggregation(sdc)
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' ### pram
#' sdc <- undolast(sdc)
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' sdc <- pram(sdc,keyVar="water")
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' ### rankSwap
#' sdc <- undolast(sdc)
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' sdc <- rankSwap(sdc)
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' ### suda2
#' sdc <- suda2(sdc)
#' sdc@@risk$suda2
#' ### topBotCoding
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' sdc@@risk$numeric
#' sdc <- topBotCoding(sdc, value=60000000, replacement=62000000, column="income")
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' sdc@@risk$numeric
#' ### LocalRecProg
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c("urbrur", "roof", "walls", "water", "sex", "relat"))
#' sdc@@risk$global
#' sdc <- LocalRecProg(sdc)
#' sdc@@risk$global
#' ### LLmodGlobalRisk
#' sdc <- undolast(sdc)
#' sdc <- LLmodGlobalRisk(sdc, inclProb=0.001)
#' sdc@@risk$model
#' }
#'
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
    if ( !is.null(object@manipKeyVars) && ncol(object@manipKeyVars) != length(object@keyVars) ) {
      stop("wrong dimension of slot 'manipKeyVars'!\n")
    }
    if ( !is.null(object@manipNumVars) && ncol(object@manipNumVars) != length(object@numVars) ) {
      stop("wrong dimension of slot 'manipNumVars'!\n")
    }
    return(TRUE)
  })

setIs("sdcMicroObj", "sdcmicroOrNULL")
