#' Generate one strata variable from multiple factors
#'
#' For strata defined by multiple variables (e.g. sex,age,country) one combined
#' variable is generated.
#'
#'
#' @param df a data.frame
#' @param stratavars character vector with variable name
#' @param name name of the newly generated variable
#' @return The original data set with one new column.
#' @author Alexander Kowarik
#' @export
#' @examples
#'
#' x <- testdata
#' x <- generateStrata(x,c("sex","urbrur"),"strataIDvar")
#' head(x)
#'
generateStrata <- function(df, stratavars, name) {
  strata <- rep("", nrow(df))
  for (i in seq_along(stratavars)) {
    strata <- paste(strata, df[, stratavars[i]], sep = "")
    if (length(stratavars) > i) {
      strata <- paste(strata, "-", sep = "")
    }
  }
  df <- cbind(df, strata)
  colnames(df)[length(colnames(df))] <- name
  return(df)
}

#' Remove certain variables from the data set inside a sdc object.
#'
#' Delete variables without changing anything else in the sdcObject (writing
#' NAs).
#'
#'
#' @name removeDirectID
#' @aliases removeDirectID removeDirectID-methods
#' removeDirectID,sdcMicroObj-method
#' @docType methods
#' @param obj object of class \code{\link{sdcMicroObj-class}}
#' @param var name of the variable(s) to be remove
#' @return the modified \code{\link{sdcMicroObj-class}}
#' @section Methods: \describe{
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @author Alexander Kowarik
#' @keywords methods
#' @export
#' @examples
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata, keyVars=c('urbrur','roof'),
#'  numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- removeDirectID(sdc, var="age")
#'
setGeneric("removeDirectID", function(obj, var) {
  standardGeneric("removeDirectID")
})

setMethod(f = "removeDirectID", signature = c("sdcMicroObj"),
definition = function(obj, var) {
  kV <- colnames(obj@origData)[get.sdcMicroObj(obj, "keyVars")]
  nV <- colnames(obj@origData)[get.sdcMicroObj(obj, "numVars")]
  wV <- colnames(obj@origData)[get.sdcMicroObj(obj, "weightVar")]
  sV <- colnames(obj@origData)[get.sdcMicroObj(obj, "strataVar")]
  hV <- colnames(obj@origData)[get.sdcMicroObj(obj, "hhId")]

  if (any(var %in% kV))
    stop("A direct identifier should not be seleceted as key variable.\n Therefore it can not be removed.")
  if (any(var %in% nV))
    stop("A direct identifier should not be seleceted as numerical key variable.\n Therefore it can not be removed.")
  if (any(var %in% wV))
    stop("A direct identifier should not be seleceted as weight variable.\n Therefore it can not be removed.")
  if (any(var %in% sV))
    stop("A direct identifier should not be seleceted as strata variable.\n Therefore it can not be removed.")
  if (any(var %in% hV))
    stop("A direct identifier should not be seleceted as cluster ID.\n Therefore it can not be removed.")

  o <- obj@origData
  if (any(!var %in% colnames(o)))
    stop("direct identifier variable not found on data set")
  o[, !colnames(o) %in% var, drop = FALSE]
  obj <- nextSdcObj(obj)
  obj@deletedVars <- c(obj@deletedVars, var)
  obj@origData <- o
  obj
})

#' Change the a keyVariable of an object of class \code{\link{sdcMicroObj-class}} from Numeric to
#' Factor or from Factor to Numeric
#'
#' Change the scale of a variable
#'
#'
#' @name varToFactor
#' @aliases varToFactor varToNumeric varToFactor-methods varToNumeric-methods
#' varToFactor,sdcMicroObj-method varToNumeric,sdcMicroObj-method
#' @docType methods
#' @param obj object of class \code{\link{sdcMicroObj-class}}
#' @param var name of the keyVariable to change
#' @return the modified \code{\link{sdcMicroObj-class}}
#' @section Methods: \describe{
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @keywords methods
#' @export
#' @examples
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- varToFactor(sdc, var="urbrur")
#'
setGeneric("varToFactor", function(obj, var) {
  standardGeneric("varToFactor")
})

setMethod(f = "varToFactor", signature = c("sdcMicroObj"),
definition = function(obj, var) {
  x <- get.sdcMicroObj(obj, type = "manipKeyVars")
  obj <- nextSdcObj(obj)
  x[, var] <- as.factor(x[, var])
  obj <- set.sdcMicroObj(obj, type = "manipKeyVars", input = list(as.data.frame(x)))
  obj
})

#' @export
setGeneric("varToNumeric", function(obj, var) {
  standardGeneric("varToNumeric")
})

setMethod(f = "varToNumeric", signature = c("sdcMicroObj"),
definition = function(obj, var) {
  x <- get.sdcMicroObj(obj, type = "manipKeyVars")
  obj <- nextSdcObj(obj)
  suppressWarnings(tmpvar <- as.numeric(as.character(x[, var])))
  x[, var] <- tmpvar
  obj <- set.sdcMicroObj(obj, type = "manipKeyVars", input = list(as.data.frame(x)))
  obj
})

#' Join levels of a keyVariable in an object of class \code{\link{sdcMicroObj-class}}
#'
#' Transforms the factor variable into a factors with less levels and
#' recomputes risk.
#'
#' @name groupVars
#' @aliases groupVars groupVars-methods groupVars,sdcMicroObj-method
#' @docType methods
#' @param obj object of class \code{\link{sdcMicroObj-class}}
#' @param var name of the keyVariable to change
#' @param before vector of levels before recoding
#' @param after vector of levels after recoding
#' @return the modified \code{\link{sdcMicroObj-class}}
#' @section Methods: \describe{
#' \item{list("signature(obj = \"sdcMicroObj\")")}{
#' This method transform a factor variable with some levels into a new factor
#' variable with less levels. The user must make sure that all levels of the
#' original variable are listed in argument 'before' and that the number of
#' elements in argument 'after' (the new levels) have the same length. This
#' means that there should be a one to one mapping from any level of the
#' original factor to a level in the recoded variable. } }
#' @keywords methods
#' @export
#' @examples
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' testdata2$urbrur <- as.factor(testdata2$urbrur)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- groupVars(sdc, var="urbrur", before=c("1","2"), after=c("1","1"))
#'
setGeneric("groupVars", function(obj, var, before, after) {
  standardGeneric("groupVars")
})

setMethod(f = "groupVars", signature = c("sdcMicroObj"),
definition = function(obj, var, before, after) {
  if (length(before) != length(after)) {
    stop("Arguments 'before' and 'after' have different length!\n")
  }
  x <- get.sdcMicroObj(obj, type = "manipKeyVars")
  obj <- nextSdcObj(obj)
  if (!all(before %in% levels(x[, var]))) {
    stop("some elements of 'before' are not valid levels in variable 'var'!\n")
  }
  if (any(duplicated(before))) {
    stop("each level from the original factor must be listed only once in argument 'before'!")
  }
  for (i in 1:length(before)) {
    levels(x[, var]) <- ifelse(levels(x[, var]) == before[i], after, levels(x[, var]))
  }
  obj <- set.sdcMicroObj(obj, type = "manipKeyVars", input = list(x))
  obj
})

#' Change the name of levels of a keyVariable in an object of class
#' \code{\link{sdcMicroObj-class}}
#'
#' Change the labels of levels.
#'
#' @name renameVars
#' @aliases renameVars renameVars-methods renameVars,sdcMicroObj-method
#' @docType methods
#' @param obj object of class \code{\link{sdcMicroObj-class}}
#' @param var name of the keyVariable to change
#' @param before vector of levels before
#' @param after vector of levels after
#' @return the modified \code{\link{sdcMicroObj-class}}
#' @section Methods: \describe{
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @keywords manip
#' @export
#' @examples
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- renameVars(sdc, var="urbrur", before=2, after=78)
#'
setGeneric("renameVars", function(obj, var, before, after) {
  standardGeneric("renameVars")
})

setMethod(f = "renameVars", signature = c("sdcMicroObj"),
definition = function(obj, var, before, after) {
  x <- get.sdcMicroObj(obj, type = "manipKeyVars")
  obj <- nextSdcObj(obj)
  levels(x[, var]) <- ifelse(levels(x[, var]) == before, after, levels(x[, var]))
  obj <- set.sdcMicroObj(obj, type = "manipKeyVars", input = list(x))
  obj
})
