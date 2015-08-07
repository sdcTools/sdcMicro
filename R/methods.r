#' @rdname sdcMicroObj-class
#' @export
#' @note internal function
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
setGeneric("get.sdcMicroObj", function(object, type) {
  standardGeneric("get.sdcMicroObj")
})

#' modify \code{sdcMicroObj}-objects depending on argument \code{type}
#'
#' @param type a character vector of length 1 defining what to calculate|return|modify. Allowed types are:}
#' \itemize{
#' \item origData: set slot 'origData' of argument \code{object}
#' @param input a list depending on argument \code{type}.}
#' \itemize{
#' \item type==dataOrig: a list containing original microdata
#'
#' @return an object of class \code{sdcMicroObj}
#'
#' @export
#' @rdname sdcMicroObj-class
#'
#' @note internal function
#' @author Bernhard Meindl \email{bernhard.meindl@@statistik.gv.at}
setGeneric("set.sdcMicroObj", function(object, type, input) {
  standardGeneric("set.sdcMicroObj")
})

#' undo last changes to \code{sdcMicroObj}-objects if possible
#' note that this will only work if the user makes use of the prev slot or uses the sdcMicroObj functions
#'
#' @param object an object of class \code{sdcMicroObj}
#'
#' @return an object of class \code{sdcMicroObj}
#'
#' @export
#' @docType methods
#' @rdname sdcMicroObj-class
#'
#' @note internal function
#' @author Elias Rut
setGeneric("undolast", function(object) {
  standardGeneric("undolast")
})

#' @aliases get.sdcMicroObj,sdcMicroObj,character-method
#' @rdname sdcMicroObj-class
setMethod(f = "get.sdcMicroObj", signature = c("sdcMicroObj", "character"),
definition = function(object, type) {
  if (!type %in% c("origData", "keyVars", "pramVars", "numVars", "ghostVars", "weightVar", "hhId", "strataVar",
    "sensibleVar", "manipKeyVars", "manipPramVars", "manipNumVars", "manipGhostVars", "manipStrataVar", "originalRisk",
    "risk", "utility", "pram", "localSuppression", "options", "prev", "set", "deletedVars")) {
    stop("get.sdcMicroObj:: argument 'type' is not valid!\n")
  }
  if ((!type %in% object@set) && !is.null(object@prev)) {
    return(get.sdcMicroObj(object@prev, type))
  }
  if (!type %in% slotNames(object)) {
    stop("wrong argument 'type'!\n")
  }
  return(slot(object, type))
})

#' @aliases set.sdcMicroObj,sdcMicroObj,character,listOrNULL-method
#' @rdname sdcMicroObj-class
setMethod(f = "set.sdcMicroObj", signature = c("sdcMicroObj", "character", "listOrNULL"),
definition = function(object, type, input) {
  if (!type %in% c("origData", "keyVars", "pramVars", "numVars", "ghostVars", "weightVar", "hhId", "strataVar",
    "sensibleVar", "manipPramVars", "manipKeyVars", "manipNumVars", "manipGhostVars", "manipStrataVar", "risk",
    "utility", "pram", "localSuppression", "options", "prev", "set")) {
    stop("set.sdcMicroObj:: check argument 'type'!\n")
  }

  if (type == "origData")
    object@origData <- input[[1]]
  if (type == "keyVars")
    object@keyVars <- input[[1]]
  if (type == "pramVars")
    object@pramVars <- input[[1]]
  if (type == "numVars")
    object@numVars <- input[[1]]
  if (type == "ghostVars")
    object@ghostVars <- input[[1]]
  if (type == "weightVar")
    object@weightVar <- input[[1]]
  if (type == "hhId")
    object@hhId <- input[[1]]
  if (type == "strataVar")
    object@strataVar <- input[[1]]
  if (type == "sensibleVar")
    object@sensibleVar <- input[[1]]
  if (type == "manipKeyVars")
    object@manipKeyVars <- input[[1]]
  if (type == "manipPramVars")
    object@manipPramVars <- input[[1]]
  if (type == "manipNumVars")
    object@manipNumVars <- input[[1]]
  if (type == "manipGhostVars")
    object@manipGhostVars <- input[[1]]
  if (type == "manipStrataVar")
    object@manipStrataVar <- input[[1]]
  if (type == "risk")
    object@risk <- input[[1]]
  if (type == "utility")
    object@utility <- input[[1]]
  if (type == "pram")
    object@pram <- input[[1]]
  if (type == "localSuppression")
    object@localSuppression <- input[[1]]
  if (type == "options")
    object@options <- input[[1]]
  if (type == "prev")
    object@prev <- input[[1]]
  if (type == "set")
    object@set <- input[[1]]
  if (is.null(object@set))
    object@set <- list()
  if (length(object@set) == 0 || !type %in% object@set)
    object@set <- c(object@set, type)
  validObject(object)
  return(object)
})

setGeneric("calc.sdcMicroObj", function(object, type, ...) {
  standardGeneric("calc.sdcMicroObj")
})

setMethod(f = "calc.sdcMicroObj", signature = c("sdcMicroObj", "character"),
definition = function(object, type, ...) {
  if (!type %in% c("violateKAnon")) {
    stop("set.sdcMicroObj:: check argument 'type'!\n")
  }

  ### how many observations violate k-Anonymity
  if (type == "violateKAnon") {
    fk <- get.sdcMicroObj(object, type = "fk")
    args <- list(...)
    m <- match("k", names(args))
    if (!is.na(m)) {
      k <- args[[m]]
    } else {
      k <- 1
    }
    return(length(which(fk <= k)))
  }
})

#' @rdname sdcMicroObj-class
setMethod(f = "undolast", signature = c("sdcMicroObj"),
definition = function(object) {
  if (is.null(object@prev)) {
    warning("Can not undo. No previous state stored. (The input object is returned).\n")
    return(object)
  }
  return(object@prev)
})

