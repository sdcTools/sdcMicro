#' get.sdcMicroObj
#'
#' extract information from \code{\link{sdcMicroObj-class}}-objects depending on argument \code{type}
#'
#' @param object a \code{\link{sdcMicroObj-class}}-object
#' @param type a character vector of length 1 defining what to calculate|return|modify. Allowed types are are
#' all slotNames of \code{obj}.
#' @return a slot of a \code{\link{sdcMicroObj-class}}-object depending on argument \code{type}
#' @export
#' @examples
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sl <- slotNames(sdc)
#' res <- sapply(sl, function(x) get.sdcMicroObj(sdc, type=x))
#' str(res)
get.sdcMicroObj <- function(object, type) {
  get.sdcMicroObjX(object=object, type=type)
}
setGeneric("get.sdcMicroObjX", function(object, type) {
  standardGeneric("get.sdcMicroObjX")
})
setMethod(f="get.sdcMicroObjX", signature = c("sdcMicroObj", "character"),
definition=function(object, type) {
  if (!type %in% slotNames(object)) {
    stop("get.sdcMicroObj:: argument 'type' is not valid!\n")
  }
  if ((!type %in% object@set) && !is.null(object@prev)) {
    return(get.sdcMicroObj(object@prev, type))
  }
  return(slot(object, type))
})

#' set.sdcMicroObj
#'
#' modify \code{\link{sdcMicroObj-class}}-objects depending on argument \code{type}
#'
#' @param object a \code{\link{sdcMicroObj-class}}-object
#' @param type a character vector of length 1 defining what to calculate|return|modify. Allowed types are listed below
#' and the slot with the corresponding name will be replaced by the content of \code{input.}
#' \itemize{
#' \item \code{origData: }
#' \item \code{keyVars: }
#' \item \code{pramVars: }
#' \item \code{numVars: }
#' \item \code{weightVar: }
#' \item \code{hhId: }
#' \item \code{strataVar: }
#' \item \code{sensibleVar: }
#' \item \code{manipPramVars: }
#' \item \code{manipNumVars: }
#' \item \code{manipGhostVars: }
#' \item \code{manipStrataVar: }
#' \item \code{risk: }
#' \item \code{utility: }
#' \item \code{pram: }
#' \item \code{localSuppression: }
#' \item \code{options: }
#' \item \code{prev: }
#' \item \code{set: }
#' \item \code{additionalResults: }
#' \item \code{deletedVars: }}
#' @param input a list depending on argument \code{type}. The content of the list must
#' match the allowed data-type of the slot in the \code{\link{sdcMicroObj-class}}-object
#' that should be replaced.
#' @return a \code{\link{sdcMicroObj-class}}-object
#' @export
#' @examples
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' ind_pram <- match(c("sex"), colnames(testdata2))
#' get.sdcMicroObj(sdc, type="pramVars")
#' sdc <- set.sdcMicroObj(sdc, type="pramVars", input=list(ind_pram))
#' get.sdcMicroObj(sdc, type="pramVars")
set.sdcMicroObj <- function(object, type, input) {
  set.sdcMicroObjX(object=object, type=type, input=input)
}

setGeneric("set.sdcMicroObjX", function(object, type, input) {
  standardGeneric("set.sdcMicroObjX")
})
setMethod(f="set.sdcMicroObjX", signature=c("sdcMicroObj", "character", "listOrNULL"),
definition=function(object, type, input) {
  if (!type %in% c("origData", "keyVars", "pramVars", "numVars", "ghostVars", "weightVar", "hhId", "strataVar",
    "sensibleVar", "manipPramVars", "manipKeyVars", "manipNumVars", "manipGhostVars", "manipStrataVar", "risk",
    "utility", "pram", "localSuppression", "options", "prev", "set", "additionalResults", "deletedVars")) {
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
  if (type == "additionalResults")
    object@additionalResults <- input[[1]]
  if (type == "deletedVars")
    object@deletedVars <- input[[1]]
  if (is.null(object@set))
    object@set <- list()
  if (length(object@set) == 0 || !type %in% object@set)
    object@set <- c(object@set, type)
  validObject(object)
  return(object)
})

#' undo last changes to \code{sdcMicroObj}-objects if possible
#' note that this will only work if the user makes use of the prev slot or uses the sdcMicroObj functions
#'
#' @param object a \code{\link{sdcMicroObj-class}} object
#'
#' @return a \code{\link{sdcMicroObj-class}} object
#' @export
#' @docType methods
#' @rdname sdcMicroObj-class
undolast <- function(object) {
  undolastX(object=object)
}

setGeneric("undolastX", function(object) {
  standardGeneric("undolastX")
})
setMethod(f="undolastX", signature=c("sdcMicroObj"), definition=function(object) {
  if (is.null(object@prev)) {
    warnMsg <- "Can not undo. No previous state stored. (The input object is returned).\n"
    object <- addWarning(object, warnMsg=warnMsg, method="undolast", variable=NA)
    warning(warnMsg)
    return(object)
  }
  return(object@prev)
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

#' \code{strataVar<-} allows to modify the variable which is used if anonymization limitation
#' techniques are applied independent for each characteristic of the defined strata.
#' @param value \code{NULL} or a character vector of length 1 specifying a valid variable name
#' @return an object of class \code{sdcMicroObj} with modified slot \code{@strataVar}
#'
#' @export
#' @docType methods
#' @rdname sdcMicroObj-class
setGeneric("strataVar<-", function(object, value) {
  standardGeneric("strataVar<-")
})

#' @rdname sdcMicroObj-class
#' @export
setReplaceMethod(f="strataVar",
signature=signature(object="sdcMicroObj", value="characterOrNULL"),
definition = function(object, value) {
 if (is.null(value)) {
   object@strataVar <- NULL
   if (length(object@set) == 0 || !"strataVar" %in% object@set)
     object@set <- c(object@set, "strataVar")
   return(object)
 }

 cn <- colnames(object@origData)
 if (length(value)!=1) {
   stop("only a single (existing) variable might be set as stratification variable!\n")
 }
 if (!value%in%cn) {
   stop("stratification-variables could not be found!\n")
 }
 if (value%in%colnames(object@manipKeyVars)) {
   stop("stratification-variables cannot be a categorical key variable!\n")
 }
 object@strataVar <- match(value, cn)
 if (length(object@set) == 0 || !"strataVar" %in% object@set)
   object@set <- c(object@set, "strataVar")
 return(object)
})

