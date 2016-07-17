#' Creates new randomized IDs
#'
#' This is useful if the record IDs consist, for example, of a geo identifier and the household line number.
#' This method can be used to create new, random IDs that cannot be reconstructed.
#'
#' @param obj an object of class \code{sdcMicroObj}
#' @param newID a character specifiying the desired variable name of the new ID
#' @param withinVar if not \code{NULL} a character vector specifying a variable (e.g an existing household ID) which
#' will be used when calculating the new IDs. If specified, the same IDs will be assigned to the same values of the given variable.
#'
#' @return an object of class \code{sdcMicroObj} with updated slot \code{origData}
#'
#' @export
#' @docType methods
#' @rdname createNewID
setGeneric("createNewID", function(obj, newID, withinVar) {
  standardGeneric("createNewID")
})

#' @rdname createNewID
#' @export
setMethod(f="createNewID", signature=c("sdcMicroObj", "character", "characterOrNULL"),
definition=function(obj, newID, withinVar=NULL) {
  tmpOrder <- xx <- NULL
  origData <- get.sdcMicroObj(obj, "origData")
  cn <- colnames(origData)
  if (newID %in% cn) {
    stop("please specify a valid name for your new, randomized ID variable!\n")
  }

  if (!is.null(withinVar) && !withinVar %in% cn) {
    stop("Variable specified in parameter 'withinVar' is not available in the original micro data!")
  }

  if (!is.null(withinVar)) {
    # same value for newID for each value of withinVar
    vv <- data.table(origData[[withinVar]])
    vv[,tmpOrder:=1:.N]
    setnames(vv, c(withinVar, "tmpOrder"))

    uu <- unique(vv, by=withinVar)
    uu[,xx:=sample(1:nrow(uu))]
    uu[,tmpOrder:=NULL]
    setnames(uu, c(withinVar, newID))

    setkeyv(vv, withinVar)
    setkeyv(uu, withinVar)

    vv <- merge(vv, uu, all.x=TRUE)
    setkey(vv, tmpOrder)
    origData[[newID]] <- vv[[newID]]
  } else {
    # random ID
    origData[[newID]] <- sample(1:nrow(origData))
  }
  obj <- set.sdcMicroObj(obj, type="origData", input=list(origData))
  return(obj)
})
