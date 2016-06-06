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
  o <- o[, !colnames(o) %in% var, drop = FALSE]
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

#' @rdname varToFactor
#' @export
setMethod(f = "varToFactor", signature = c("sdcMicroObj"),
definition = function(obj, var) {
  x <- get.sdcMicroObj(obj, type="manipKeyVars")
  x2 <- varToFactor(x, var=var)
  obj <- nextSdcObj(obj)
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(as.data.frame(x2)))
  obj
})

#' @rdname varToFactor
#' @export
setMethod(f="varToFactor", signature=c("data.frame"),
definition=function(obj, var) {
  if ( length(var)!=1) {
    stop("More than 1 variable specified in 'var'!\n")
  }
  if ( !var %in% colnames(obj)) {
    stop("variable specified in 'var' not available in 'obj'!\n")
  }
  obj[[var]] <- as.factor(obj[[var]])
  obj
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


#' Join levels of a variables in an object of class
#' \code{\link{sdcMicroObj-class}} or \code{factor} or \code{data.frame}
#'
#' If the input is an object of class \code{\link{sdcMicroObj-class}}, the
#' specified factor-variable is recoded into a factor with less levels and
#' risk-measures are automatically recomputed.
#'
#' If the input is of class \code{data.frame}, the result is a \code{data.frame} with
#' a modified column specified by \code{var}.
#'
#' If the input is of class \code{factor}, the result is a \code{factor} with different
#' levels.
#'
#' @name groupAndRename
#' @aliases groupAndRename groupAndRename,data.frame-method,
#' groupAndRename,factor-method,groupAndRename,sdcMicroObj-method
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
#' @author Bernhard Meindl
#' @export
#' @examples
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' testdata2$urbrur <- as.factor(testdata2$urbrur)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- groupAndRename(sdc, var="urbrur", before=c("1","2"), after=c("1"))
setGeneric("groupAndRename", function(obj, var, before, after) {
  standardGeneric("groupAndRename")
})

#' @rdname groupAndRename
#' @export
setMethod(f="groupAndRename", signature=c("factor"),
definition=function(obj, var, before, after) {
  if (!all(before %in% levels(obj))) {
    stop("some elements of 'before' are not valid levels in variable 'var'!\n")
  }
  if (any(duplicated(before))) {
    stop("each level from the original factor must be listed only once in argument 'before'!")
  }
  ll <- levels(obj)
  ll[ll %in% before] <- after[1]
  levels(obj) <- ll
  obj
})

#' @rdname groupAndRename
#' @export
setMethod(f="groupAndRename", signature=c("data.frame"),
definition=function(obj, var, before, after) {
  if (length(var) != 1) {
    stop("length of input 'var' != 1!\n")
  }
  if (!var %in% colnames(obj)) {
    stop("variable specified in 'var' is not available in 'obj'!\n")
  }
  fac <- obj[[var]]
  if (!is.factor(obj[[var]]) ) {
    stop("check input, we do not have a factor here!\n")
  }
  obj[[var]] <- groupAndRename(obj[[var]], var=NULL, before=before, after=after)
  obj
})

#' @rdname groupAndRename
#' @export
setMethod(f="groupAndRename", signature=c("sdcMicroObj"),
definition=function(obj, var, before, after) {
  x <- get.sdcMicroObj(obj, type="manipKeyVars")
  x <- groupAndRename(x, var=var, before=before, after=after)
  obj <- nextSdcObj(obj)
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(x))
  obj
})


# wrapper for tryCatch()
tryCatchFn <- function(expr) {
  result <- tryCatch({expr},
   error=function(e) {
     return(e)
   })
  return(result)
}

#' readMicrodata
#'
#' reads data from various formats into R. Used by default in \code{\link{sdcGUI}}.
#'
#' @param path a file path
#' @param type which format does the file have. currently allowed values are
#' \itemize{
#' \item \code{sas}
#' \item \code{spss}
#' \item \code{stata}
#' \item \code{R}
#' \item \code{rdf}
#' \item \code{csv}
#' }
#' @param convertCharToFac (logical) if TRUE, all character vectors are automatically
#' converted to factors
#' @param drop_all_missings (logical) if TRUE, all variables that contain NA-values only
#' will be dropped
#' @param ... additional parameters. Currently used only if \code{type='csv'} to pass
#' arguments to \code{read.table()}.
#'
#' @return a data.frame or an object of class 'simple.error'
#' @author Bernhard Meindl
#' @export
readMicrodata <- function(path, type, convertCharToFac=TRUE, drop_all_missings=TRUE, ...) {
  if (type=="sas") {
    res <- tryCatchFn(read_sas(b7dat=path))
  }
  if (type=="spss") {
    res <- tryCatchFn(read_spss(path=path))
  }
  if (type=="stata") {
    res <- tryCatchFn(read_dta(path=path))
  }
  if (type=="R") {
    res <- tryCatchFn(get(load(file=path)))
  }
  if (type=="rdf") {
    res <- tryCatchFn(get(paste(path)))
  }
  if (type=="csv") {
    opts <- list(...)
    header <- ifelse(opts$header==TRUE, TRUE, FALSE)
    sep <- opts$sep
    res <- tryCatchFn(read.table(path, sep=sep, header=header))
  }
  if ( "simpleError" %in% class(res) ) {
    return(res)
  } else {
    if (!"data.frame" %in% class(res)) {
      res$message <- paste0(res$message,"\ndata read into the system was not of class 'data.frame'!")
      return(res)
    }
    # convert result to clas 'data.frame' if it is a 'tbl_df'...
    if ("tbl_df" %in% class(res)) {
      class(res) <- "data.frame"
    }
    # check if any variable has class 'labelled' and convert it to factors.
    # this might happen if we read data with read_xxx() from haven
    cl_lab <- which(sapply(res, class)=="labelled")
    if (length(cl_lab) > 0) {
      if (length(cl_lab)==1) {
        res[[cl_lab]] <- as_factor(res[[cl_lab]])
      } else {
        res[,cl_lab] <- lapply(res[,cl_lab] , as_factor)
      }
    }

    if (convertCharToFac) {
      # convert character-variables to factors
      cl_char <- which(sapply(res, class)=="character")
      if (length(cl_char) >0) {
        if (length(cl_char) == 1) {
          res[[cl_char]] <- as.factor(res[[cl_char]])
        } else {
          res[,cl_char] <- lapply(res[,cl_char], as.factor)
        }
      }
    }
    if (drop_all_missings) {
      # drop all variables that are NA-only
      keep <- which(sapply(res, function(x) sum(is.na(x))!=length(x)))
      res <- res[,keep,drop=FALSE]
    }
  }
  res
}

