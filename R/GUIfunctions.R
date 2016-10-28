#' Creates a household level file from a dataset with a household structure.
#'
#' It removes individual level variables and selects one record per household based on a household ID. The function can also be used for other hierachical structures.
#'
#' @param dat a data.frame with the full dataset
#' @param hhId name of the variable with the household (cluster) ID
#' @param hhVars character vector with names of all household level variables
#' @param weightVar name of the variable with the sampling weights
#' @param weightType HH or IND specifying whether weightVar is the household-level or individual-level weight.
#' If the weights are at the individual level, the weights are aggregated by averaging over all individuals
#' within the household (cluster) and dividing by the number of individuals in the cluster.
#' @return a data.frame with only household level variables and one record per household
#' @author Thijs Benschop
#' @export
#'
#' @examples
#'
#' x <- testdata
#' x_hh <- selectHouseholdData(x, "ori_hid", c("urbrur", "roof",  "walls", "water", "electcon", "sampling_weight"), weightVar = "sampling_weight", weightType = "IND")
#'
selectHouseholdData <- function(dat, hhId, hhVars, weightVar = NULL, weightType = "HH") {
  # Check whether specified variables are available in the data
  if (any(!(hhVars %in% colnames(dat))))
    stop("Some selected household variables aren't available in the data. \n Respecify hhVars.")
  if (!(hhId %in% colnames(dat)))
    stop("The selected household ID isn't available in the data. \n Respecify hhId")
  if (!is.null(weightVar))
    if (!(weightVar %in% colnames(dat)))
      stop("The selected weight variable isn't available in the data. \n Respecify weightVar")
  
  # Check whether weightVar is also in hhVars, if so remove from hhVars
  if (!is.null(weightVar))
    if (weightVar %in% hhVars)
      hhVars <- hhVars[-which(hhVars == weightVar)]
    
    # Remove any records with missing household ID (these cannot be matched later on in the process)
    res <- dat[complete.cases(dat[,hhId]), ]
    
    # If the sampling weights are at the individual level, aggregate by averaging over all individuals
    # within the household (cluster) and dividing by the number of individuals in the cluster.
    if(weightType == "IND"){
      if (is.null(weightVar))
        stop("You didn't specify a weight variable. \n Specify weightVar")
      
      # Compute household level weight (average over individual weights in cluster divided by the number of individuals in cluster)
      res[,weightVar] <- sapply(res[,hhId], FUN = function(x) {sum(res[res[,hhId] == x, weightVar]) / (length(res[res[,hhId] == x, weightVar])^2)})
    }
    
    # Keep only one observation per household
    res <- res[which(!duplicated(res[,hhId])),]
    
    # Drop all variables that are not at the household level
    res <- res[,c(hhId, hhVars, weightVar)]
    
    return(res)
}

#' Generate one strata variable from multiple factors
#'
#' For strata defined by multiple variables (e.g. sex,age,country) one combined
#' variable is generated.
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
    strata <- paste(strata, df[, stratavars[i]], sep="")
    if (length(stratavars) > i) {
      strata <- paste(strata, "-", sep="")
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
#' @docType methods
#' @param obj object of class \code{\link{sdcMicroObj-class}}
#' @param var name of the variable(s) to be remove
#' @return the modified \code{\link{sdcMicroObj-class}}
#' @author Alexander Kowarik
#' @keywords methods
#' @export
#' @examples
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata, keyVars=c('urbrur','roof'),
#'  numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- removeDirectID(sdc, var="age")
removeDirectID <- function(obj, var) {
  removeDirectIDX(obj=obj, var=var)
}

setGeneric("removeDirectIDX", function(obj, var) {
  standardGeneric("removeDirectIDX")
})

setMethod(f="removeDirectIDX", signature=c("sdcMicroObj"),
definition=function(obj, var) {
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
  o <- o[, !colnames(o) %in% var, drop=FALSE]
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
#' @name varToFactor
#' @docType methods
#' @param obj object of class \code{\link{sdcMicroObj-class}}
#' @param var name of the keyVariable to change
#' @return the modified \code{\link{sdcMicroObj-class}}
#' @keywords methods
#' @export
#' @examples
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- varToFactor(sdc, var="urbrur")
#'
varToFactor <- function(obj, var) {
  varToFactorX(obj=obj, var=var)
}
setGeneric("varToFactorX", function(obj, var) {
  standardGeneric("varToFactorX")
})

setMethod(f="varToFactorX", signature=c("sdcMicroObj"),
definition=function(obj, var) {
  obj <- nextSdcObj(obj)
  x <- get.sdcMicroObj(obj, type="manipKeyVars")
  x2 <- varToFactor(x, var=var)
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(as.data.frame(x2)))
  obj
})

setMethod(f="varToFactorX", signature=c("data.frame"),
definition=function(obj, var) {
  #if ( length(var)!=1) {
  #  stop("More than 1 variable specified in 'var'!\n")
  #}
  if (!all(var %in% colnames(obj))) {
    stop("at least one variable specified in 'var' is not available in 'obj'!\n")
  }
  for (vv in var) {
    obj[[vv]] <- as.factor(obj[[vv]])
  }
  obj
  obj
})


#' @export
#' @rdname varToFactor
varToNumeric <- function(obj, var) {
  varToNumericX(obj=obj, var=var)
}

setGeneric("varToNumericX", function(obj, var) {
  standardGeneric("varToNumericX")
})

setMethod(f="varToNumericX", signature=c("sdcMicroObj"),
definition=function(obj, var) {
  obj <- nextSdcObj(obj)
  x <- get.sdcMicroObj(obj, type="manipKeyVars")
  suppressWarnings(tmpvar <- as.numeric(as.character(x[, var])))
  x2 <- varToNumeric(x, var=var)
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(as.data.frame(x2)))
  obj
})

setMethod(f="varToNumericX", signature=c("data.frame"),
definition=function(obj, var) {
  if (!all(var %in% colnames(obj))) {
    stop("at least one variable specified in 'var' is not available in 'obj'!\n")
  }
  for (vv in var) {
    obj[[vv]] <- as.numeric(obj[[vv]])
  }
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
    res <- tryCatchFn(read_sas(data_file=path))
  }
  if (type=="spss") {
    res <- tryCatchFn(read_spss(file=path))
  }
  if (type=="stata") {
    res <- tryCatchFn(read_dta(file=path))
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

#' importProblem
#'
#' reads an sdcProblem with code that has been exported within \code{\link{sdcGUI}}.
#'
#' @param path a file path
#' @return an object of class \code{sdcMicro_GUI_export} or an object of class 'simple.error'
#' @author Bernhard Meindl
#' @export
importProblem <- function(path) {
  res <- tryCatchFn(get(load(file=path)))
  if ( "simpleError" %in% class(res) ) {
    return(res)
  } else {
    if (!"sdcMicro_GUI_export" %in% class(res)) {
      res$message <- paste0(res$message,"\ndata read into the system was not of class 'sdcMicro_GUI_export'!")
      return(res)
    }
  }
  res
}

#' subsetMicrodata
#'
#' allows to restrict original data to only a subset. This may be useful to test some anonymization
#' methods. This function will only be used in the graphical user interface \code{\link{sdcGUI}}.
#'
#' @param obj an object of class \code{\link{data.frame}} containing micro data
#' @param type algorithm used to sample from original microdata. Currently supported choices are
#' \itemize{
#' \item \code{n_perc}{ the restricted microdata will be a \code{n-percent} sample of the original microdata.}
#' \item \code{first_n}{ only the first \code{n} observations will be used.}
#' \item \code{every_n}{ the restricted microdata set consists of every \code{n-th} record.}
#' \item \code{size_n}{ a total of \code{n} observations will be randomly drawn.}
#' }
#' @param n numeric vector of length 1 specifying the specific parameter with respect to argument \code{type}.
#' @return an object of class \code{\link{sdcMicroObj-class}} with modified slot \code{@origData}.
#' @author Bernhard Meindl
#' @rdname subsetMicrodata
subsetMicrodata <- function(obj, type, n) {
  if (!type %in% c("n_perc","first_n","every_n","size_n")) {
    stop("invalid value in argument 'type'\n")
  }
  if (n < 1) {
    stop("argument 'n' must be >=1\n")
  }

  dat <- obj
  nrObs <- nrow(dat)
  if (type=="n_perc") {
    ssize <- ceiling((nrObs/100)*n)
    dat <- dat[sample(1:nrObs, ssize),,drop=FALSE]
  }
  if (type=="first_n") {
    dat <- dat[1:n,,drop=F]
  }
  if (type=="every_n") {
    ssize <- (1:nrObs)%%n==1
    dat <- dat[ssize,,drop=F]
  }
  if (type=="size_n") {
    dat <- dat[sample(1:nrObs, n),,drop=F]
  }
  dim(dat)
  return(dat)
}


#' writeSafeFile
#'
#' writes an anonymized dataset to a file. This function should be used in the
#' graphical user interface \code{\link{sdcGUI}} only.
#'
#' @param obj an object of class \code{\link{data.frame}} containing micro data
#' @param randomizeRecords (logical) specifies, if the output records should be randomized. The following
#' options are possible:
#' \itemize{
#' \item {'no'}{default, no randomization takes place}
#' \item {'simple'}{records are just randomly swapped.}
#' \item {'byHH'}{if slot 'hhId' is not \code{NULL}, the clusters defined by this variable are randomized across the dataset. If
#' slot 'hhId' is \code{NULL}, the records or the dataset are randomly changed.}
#' \item {'withinHH'}{if slot 'hhId' is not \code{NULL}, the clusters defined by this variable are randomized across the dataset and
#' additionally, the order of records within the clusters are also randomly changed. If slot 'hhId' is \code{NULL}, the records or the dataset are
#' randomly changed.}}
#' @param format (character) specifies the output file format. Accepted values are:
#' \itemize{
#' \item {'rdata'}{output will be saved in the R binary file-format.}
#' \item {'sav'}{output will be saved as SPSS-file.}
#' \item {'dta'}{ouput will be saved as STATA-file.}
#' \item {'csv'}{output will be saved as comma seperated (text)-file.}}
#' @param fileOut (character) file to which output should be written
#' @param ... optional arguments used for \code{write.table} if argument \code{format} equals \code{csv}
#' @return NULL
#' @author Bernhard Meindl
#' @rdname writeSafeFile
#' @export
writeSafeFile <- function(obj, format, randomizeRecords, fileOut, ...) {
  if (!class(obj)=="sdcMicroObj") {
    stop("invalid input in argument 'obj'\n")
  }
  dat <- extractManipData(obj, randomizeRecords=randomizeRecords)

  if (format=="rdata") {
    save(dat, file=fileOut)
  }
  if (format=="sav") {
    write_sav(data=dat, path=fileOut)
  }
  if (format=="dta") {
    write_dta(data=dat, path=fileOut)
  }
  if (format=="csv") {
    inp <- list(...)
    write.table(dat, file=fileOut, col.names=inp$col.names, sep=inp$sep, dec=inp$dec)
  }
  return(invisible(NULL))
}
