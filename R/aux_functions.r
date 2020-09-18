# obj: an sdcMicroObj-object
# v: either a numeric vector specifying column-indices or a character vector specifying
# column-names
standardizeInput <- function(obj, v) {
  if (!inherits(obj, "sdcMicroObj")) {
    stop("obj must be an object of class 'sdcMicroObj'!", call. = FALSE)
  }
  if (is.null(v)) {
    return(NULL)
  }

  if (is.numeric(v)) {
    if (all(v %in% 1:ncol(get.sdcMicroObj(obj, type = "origData")))) {
      return(v)
    } else {
      stop("please specify valid column-indices!", call. = FALSE)
    }
  } else if (is.character(v)) {
    m <- match(v, colnames(get.sdcMicroObj(obj, type = "origData")))
    if (!any(is.na(m))) {
      return(m)
    } else {
      stop("please specify valid column-names!", call. = FALSE)
    }
  } else {
    stop(
      "please specify either a numeric vector specifying column-indices or a character vector containing valid variable names!",
      call. = FALSE
    )
  }
}

#' @name sdcMicroObj-class
#' @param dat The microdata set. A numeric matrix or data frame containing the data.
#' @param keyVars Indices or names of categorical key variables. They must, of
#' course, match with the columns of \sQuote{dat}.
#' @param pramVars Indices or names of categorical variables considered to be pramed.
#' @param numVars Index or names of continuous key variables.
#' @param ghostVars if specified a list which each element being a list of exactly two elements.
#' The first element must be a character vector specifying exactly one variable name that was
#' also specified as a categorical key variable (\code{keyVars}), while the second element is
#' a character vector of valid variable names (that must not be listed as \code{keyVars}).
#' If \code{\link{localSuppression}} or \code{\link{kAnon}} was applied, the resulting
#' suppression pattern for each key-variable is transferred to the depending variables.
#' @param weightVar Indices or name determining the vector of sampling weights.
#' @param hhId Index or name of the cluster ID (if available).
#' @param strataVar Indices or names of stratification variables.
#' @param sensibleVar Indices or names of sensible variables (for l-diversity)
#' @param excludeVars which variables of \code{dat} should not be included in
#' result-object? Users may specify a vector of variable-names available in \code{dat}
#' that were not specified in either \code{keyVars}, \code{numVars}, \code{pramVars},
#' \code{ghostVars}, \code{hhId}, \code{strataVar} or \code{sensibleVar}.
#' @param options additional options (if specified, a list must be used as input)
#' @param seed (numeric) number specifiying the seed which will be set to allow for
#' reproducablity. The number will be rounded and saved as element \code{seed} in slot \code{options}.
#' @param randomizeRecords (logical) if \code{TRUE}, the order of observations in the input microdata set
#' will be randomized.
#' @param alpha numeric between 0 and 1 specifying the fraction on how much keys containing \code{NAs} should
#' contribute to the frequency calculation which is also crucial for risk-estimation.
#' @references Templ, M. and Meindl, B. and Kowarik, A.: \emph{Statistical Disclosure Control for
#' Micro-Data Using the R Package sdcMicro}, Journal of Statistical Software,
#' 67 (4), 1--36, 2015. \doi{10.18637/jss.v067.i04}
#' @export
#' @examples
#' ## we can also specify ghost (linked) variables
#' ## these variables are linked to some categorical key variables
#' ## and have the sampe suppression pattern as the variable that they
#' ## are linked to after \code{\link{localSuppression}} has been applied
#' data(testdata)
#' testdata$electcon2 <- testdata$electcon
#' testdata$electcon3 <- testdata$electcon
#' testdata$water2 <- testdata$water
#'
#' keyVars <- c("urbrur","roof","walls","water","electcon","relat","sex")
#' numVars <- c("expend","income","savings")
#' w <- "sampling_weight"
#'
#' ## we want to make sure that some variables not used as key-variables
#' ## have the same suppression pattern as variables that have been
#' ## selected as key variables. Thus, we are using 'ghost'-variables.
#' ghostVars <- list()
#'
#' ## we want variables 'electcon2' and 'electcon3' to be linked
#' ## to key-variable 'electcon'
#' ghostVars[[1]] <- list()
#' ghostVars[[1]][[1]] <- "electcon"
#' ghostVars[[1]][[2]] <- c("electcon2","electcon3")
#'
#' \dontrun{
#' # dontrun because Examples with CPU time > 2.5 times elapsed time
#' ## we want variable 'water2' to be linked to key-variable 'water'
#' ghostVars[[2]] <- list()
#' ghostVars[[2]][[1]] <- "water"
#' ghostVars[[2]][[2]] <- "water2"
#'
#' ## create the sdcMicroObj
#' obj <- createSdcObj(testdata, keyVars=keyVars,
#'   numVars=numVars, w=w, ghostVars=ghostVars)
#'
#' ## apply 3-anonymity to selected key variables
#' obj <- kAnon(obj, k=3); obj
#'
#' ## check, if the suppression patterns are identical
#' manipGhostVars <- get.sdcMicroObj(obj, "manipGhostVars")
#' manipKeyVars <- get.sdcMicroObj(obj, "manipKeyVars")
#' all(is.na(manipKeyVars$electcon) == is.na(manipGhostVars$electcon2))
#' all(is.na(manipKeyVars$electcon) == is.na(manipGhostVars$electcon3))
#' all(is.na(manipKeyVars$water) == is.na(manipGhostVars$water2))
#'
#' ## exclude some variables
#' obj <- createSdcObj(testdata, keyVars=c("urbrur","roof","walls"), numVars="savings",
#'    weightVar=w, excludeVars=c("relat","electcon","hhcivil","ori_hid","expend"))
#' colnames(get.sdcMicroObj(obj, "origData"))
#' }
createSdcObj <- function(dat, keyVars, numVars=NULL, pramVars=NULL, ghostVars=NULL, weightVar=NULL,
  hhId=NULL, strataVar=NULL, sensibleVar=NULL, excludeVars=NULL, options=NULL, seed=NULL,
  randomizeRecords=FALSE, alpha=1) {
  obj <- new("sdcMicroObj")
  options <- list()
  if (!is.null(seed) && is.numeric(seed)) {
    ss <- round(seed)
    set.seed(ss)
    options$seed <- ss
  } else {
    options$seed <- NA
  }

  if (!is.data.frame(dat)) {
    dat <- as.data.frame(dat)
  }
  class(dat) <- "data.frame" # removing e.g data.table attributes

  if (randomizeRecords==TRUE) {
    dat <- dat[sample(1:nrow(dat)),]
    rownames(dat) <- NULL
  }
  options$randomizeRecords <- randomizeRecords
  obj <- set.sdcMicroObj(obj, type="origData", input=list(dat))

  usedVars <- c(standardizeInput(obj, keyVars),
    standardizeInput(obj, numVars), standardizeInput(obj, pramVars),
    standardizeInput(obj, weightVar), standardizeInput(obj, hhId),
    standardizeInput(obj, strataVar), standardizeInput(obj, sensibleVar))
  if (!is.null(ghostVars)) {
    for (i in seq_along(ghostVars)) {
      usedVars <- c(usedVars, standardizeInput(obj, ghostVars[[i]][[2]]))
    }
  }
  usedVars <- unique(usedVars)
  # exclude variables if required
  if (!is.null(excludeVars)) {
    excludeVarsInd <- standardizeInput(obj, excludeVars)
    if (any(excludeVarsInd %in% usedVars)) {
      stop("You have specified variables in 'excludeVars' that cannot be removed!\n")
    }
    obj@origData <- obj@origData[,-c(excludeVarsInd),drop=FALSE]
    obj@deletedVars <- excludeVars
    dat <- get.sdcMicroObj(obj, type="origData")
  }

  # key-variables
  keyVarInd <- standardizeInput(obj, keyVars)
  TFcharacter <- lapply(dat[, keyVarInd, drop=FALSE], class) %in% "character"
  if (any(TFcharacter)) {
    for (kvi in which(TFcharacter)) {
      dat[, keyVarInd[kvi]] <- as.factor(dat[, keyVarInd[kvi]])
    }
  }

  obj <- set.sdcMicroObj(obj, type="keyVars", input=list(keyVarInd))
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(dat[, keyVarInd, drop=FALSE]))

  if (!is.null(pramVars)) {
    pramVarInd <- standardizeInput(obj, pramVars)
    obj <- set.sdcMicroObj(obj, type="pramVars", input=list(pramVarInd))

    # variable only consists of NA values?
    all.na <- which(sapply(obj@origData[, pramVars], function(x) {
      all(is.na(x))
    }))
    if (length(all.na) > 0) {
      warnMsg <- "at least one pramVar only contains NA values! --> we do not use this variable!\n"
      obj <- addWarning(obj, warnMsg=warnMsg, method="createSdcObj", variable=NA)
      warning(warnMsg)
      obj <- set.sdcMicroObj(obj, type="pramVars", list(get.sdcMicroObj(obj, type="pramVars")[-all.na]))
      pramVarInd <- pramVarInd[-all.na]
    }
    pramData <- dat[, pramVarInd, drop=FALSE]
    obj <- set.sdcMicroObj(obj, type="manipPramVars", input=list(dat[, pramVarInd,
      drop=FALSE]))
  }
  # numeric-variables
  if (!is.null(numVars)) {
    numVarInd <- standardizeInput(obj, numVars)
    obj <- set.sdcMicroObj(obj, type="numVars", input=list(numVarInd))
    obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(dat[, numVarInd, drop=FALSE]))
  }

  # ghostVars
  if ( !is.null(ghostVars) ) {
    for ( i in seq_along(ghostVars) ) {
      gV <- standardizeInput(obj, ghostVars[[i]][[1]])
      sV <- standardizeInput(obj, ghostVars[[i]][[2]])
      if ( length(gV) != 1 ) {
        stop("only one (existing) key variable name can be specified as idenpendent variables in a ghostVars-element!\n")
      }
      if ( any(sV %in% keyVarInd) ) {
        stop("one variables that are no categorical key variables can be specified as dependent variables in a ghostVars element.\n")
      }
      ghostVars[[i]][[1]] <- gV
      ghostVars[[i]][[2]] <- sV
    }
    obj <- set.sdcMicroObj(obj, type="ghostVars", input=list(ghostVars))
    ghostVarInd <- unlist(lapply(ghostVars, function(x) { x[[2]]}))
    obj <- set.sdcMicroObj(obj, type ="manipGhostVars", input=list(dat[, ghostVarInd, drop=FALSE]))
  }

  # weight-variable
  if (!is.null(weightVar)) {
    weightVarInd <- standardizeInput(obj, weightVar)
    obj <- set.sdcMicroObj(obj, type="weightVar", input=list(weightVarInd))
  }
  # hhId-variable
  if (!is.null(hhId)) {
    hhIdInd <- standardizeInput(obj, hhId)
    obj <- set.sdcMicroObj(obj, type="hhId", input=list(hhIdInd))
  }
  # strata-variable
  if (!is.null(strataVar)) {
    strataVarInd <- standardizeInput(obj, strataVar)
    obj <- set.sdcMicroObj(obj, type="strataVar", input=list(strataVarInd))
  }
  # sensible-variable
  if (!is.null(sensibleVar)) {
    sensibleVarInd <- standardizeInput(obj, sensibleVar)
    obj <- set.sdcMicroObj(obj, type="sensibleVar", input=list(sensibleVarInd))
  }
  options$alpha <- alpha
  obj <- set.sdcMicroObj(obj, type="options", input=list(options))

  obj <- measure_risk(obj)
  obj@originalRisk <- obj@risk

  if (length(numVars) > 0) {
    obj <- dRisk(obj)
    obj <- dUtility(obj)
  }
  obj
}

computeNumberPrev <- function(obj) {
  tmpo <- obj
  for (i in 1:1000) {
    tmpo <- tmpo@prev
    if (is.null(tmpo)) {
      return(i - 1)
    }
  }
}

deletePrevSave <- function(obj, m) {
  nprev <- computeNumberPrev(obj)
  if (m >= 1 && m <= nprev) {
    cmd <- paste("obj@", paste(rep("prev", m), collapse="@"), "<-NULL", sep="")
    eval(parse(text=cmd))
  }
  return(obj)
}


#' nextSdcObj
#'
#' internal function used to provide the undo-functionality.
#'
#' @param obj a \code{\link{sdcMicroObj-class}} object
#' @return a modified \code{\link{sdcMicroObj-class}} object
#' @export
#' @docType methods
nextSdcObj <- function(obj) {
  nextSdcObjX(obj)
}
setGeneric("nextSdcObjX", function(obj) {
  standardGeneric("nextSdcObjX")
})

setMethod(f="nextSdcObjX", signature=c("sdcMicroObj"), definition=function(obj) {
  options <- get.sdcMicroObj(obj, type="options")
  if (("noUndo" %in% options)) {
    return(obj)
  }
  if (nrow(obj@origData) > 1e+05) {
    warnMsg <- "No previous states are saved because your data set has more than 100 000 observations.\n"
    obj <- addWarning(obj, warnMsg=warnMsg, method="nextSdcObj", variable=NA)
    warning(warnMsg)
    return(obj)
  }
  if (length(grep("maxUndo", options)) > 0)
    maxUndo <- as.numeric(substr(options[grep("maxUndo", options)], 9, stop=nchar(options[grep("maxUndo",
      options)], type="width"))) else maxUndo <- 1
  obj <- deletePrevSave(obj, maxUndo)
  obj <- set.sdcMicroObj(obj, type="prev", input=list(obj))
  return(obj)
})

#' Recompute Risk and Frequencies for a sdcMicroObj
#'
#' Recomputation of Risk should be done after manual changing the content of an
#' object of class \code{\link{sdcMicroObj-class}}
#'
#' By applying this function, the dislosure risk is re-estimated and the
#' corresponding slots of an object of class \code{\link{sdcMicroObj-class}} are updated.
#' This function mostly used internally to automatically update the risk after
#' an sdc method is applied.
#'
#' @name calcRisks
#' @param obj an object of class \code{\link{sdcMicroObj-class}}
#' @param ... no arguments at the moment
#' @seealso \code{\link{sdcMicroObj-class}}
#' @export
#' @examples
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- calcRisks(sdc)
#'
calcRisks <- function(obj, ...) {
  calcRisksX(obj=obj, ...)
}

setGeneric("calcRisksX", function(obj, ...) {
  standardGeneric("calcRisksX")
})

setMethod(f="calcRisksX", signature=c("sdcMicroObj"), definition=function(obj, ...) {
  risk <- get.sdcMicroObj(obj, type="risk")
  modelSet <- (!is.null(risk$model))
  suda2Set <- (!is.null(risk$suda2))
  obj <- measure_risk(obj)
  if (modelSet) {
    kv <- colnames(get.sdcMicroObj(obj, type = "manipKeyVars"))
    obj <- modRisk(
      obj = obj,
      method = "IPF",
      formulaM = as.formula(paste(" ~ ", paste(kv, collapse="+"))))
  }
  if (suda2Set) {
    obj <- suda2(obj)
  }
  if (length(get.sdcMicroObj(obj, type="manipNumVars")) > 0) {
    obj <- dRisk(obj)
  }
  obj
})

#' Remove certain variables from the data set inside a sdc object.
#'
#' Extract the manipulated data from an object of class \code{\link{sdcMicroObj-class}}
#'
#' @name extractManipData
#' @param obj object of class \code{\link{sdcMicroObj-class}}
#' @param ignoreKeyVars If manipulated KeyVariables should be returned or the
#' unchanged original variables
#' @param ignorePramVars if manipulated PramVariables should be returned or the
#' unchanged original variables
#' @param ignoreNumVars if manipulated NumericVariables should be returned or
#' the unchanged original variables
#' @param ignoreGhostVars if manipulated Ghost (linked) Variables should be returned or
#' the unchanged original variables
#' @param ignoreStrataVar if manipulated StrataVariables should be returned or
#' the unchanged original variables
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
#' @return a \code{data.frame} containing the anonymized data set
#' @author Alexander Kowarik, Bernhard Meindl
#' @export
#' @examples
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('urbrur','roof'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- removeDirectID(sdc, var="age")
#' dataM <- extractManipData(sdc)
extractManipData <- function(obj, ignoreKeyVars=FALSE, ignorePramVars=FALSE, ignoreNumVars=FALSE, ignoreGhostVars=FALSE, ignoreStrataVar=FALSE, randomizeRecords="no") {
  extractManipDataX(obj, ignoreKeyVars=ignoreKeyVars, ignorePramVars=ignorePramVars, ignoreNumVars=ignoreNumVars,
    ignoreGhostVars=ignoreGhostVars, ignoreStrataVar=ignoreStrataVar, randomizeRecords=randomizeRecords)
}

setGeneric("extractManipDataX", function(obj, ignoreKeyVars=FALSE, ignorePramVars=FALSE,
  ignoreNumVars=FALSE, ignoreGhostVars=FALSE, ignoreStrataVar=FALSE, randomizeRecords="no") {
  standardGeneric("extractManipDataX")
})

setMethod(f="extractManipDataX", signature=c("sdcMicroObj"), definition=function(obj,
  ignoreKeyVars=FALSE, ignorePramVars=FALSE, ignoreNumVars=FALSE,
  ignoreGhostVars=FALSE, ignoreStrataVar=FALSE, randomizeRecords="no") {
  hhid <- clusterid <- newid <- N <- id <- NULL
  if (!randomizeRecords %in% c("no","simple","byHH", "withinHH")) {
    stop("invalid value in argument 'randomizeRecords'\n")
  }
  o <- get.sdcMicroObj(obj, type="origData")
  k <- get.sdcMicroObj(obj, type="manipKeyVars")
  p <- get.sdcMicroObj(obj, type="manipPramVars")
  n <- get.sdcMicroObj(obj, type="manipNumVars")
  g <- get.sdcMicroObj(obj, type="manipGhostVars")
  s <- get.sdcMicroObj(obj, type="manipStrataVar")
  origKeys <- o[,colnames(k)]
  if (!is.null(k) && !ignoreKeyVars)
    o[, colnames(k)] <- k
  if (!is.null(p) && !ignorePramVars)
    o[, colnames(p)] <- p
  if (!is.null(n) && !ignoreNumVars)
    o[, colnames(n)] <- n
  if (!is.null(g) && !ignoreGhostVars)
    o[, colnames(g)] <- g
  if (!is.null(s) && !ignoreStrataVar)
    o$sdcGUI_strataVar <- s
  ## quick and dirty: ensure that keyVars are factors:
  if (!is.null(k) && !ignoreKeyVars) {
    for (i in 1:length(colnames(k))) {
      cc <- class(origKeys[, colnames(k)[i]])
      v_p <- o[,colnames(k)[i]]
      if (cc!=class(v_p)) {
        if (cc=="integer") {
          o[,colnames(k)[i]] <- as.integer(v_p)
        }
        if (cc=="character") {
          o[,colnames(k)[i]] <- as.character(v_p)
        }
        if (cc=="numeric") {
          o[,colnames(k)[i]] <- as.numeric(v_p)
        }
        if (cc=="logical") {
          o[,colnames(k)[i]] <- as.logical(v_p)
        }
      }
    }
  }

  if (randomizeRecords!="no") {
    hhid <- get.sdcMicroObj(obj, "hhId")
    if (is.null(hhid) | randomizeRecords=="simple") {
      # just simple randomization
      o <- o[sample(1:nrow(o)),]
    } else {
      tmp <- data.table(id=1:nrow(o), clusterid=o[[hhid]])
      setkey(tmp, clusterid)

      if (randomizeRecords=="withinHH") {
        tmp <- tmp[,lapply(.SD, function(x) {
          if(length(x)==1) { return(x)} else { return(sample(x))}
        }), by=key(tmp), .SDcols="id"]
      }
      neworder <- tmp[,.N, by=key(tmp)]
      neworder[,clusterid:=sample(clusterid)]
      neworder[,newid:=.I]
      neworder[,N:=NULL]
      tmp <- tmp[neworder]

      if (randomizeRecords=="byHH") {
        setkey(tmp, newid, id)
      }
      if (randomizeRecords=="withinHH") {
        setkey(tmp, newid)
      }
      o <- o[tmp$id,]

      # randomize order of clusters
      if (randomizeRecords=="byHH") {
        setkey(tmp, newid, id)
        o <- o[tmp$id,]
      }
    }
    rownames(o) <- NULL
  }
  return(o)
})

addWarning <- function(obj, warnMsg, method, variable=NA) {
  if (!class(obj)=="sdcMicroObj") {
    stop("'obj' must be a of class 'sdcMicroObj'!\n")
  }
  if (!is.character(method)) {
    stop("'method' must be a of class 'character'!\n")
  }
  if (!is.character(warnMsg)) {
    stop("'warnMsg' must be a of class 'character'!\n")
  }
  addRes <- get.sdcMicroObj(obj, type="additionalResults")
  df <- data.frame(warnMsg=warnMsg, method=method, variable=variable, stringsAsFactors=FALSE)
  if (is.null(addRes)) {
    addRes <- list()
    addRes$sdcMicro_warnings <- df
  } else {
    if (is.null(addRes$sdcMicro_warnings)) {
      addRes$sdcMicro_warnings <- df
    } else {
      addRes$sdcMicro_warnings <- rbind(addRes$sdcMicro_warnings, df)
    }
  }
  obj <- set.sdcMicroObj(obj, type="additionalResults", list(addRes))
  obj
}
