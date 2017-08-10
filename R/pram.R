#' Post Randomization
#'
#' To be used on categorical data stored as factors. The algorithm randomly
#' changes the values of variables in selected records (usually the risky ones)
#' according to an invariant probability transition matrix or a custom-defined
#' transition matrix.
#'
#' @name pram
#' @docType methods
#' @param obj Input data. Allowed input data are objects of class
#' \code{data.frame}, \code{factor} or \code{\link{sdcMicroObj-class}}.
#' @param variables Names of variables in 'obj' on which post-randomization
#' should be applied. If obj is a factor, this argument is ignored. Please note that
#' pram can only be applied to factor-variables.
#' @param strata_variables Names of variables for stratification (will be set
#' automatically for an object of class \code{\link{sdcMicroObj-class}}. One can also specify
#' an integer vector or factor that specifies that desired groups. This vector must match the dimension
#' of the input data set, however. For a possible use case, have a look at the examples.
#' @param ...  further input, currently ignored.
#' @param pd minimum diagonal entries for the generated transition matrix P.
#' Either a vector of length 1 (which is recycled) or a vector of the same length as
#' the number of variables that should be postrandomized. It is also possible to set \code{pd}
#' to a numeric matrix. This matrix will be used directly as the transition matrix. The matrix must
#' be constructed as follows:
#' \itemize{
#' \item the matrix must be a square matrix
#' \item the rownames and colnames of the matrix must match the levels (in the same order) of the factor-variable that should be
#' postrandomized.
#' \item the rowSums and colSums of the matrix need to equal 1
#' }
#' It is also possible to combine the different ways. For details have a look at the examples.
#' @param alpha amount of perturbation for the invariant Pram method. This is a numeric vector
#' of length 1 (that will be recycled if necessary) or a vector of the same length as the number
#' of variables. If one specified as transition matrix directly, alpha is ignored.
#' @return a modified \code{\link{sdcMicroObj-class}} object or a new object containing
#' original and post-randomized variables (with suffix "_pram").
#' @author Alexander Kowarik, Matthias Templ, Bernhard Meindl
#' @references \url{http://www.gnu.org/software/glpk}
#' 
#' Kowarik, A. and Templ, M. and Meindl, B. and Fonteneau, F. and Prantner, B.:
#' \emph{Testing of IHSN Cpp Code and Inclusion of New Methods into sdcMicro},
#' in: Lecture Notes in Computer Science, J. Domingo-Ferrer, I. Tinnirello
#' (editors.); Springer, Berlin, 2012, ISBN: 978-3-642-33626-3, pp. 63-77. 
#' \doi{10.1007/978-3-642-33627-0_6}
#' 
#' Templ, M. and Kowarik, A. and Meindl, B. 
#' Statistical Disclosure Control for Micro-Data Using the R Package sdcMicro. 
#' \emph{Journal of Statistical Software}, \strong{67} (4), 1--36, 2015. \doi{10.18637/jss.v067.i04}
#'
#' Templ, M. Statistical Disclosure Control for Microdata: Methods and Applications in R.
#' \emph{Springer International Publishing}, 287 pages, 2017. ISBN 978-3-319-50272-4.
#' \doi{10.1007/978-3-319-50272-4}
#' 
#' @keywords manip
#' @export
#' @note Deprecated method 'pram_strata'is no longer available
#' in sdcMicro > 4.5.0
#' @examples
#' data(testdata)
#'
#' ## application on a factor-variable
#' res <- pram(as.factor(testdata$roof))
#' print(res)
#' summary(res)
#'
#' ## application on a data.frame
#' ## pram can only be applied to factors, thus we have to recode
#' ## to factors before the method can be applied
#' testdata$roof <- factor(testdata$roof)
#' testdata$walls <- factor(testdata$walls)
#' testdata$water <- factor(testdata$water)
#'
#' ## pram() is applied within subgroups defined by
#' ## variables "urbrur" and "sex"
#' res <- pram(testdata, variables="roof",
#'   strata_variables=c("urbrur","sex"))
#' print(res)
#' summary(res)
#'
#' ## default parameters (pd=0.8 and alpha=0.5) for the generation
#' ## of the invariant transition matrix will be used for all variables
#' res1 <- pram(testdata, variables=c("roof","walls","water"))
#' print(res1)
#'
#' ## specific parameters for each variable
#' res2 <- pram(testdata,variables=c("roof","walls","water"),
#'   pd=c(0.95,0.8,0.9), alpha=0.5)
#' print(res2)
#'
#' ## detailed information on pram-parameters (such as the transition matrix 'Rs')
#' ## is stored in the output, eg. for variable 'roof'
#' attr(res2, "pram_params")$roof
#'
#' ## we can also specify a custom transition-matrix directly
#' # for variable roof; matrix must have rownames and colnames that match
#' # the levels of the variable that should be post-randomized
#' # rowSums() and colSums() must equal 1 too!
#' mat <- diag(length(levels(testdata$roof)))
#' rownames(mat) <- colnames(mat) <- levels(testdata$roof)
#' res3 <- pram(testdata,variables="roof", pd=mat)
#' print(res3) # of course, nothing has changed!
#'
#' ## it is possible use a transistion matrix for a variable and use the 'traditional' way
#' ## of specifying a number for the minimal diagonal entries of the transision matrix
#' ## for other variables. In this case we must supply \code{pd} as list.
#' res4 <- pram(testdata,variables=c("roof","walls"), pd=list(mat,0.5), alpha=c(NA, 0.5))
#' print(res4)
#' summary(res4)
#' attr(res4, "pram_params")
#'
#' ## application to objects of class sdcMicro with default parameters
#' data(testdata2)
#' testdata2$urbrur <- factor(testdata2$urbrur)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- pram(sdc, variables=c("urbrur"))
#' print(sdc, type="pram")
#'
#' ## this is equal to the previous application. If argument 'variables' is NULL,
#' ## all variables from slot 'pramVars' will be used if possible.
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight',
#'   pramVars="urbrur")
#' sdc <- pram(sdc)
#' print(sdc, type="pram")
#'
#' ## we can specify transition matrices for sdcMicroObj-objects too
#' testdata2$roof <- factor(testdata2$roof)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' mat <- diag(length(levels(testdata2$roof)))
#' rownames(mat) <- colnames(mat) <- levels(testdata2$roof)
#' mat[1,] <- c(0.9,0,0,0.05,0.05)
#' sdc <- pram(sdc, variables="roof", pd=mat)
#' print(sdc, type="pram")
#' # we can also have a look at the transitions
#' get.sdcMicroObj(sdc, "pram")$transitions

pram <- function(obj, variables=NULL, strata_variables=NULL, pd=0.8, alpha=0.5) {
  pramX(obj=obj, variables=variables, strata_variables=strata_variables, pd=pd, alpha=alpha)
}

setGeneric("pramX", function(obj, variables=NULL, strata_variables=NULL, pd=0.8, alpha=0.5) {
  standardGeneric("pramX")
})

setMethod(f="pramX", signature=c("sdcMicroObj"),
definition=function(obj, variables=NULL, strata_variables=NULL, pd=0.8, alpha=0.5) {
  obj <- nextSdcObj(obj)
  pramVars <- get.sdcMicroObj(obj, type="pramVars")
  if (length(pramVars) == 0 && is.null(variables)) {
    stop("Error: slot pramVars is NULL and argument 'variables' was not specified!\nDefine one of them to use pram on these variables\n")
  }
  if (is.null(variables)) {
    pramVars <- colnames(obj@origData)[get.sdcMicroObj(obj, type="pramVars")]
  } else {
    pramVars <- variables
  }

  pp <- get.sdcMicroObj(obj, type="pram")
  if (!is.null(pp)) {
    if ( any(pramVars%in%pp$summary$variable)) {
      stop("pram() was already applied on at least one variable!\n")
    }
  }

  ### Get data from manipPramVars
  manipPramVars <- get.sdcMicroObj(obj, type="manipPramVars")
  strataVars <- get.sdcMicroObj(obj, type="strataVar")
  manipKeyVars <- get.sdcMicroObj(obj, type="manipKeyVars")
  kVar <- pramVars[pramVars %in% colnames(manipKeyVars)]
  pVar <- pramVars[pramVars %in% colnames(manipPramVars)]
  rVar <- pramVars[!pramVars %in% c(kVar, pVar)]

  if (length(kVar) > 0) {
    warnMsg <- "If pram is applied on key variables, the k-anonymity and risk assessment are not useful anymore.\n"
    obj <- addWarning(obj, warnMsg=warnMsg, method="pram", variable=kVar[1])
    warning(warnMsg)
    manipData <- manipKeyVars[, kVar, drop=FALSE]
  }
  if (length(pVar) > 0) {
    if (exists("manipData")) {
      manipData <- cbind(manipData, manipPramVars[, pVar, drop=FALSE])
    } else {
      manipData <- manipPramVars[, pVar, drop=FALSE]
    }
  }
  if (length(rVar) > 0) {
    if (exists("manipData")) {
      manipData <- cbind(manipData, obj@origData[, rVar, drop=FALSE])
    } else {
      manipData <- obj@origData[, rVar, drop=FALSE]
    }
  }
  if (!exists("manipData")) {
    manipData <- obj@origData[, pramVars, drop=FALSE]
  }

  if (!is.null(strata_variables)) {
    # case 1: character vector
    if (is.character(strata_variables)) {
      sData <- get.sdcMicroObj(obj, type="origData")[, strata_variables, drop=FALSE]
    }
    if (class(strata_variables) %in% c("integer","numeric","factor")) {
      sData <- data.table(strat=strata_variables)
      if (nrow(sData) != nrow(manipData)) {
        stop("Dimension of 'strata_variables' does not match with dimension of dataset!\n")
      }
    }
    manipData <- cbind(manipData, sData)
    strataVars <- c((length(pramVars)+1):length(manipData))
  } else if (length(strataVars) > 0) {
    sData <- get.sdcMicroObj(obj, type="origData")[, strataVars, drop=FALSE]
    manipData <- cbind(manipData, sData)
    strataVars <- c((length(pramVars)+1):length(manipData))
  }

  levList <- lapply(manipData[,pramVars,drop=FALSE], levels)
  params <- inputs_pram(pd=pd, alpha=alpha, levList=levList)
  res_pram <- pramWORK(data=manipData, variables=pramVars,
    strata_variables=strataVars, params=params)

  if ( is.null(pp)) {
    pram_inp <- list()
    pram_inp$params <- attr(res_pram, "pram_params")
    pram_inp$transitions <- attr(res_pram, "transitions")
    pram_inp$comparison <- attr(res_pram, "compdat")
    pram_inp$summary <- attr(res_pram,"summary")
  } else {
    pram_inp <- pp
    pram_inp$params <- c(pram_inp$params, attr(res_pram, "pram_params"))
    pram_inp$transitions <- c(pram_inp$transitions, attr(res_pram, "transitions"))
    pram_inp$comparison <- c(pram_inp$comparison, attr(res_pram, "compdat"))
    pram_inp$summary <- rbind(pram_inp$summary, attr(res_pram,"summary"))
  }
  obj <- set.sdcMicroObj(obj, type="pram", input=list(pram_inp))

  manipData[, pramVars] <- res_pram[, paste0(pramVars, "_pram"), drop=FALSE]

  if (length(pVar) > 0) {
    manipPramVars[, pVar] <- manipData[, pVar]
  }
  if (length(rVar) > 0) {
    if (is.null(manipPramVars)) {
      manipPramVars <- manipData[, rVar, drop=FALSE]
    } else {
      manipPramVars <- cbind(manipPramVars, manipData[, rVar, drop=FALSE])
    }
  }
  obj <- set.sdcMicroObj(obj, type="manipPramVars", input=list(manipPramVars))

  if (length(kVar) > 0) {
    manipKeyVars[, kVar] <- manipData[, kVar]
    obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(manipKeyVars))
  }
  pram <- get.sdcMicroObj(obj, type="pram")
  if (is.null(pram)) {
    pram <- list()
  }
  obj <- set.sdcMicroObj(obj, type="pram", input=list(pram))
  pramVarInd <- unique(c(obj@pramVars, standardizeInput(obj, pramVars)))
  obj <- set.sdcMicroObj(obj, type="pramVars", input=list(pramVarInd))
  obj <- calcRisks(obj)
  obj
})

setMethod(f="pramX", signature=c(obj="data.frame"),
definition=function(obj, variables=NULL, strata_variables=NULL, pd=0.8, alpha=0.5) {
  levList <- lapply(obj[,variables,drop=FALSE], levels)
  params <- inputs_pram(pd=pd, alpha=alpha, levList=levList)
  res <- pramWORK(data=obj, variables=variables, strata_variables=strata_variables, params=params)
  class(res) <- "pram"
  res
})

setMethod(f="pramX", signature=c(obj="factor"),
definition=function(obj, variables=NULL, strata_variables=NULL, pd=0.8, alpha=0.5) {
  if (!is.null(strata_variables)) {
    xx <- data.frame(x=obj, strata=strata_variables, stringsAsFactors=FALSE)
  } else {
    xx <- data.frame(x=obj, stringsAsFactors=FALSE)
  }
  levList <- list(levels(obj))
  params <- inputs_pram(pd=pd, alpha=alpha, levList=levList)
  res <- pramWORK(data=xx, variables="x", strata_variables=strata_variables, params=params)
  class(res) <- "pram"
  res
})

# handling of NA and NULL Values for weight-vector with strata x <-
# .Call('Pram',as.matrix(dat),-999,2,1,-1) only frequency x <-
# .Call('Pram',as.matrix(dat),-999,0,0,-1)

# levList: if Rs was specified as a list-element of 'pd' we need the
# levels to check if the matrix is valid!
inputs_pram <- function(pd, alpha, levList) {
  checkMat <- function(inpMat, levs) {
    if ( is.null(rownames(inpMat))) {
      stop("at least one matrix does not have row-names\n")
    }
    if (nrow(inpMat)!=ncol(inpMat)) {
      stop("at least one matrix is not a square matrix!\n")
    }
    if (!identical(rownames(inpMat),colnames(inpMat))) {
      stop("rownames and colnames of at least one input matrix do not match!\n")
    }
    if (!identical(rownames(inpMat),levs)) {
      stop("rownames do not match factor-levels for at least one transition matrix!\n")
    }
    TRUE
  }

  if (any(sapply(levList, is.null))) {
    stop("at least one variable is not coded as a factor.\n")
  }

  if ( is.matrix(pd) & length(levList)==1) {
    checkMat(pd, levList[[1]])
    return(list(pd=list(pd), alpha=NA))
  }

  nrPramVars <- length(levList)
  if (is.vector(alpha)) {
    if ( !is.numeric(alpha)) {
      stop("'alpha' needs to be a numeric vector!\n")
    }
    if (length(alpha)==1) {
      alpha <- as.list(rep(alpha, nrPramVars))
    } else {
      if ( length(alpha)!=nrPramVars) {
        stop("length of 'alpha' does not match with length of variables!\n")
      }
      alpha <- as.list(alpha)
    }
  } else {
    stop("'alpha' needs to be a vector!\n")
  }

  # at least one element should be a transition-matrix
  # we check if levels match
  if (is.list(pd)) {
    if (length(pd) != nrPramVars) {
      stop("length of 'pd' does not match with length of variables!\n")
    }
    for (i in seq_along(pd)) {
      if (!is.numeric(pd[[i]])) {
        stop("all elements of 'pd' must be either numeric vectors or matrices!\n")
      }
      if (is.matrix(pd[[i]])) {
        checkMat(inpMat=pd[[i]], levs=levList[[i]])
      }
    }
  } else if (is.vector(pd)) {
    if ( !is.numeric(pd)) {
      stop("'pd' needs to be a numeric vector!\n")
    }
    if ( length(pd)==1 ) {
      pd <- as.list(rep(pd, nrPramVars))
    } else {
      if ( length(pd)!=nrPramVars) {
        stop("length of 'pd' does not match with length of variables!\n")
      }
      pd <- as.list(pd)
    }
  }
  params <- list(pd=pd, alpha=alpha)
  params
}

pramWORK <- function(data, variables=NULL, strata_variables=NULL, params) {
  # calculate invariant transition matrix or check input-matrix
  calcTransitionMatrix <- function(xvec, pd, alpha=NULL) {
    levs <- levels(xvec)
    L <- length(levs)
    if (is.matrix(pd)) {
      # checks
      if (nrow(pd) != L | ncol(pd) != L) {
        stop("dimensions of transition-matrix do not match!\n")
      }
      if (any(abs(rowSums(pd)-1) > .Machine$double.eps)) {
        stop("rowSums of transition-matrix do not always equal 1!\n")
      }
      if (!identical(rownames(pd), levs)) {
        stop("rownames of 'pd' must equal levels of factor that should be pramed!")
      }
      if (!identical(colnames(pd), levs)) {
        stop("colnames of 'pd' must equal levels of factor that should be pramed!")
      }
      return(pd)
    }

    P <- matrix(, ncol=L, nrow=L)
    pds <- runif(L, min=pd, max=1)
    tri <- (1 - pds)/(L - 1)
    for (i in seq(L)) {
      P[i, ] <- tri[i]
    }
    diag(P) <- pds
    p <- table(xvec)/sum(as.numeric(na.omit(xvec)))
    Qest <- P
    for (k in seq(L)) {
      s <- sum(p * P[, k])
      for (j in seq(L)) {
        Qest[k, j] <- P[j, k] * p[j]/s
      }
    }
    R <- P %*% Qest
    EI <- diag(L)
    Rs <- alpha * R + (1 - alpha) * EI
    rownames(Rs) <- colnames(Rs) <- levs
    return(Rs)
  }

  # pram on a simple vector
  # x: input vector
  # Rs: transition matrix
  do.pram <- function(x, Rs) {
    # special case: na-only input
    if (all(is.na(x))) {
      return(list(x=x, xpramed=x))
    }
    xpramed <- x
    levs <- levels(xpramed)

    # perform sampling
    for ( i in 1:length(levs) ) {
      ii <- which(xpramed==levs[i])
      if (length(ii) > 0) {
        xpramed[ii] <- sample(levs, length(ii), prob=Rs[i, ], replace=TRUE)
      }
    }
    xpramed <- factor(xpramed, levels=levs)
    return(list(x=x, xpramed=xpramed))
  }

  idvarpram <- tmpfactor_for_pram <- NULL

  if (is.null(variables)) {
    stop("Please define valid variables to pram!\n")
  }

  data <- as.data.table(data)
  # all variables must be factors (new in sdcMicro >= 4.7.0)
  if (!all(sapply(data, class)[variables]=="factor")) {
    stop("all variables that should be pramed must be factors!\n")
  }

  # calculate stratification variable in any case;
  # even if it is only a 'pseudo' one
  data[,idvarpram:=1:.N]
  if (length(strata_variables) > 0) {
    f <- as.factor(apply(data[,strata_variables,with=F],1, paste0, collapse="_"))
  } else {
    f <- factor(rep(1, nrow(data)))
  }
  data[,tmpfactor_for_pram:=f]
  out <- list()
  out$pramdat <- NA
  out$params <- list()
  pd <- params$pd
  alpha <- params$alpha
  transitions <- list()
  comparedata <- list()
  for (i in 1:length(variables)) {
    v <- variables[i]
    pV <- data[[v]]
    levs <- levels(pV)

    s <- split(data[, c(variables, "idvarpram"), with=FALSE], f)
    cmd <- paste0("data[,",v,"_pram:=factor(NA, levels=levs)]")
    eval(parse(text=cmd))
    ll <- levels(data$tmpfactor_for_pram)

    # calculate or check and use transition-matrix
    Rs <- calcTransitionMatrix(xvec=data[[v]], pd=pd[[i]], alpha=alpha[[i]])
    for (si in ll) {
      ii <- which(data$tmpfactor_for_pram==si & !is.na(data[[v]]))
      res <- do.pram(x=data[ii][[v]], Rs=Rs)$xpramed
      cmd <- paste0("data[ii,",v,"_pram:=res]")
      eval(parse(text=cmd))
    }

    # transitions
    dat_o <- data[[v]]
    dat_p <- data[[paste0(v,"_pram")]]
    rr <- apply(rbind(dat_o, dat_p), 2, paste, collapse=" --> ")
    result <- data.table(table(rr))
    rownames(result) <- 1:nrow(result)
    colnames(result) <- c("transition", "Frequency")
    out$params[[length(out$params)+1]] <- list(Rs=Rs, pd=pd[[i]], alpha=alpha[[i]])
    transitions[[length(transitions)+1]] <- result

    # frequency-comparison
    to <- table(dat_o, useNA="always")
    tp <- table(dat_p, useNA="always")
    compdat <- matrix(NA, nrow=2, ncol=+1+length(to))
    compdat[1,-1] <- to
    compdat[2,-1] <- tp
    compdat[,1] <- c("Original Frequencies","Frequencies after Perturbation")
    compdat <- as.data.table(compdat)
    cn <- c(v, names(to))
    cn[length(cn)] <- "NA"
    setnames(compdat, cn)
    comparedata[[length(comparedata)+1]] <- compdat
  }
  data[,tmpfactor_for_pram:=NULL]
  data[,idvarpram:=NULL]
  data <- as.data.frame(data)

  x <- data
  x <- apply(x, 2, as.character)
  x[is.na(x)] <- "."  # NA comparisons -> cast to character (better solution?)
  pramVars <- colnames(x)[grep("_pram", colnames(x))]
  var <- unlist(lapply(pramVars, function(x) substring(x, 1, nchar(x, type="width") - 5)))
  df <- data.frame(variable=var, nrChanges=NA, percChanges=NA, stringsAsFactors=FALSE)
  for (i in 1:length(pramVars)) {
    s <- sum(as.character(x[, var[i]]) != as.character(x[, pramVars[i]]))
    p <- round(s/nrow(x) * 100, 2)
    df[i, "nrChanges"] <- s
    df[i, "percChanges"] <- p
  }
  x <- data
  attr(x, "summary") <- df
  names(out$params) <- names(transitions) <- names(comparedata) <- variables
  attr(x, "pram_params") <- out$params
  attr(x, "transitions") <- transitions
  attr(x, "compdat") <- comparedata
  invisible(x)
}

#' Print method for objects from class pram
#'
#' Print method for objects from class pram
#' @param x an object of class \code{\link{pram}}
#' @param \dots Additional arguments passed through.
#' @return absolute and relative frequencies of changed observations in each modified variable
#' @author Bernhard Meindl, Matthias Templ
#' @seealso \code{\link{pram}}
#' @keywords print
#' @method print pram
#' @author Matthias Templ and Bernhard Meindl
#' @export
print.pram <- function(x, ...) {
  params <- attr(x, "pram_params")
  df <- attr(x, "summary")
  cat("Number of changed observations: \n")
  cat("- - - - - - - - - - - \n")
  for (i in 1:nrow(df)) {
    cat(df$variable[i], " != ", paste0(df$variable[i],"_pram"), " : ", df$nrChanges[i], " (", df$percChanges[i], "%)", "\n", sep="")
  }
  return(invisible(df))
}

#' Summary method for objects from class pram
#'
#' Summary method for objects from class \sQuote{pram} to provide information
#' about transitions.
#'
#' Shows various information about the transitions.
#' @param object object from class \sQuote{pram}
#' @param \dots Additional arguments passed through.
#' @return The summary of object from class \sQuote{pram}.
#' @author Matthias Templ and Bernhard Meindl
#' @seealso \code{\link{pram}}
#' @references Templ, M.  \emph{Statistical Disclosure Control for Microdata
#' Using the R-Package sdcMicro}, Transactions on Data Privacy, vol. 1, number
#' 2, pp. 67-85, 2008.  \url{http://www.tdp.cat/issues/abs.a004a08.php}
#' @keywords print
#' @export
#' @examples
#'
#' data(free1)
#' x <- as.factor(free1[,"MARSTAT"])
#' x2 <- pram(x)
#' x2
#' summary(x2)
#'
summary.pram <- function(object, ...) {
  params <- attr(object, "pram_params")
  df <- attr(object, "summary")
  transitions <- attr(object, "transitions")
  compdats <- attr(object, "compdat")
  for (i in 1:nrow(df)) {
    cat("Variable: ", df$variable[i])
    cat("\n ----------------------")
    cat("\nFrequencies in original and perturbed data:\n")
    print(compdats[[i]])
    cat("\nTransitions:\n")
    print(transitions[[i]])
    cat("\n")
  }
}
