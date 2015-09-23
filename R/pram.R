#' Post Randomization
#'
#' To be used on categorical data. It randomly change the values of variables
#' on selected records (usually the risky ones) according to an invariant
#' probability transition matrix.
#'
#'
#' @name pram
#' @aliases pram-methods pram,data.frame-method pram,matrix-method
#' pram,vector-method pram,sdcMicroObj-method pram
#' @docType methods
#' @param obj Input data. Allowed input data are objects of class 'matrix',
#' 'data.frame', 'vector' or \code{\link{sdcMicroObj-class}}.
#' @param variables Names of variables in 'obj' on which post-randomization
#' should be applied. If obj is a vector, this argument is ignored.
#' @param strata_variables Names of variables for stratification (will be set
#' automatically for an object of class \code{\link{sdcMicroObj-class}}. One can also specify
#' an integer vector or factor that specifies that desired groups. This vector must match the dimension
#' of the input data set, however. For a possible use case, have a look at the examples.
#' @param ...  further input, currently ignored.
#' @param pd minimum diagonal entries for the generated transition matrix P.
#' Either a vector of length 1 or a vector of length ( number of categories ).
#' @param alpha amount of perturbation for the invariant Pram method
#' @return a modified \code{\link{sdcMicroObj-class}} object or a new object containing
#' original and post-randomized variables (with suffix "_pram").
#' @section Methods: \describe{
#' \item{list("signature(obj = \"sdcMicroObj\")")}{...}
#' \item{list("signature(obj = \"data.frame\")")}{...}
#' \item{list("signature(obj = \"matrix\")")}{...}
#' \item{list("signature(obj = \"vector\")")}{...}}
#' @author Alexander Kowarik, Matthias Templ, Bernhard Meindl
#' @references \url{http://www.gnu.org/software/glpk}
#'
#' \url{http://www.ccsr.ac.uk/sars/guide/2001/pram.pdf}
#' @keywords manip
#' @export
#' @note Deprecated method 'pram_strata'is no longer available
#' in sdcMicro > 4.5.0
#' @examples
#'
#' data(testdata)
#' res <- pram(testdata,
#'   variables="roof",
#'   strata_variables=c("urbrur","sex"))
#' print(res)
#'
#' res1 <- pram(testdata,variables=c("roof","walls","water"),strata_variables=c("urbrur","sex"))
#' print(res1)
#'
#' res2 <- pram(testdata,variables=c("roof","walls","water"),
#'   strata_variables=NULL)
#' print(res2)
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- pram(sdc, variables=c("urbrur"))
#'
#' # this is equal to the previous application:
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight',
#'   pramVars="urbrur")
#' sdc <- pram(sdc)
#'
#' ## using a custom strata variable
#' # we want to apply pram to variable 'urbrur' for each group of variable 'urbrur'
#' # however: values no value should be changed where roof==4
#' # thus, we are creating a new value for these observations
#' data(testdata)
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sv <- testdata$urbrur
#' # new category for those that observations that should not change:
#' sv[testdata$roof==4] <- max(sv)+1
#' sdc <- pram(sdc, variables=c("roof"), strata_variables=sv)
#' orig <- get.sdcMicroObj(sdc, "origData")$roof
#' pramed <- get.sdcMicroObj(sdc, "manipPramVars")$roof
#' all(pramed[orig==4]==4) # nothing has changed!
setGeneric("pram", function(obj, variables = NULL, strata_variables = NULL,
  pd = 0.8, alpha = 0.5) {
  standardGeneric("pram")
})

setMethod(f = "pram", signature = c("sdcMicroObj"),
definition = function(obj, variables = NULL, strata_variables = NULL, pd = 0.8, alpha = 0.5) {
  pramVars <- get.sdcMicroObj(obj, type = "pramVars")
  if (length(pramVars) == 0 && is.null(variables)) {
    stop("Error: slot pramVars is NULL and argument 'variables' was not specified!\nDefine one of them to use pram on these variables\n")
  }
  if (is.null(variables)) {
    pramVars <- colnames(obj@origData)[get.sdcMicroObj(obj, type = "pramVars")]
  } else {
    pramVars <- variables
  }
  ### Get data from manipPramVars
  manipPramVars <- get.sdcMicroObj(obj, type = "manipPramVars")
  strataVars <- get.sdcMicroObj(obj, type = "strataVar")
  manipKeyVars <- get.sdcMicroObj(obj, type = "manipKeyVars")
  kVar <- pramVars[pramVars %in% colnames(manipKeyVars)]
  pVar <- pramVars[pramVars %in% colnames(manipPramVars)]
  rVar <- pramVars[!pramVars %in% c(kVar, pVar)]

  if (length(kVar) > 0) {
    warning("If pram is applied on key variables, the k-anonymity and risk assessment are not useful anymore.\n")
    manipData <- manipKeyVars[, kVar, drop = FALSE]
  }
  if (length(pVar) > 0) {
    if (exists("manipData")) {
      manipData <- cbind(manipData, manipPramVars[, pVar, drop = FALSE])
    } else {
      manipData <- manipPramVars[, pVar, drop = FALSE]
    }
  }
  if (length(rVar) > 0) {
    if (exists("manipData")) {
      manipData <- cbind(manipData, obj@origData[, rVar, drop = FALSE])
    } else {
      manipData <- obj@origData[, rVar, drop = FALSE]
    }
  }
  if (!exists("manipData")) {
    manipData <- obj@origData[, pramVars, drop = FALSE]
  }

  if ( !is.null(strata_variables) ) {
    # case 1: character vector
    if ( is.character(strata_variables) ) {
      sData <- get.sdcMicroObj(obj, type = "origData")[, strata_variables, drop = FALSE]
    }
    if ( class(strata_variables) %in% c("integer","numeric","factor") ) {
      sData <- data.table(strat=strata_variables)
      if ( nrow(sData) != nrow(manipData) ) {
        stop("Dimension of 'strata_variables' does not match with dimension of dataset!\n")
      }
    }
    manipData <- cbind(manipData, sData)
    strataVars <- c((length(pramVars)+1):length(manipData))
  } else if (length(strataVars) > 0) {
    sData <- get.sdcMicroObj(obj, type = "origData")[, strataVars, drop = FALSE]
    manipData <- cbind(manipData, sData)
    strataVars <- c((length(pramVars)+1):length(manipData))
  }

  res <- pramWORK(data = manipData, variables = pramVars, strata_variables = strataVars,
    pd = pd, alpha = alpha)
  tmp <- data.frame(unclass(res))
  manipData[, pramVars] <- tmp[, paste(pramVars, "_pram", sep = ""), drop = FALSE]
  obj <- nextSdcObj(obj)

  if (length(pVar) > 0) {
    manipPramVars[, pVar] <- manipData[, pVar]
  }
  if (length(rVar) > 0) {
    if (is.null(manipPramVars))
      manipPramVars <- manipData[, rVar, drop = FALSE] else manipPramVars <- cbind(manipPramVars, manipData[, rVar, drop = FALSE])
  }
  obj <- set.sdcMicroObj(obj, type = "manipPramVars", input = list(manipPramVars))

  if (length(kVar) > 0) {
    manipKeyVars[, kVar] <- manipData[, kVar]
    obj <- set.sdcMicroObj(obj, type = "manipKeyVars", input = list(manipKeyVars))
  }
  pram <- get.sdcMicroObj(obj, type = "pram")
  if (is.null(pram)) {
    pram <- list()
  }
  pram$pd <- pd
  pram$alpha <- alpha

  pram$summary <- print.pram(res)
  obj <- set.sdcMicroObj(obj, type = "pram", list(pram))
  pramVarInd <- standardizeInput(obj, pramVars)
  obj <- set.sdcMicroObj(obj, type = "pramVars", input = list(pramVarInd))
  obj <- calcRisks(obj)
  obj
})

setMethod(f = "pram", signature = c("data.frame"),
definition = function(obj, variables = NULL, strata_variables = NULL, pd = 0.8, alpha = 0.5) {
  pramWORK(data = obj, variables = variables, strata_variables = strata_variables, pd = pd, alpha = alpha)
})

setMethod(f = "pram", signature = c("matrix"),
definition = function(obj, variables = NULL, strata_variables = NULL, pd = 0.8, alpha = 0.5) {
  pramWORK(data = obj, variables = variables, strata_variables = strata_variables, pd = pd, alpha = alpha)
})

setMethod(f = "pram", signature = c("vector"),
definition = function(obj, variables = NULL, strata_variables = NULL, pd = 0.8, alpha = 0.5) {
  if ( !is.null(strata_variables) ) {
    xx <- data.frame(x = obj, strata=strata_variables, stringsAsFactors = FALSE)
  } else {
    xx <- data.frame(x = obj, stringsAsFactors = FALSE)
  }
  pramWORK(data = xx, variables = "x", strata_variables = strata_variables, pd = pd, alpha = alpha)
})

# handling of NA and NULL Values for weight-vector with strata x <-
# .Call('Pram',as.matrix(dat),-999,2,1,-1) only frequency x <-
# .Call('Pram',as.matrix(dat),-999,0,0,-1)
pramWORK <- function(data, variables = NULL, strata_variables = NULL, pd = 0.8, alpha = 0.5) {
  # pram on a simple vector
  do.pram <- function(x, pd, alpha) {
    # special case: na-only input
    if (all(is.na(x))) {
      return(list(x = x, xpramed = x))
    }

    if (class(x) != "factor") {
      warning("pram makes only sense for categorical variables stored as factors")
    }
    fac <- FALSE
    recoding <- FALSE
    xpramed <- x

    if (class(x) == "factor") {
      fac <- TRUE
    } else {
      fac <- FALSE
      xpramed <- as.factor(xpramed)
    }
    lev <- levels(xpramed)
    xpramed <- as.integer(as.factor(xpramed))

    # Recoding necessary if factors != 1:...
    recodeNAS <- FALSE
    nas <- which(is.na(xpramed))
    if (length(nas) > 0) {
      NAKat <- max(xpramed, na.rm = TRUE) + 1
      xpramed[nas] <- NAKat
      recodeNAS <- TRUE
    }

    if (min(xpramed, na.rm = TRUE) != 1 | max(xpramed, na.rm = TRUE) != length(unique(xpramed))) {
      recoding <- TRUE
      tmp <- xpramed
      xpramed <- rep(NA, length(tmp))
      un <- sort(unique(tmp))
      xpramedBack <- list()
      xpramedBack[[1]] <- un
      xpramedBack[[2]] <- 1:length(un)
      for (i in 1:length(un)) {
        xpramed[which(tmp == un[i])] <- i
      }
    }

    L <- length(table(xpramed))
    P <- matrix(, ncol = L, nrow = L)
    pds <- runif(L, min = pd, max = 1)
    tri <- (1 - pds)/(L - 1)
    for (i in seq(L)) {
      P[i, ] <- tri[i]
    }
    diag(P) <- pds
    p <- table(xpramed)/sum(as.numeric(xpramed))
    Qest <- P
    for (k in seq(L)) {
      s <- sum(p * P[, k])
      for (j in seq(L)) {
        Qest[k, j] <- P[j, k] * p[j]/s
      }
    }
    R <- P %*% Qest
    EI <- matrix(0, ncol = L, nrow = L)
    diag(EI) <- 1
    Rs <- alpha * R + (1 - alpha) * EI

    for (i in 1:length(xpramed)) {
      xpramed[i] <- sample(1:L, 1, prob = Rs[xpramed[i], ])
    }

    # Recoding necessary??
    if (recoding == TRUE) {
      xpramedFinal <- rep(NA, length(tmp))
      for (i in 1:length(xpramedBack[[1]])) {
        xpramedFinal[which(xpramed == i)] <- xpramedBack[[1]][i]
      }
      xpramed <- xpramedFinal
    }

    if (recodeNAS == TRUE) {
      nas <- which(xpramed == NAKat)
      if (length(nas) > 0) {
        xpramed[nas] <- NA
      }
    }
    if (fac == TRUE) {
      if (length(unique(xpramed)) == length(lev)) {
        xpramed <- factor(xpramed, labels = lev)
      } else {
        xpramed <- factor(xpramed, labels = lev[sort(unique(xpramed))])
      }
    }
    if (fac == FALSE & class(x) == "character") {
      xpramed <- as.character(factor(xpramed, labels = lev))
    }
    return(list(x = x, xpramed = xpramed))
  }

  idvarpram <- tmpfactor_for_pram <- NULL
  if (is.null(variables)) {
    stop("Please define valid variables to pram!\n")
  }

  data <- as.data.table(data)
  if (length(strata_variables) > 0) {
    data[,idvarpram:=1:.N]
    fac <- chara <- list()

    f <- as.factor(apply(data[,strata_variables,with=F],1,paste0, collapse="_"))
    data[,tmpfactor_for_pram:=f]
    s <- split(data[, c(variables, "idvarpram"), with = FALSE], f)
    for (i in 1:length(variables)) {
      v <- variables[i]
      cmd <- paste0("data[,",v,"_pram:='']")
      eval(parse(text=cmd))

      fac[[i]] <- FALSE
      if (!is.factor(data[[v]]) & is.character(data[[v]])) {
        fac[[i]] <- TRUE
        chara[[i]] <- TRUE
      } else if (!is.factor(data[[v]]) & is.numeric(data[[v]])) {
        chara[[i]] <- FALSE
      } else {
        fac[[i]] <- TRUE
      }
      ll <- levels(f)
      for ( si in ll ) {
        res <- do.pram(x = as.factor(data[tmpfactor_for_pram==si][[v]]), pd = pd, alpha = alpha)$xpramed
        cmd <- paste0("data[tmpfactor_for_pram==",shQuote(si),",",v,"_pram:=as.character(res)]")
        eval(parse(text=cmd))
      }
      # correct output class
      if ( !fac[[i]] ) {
        if ( chara[[i]] ) {
          cmd <- paste0("data[,",v,"_pram:=as.character(",v,"_pram)]")
        } else {
          cmd <- paste0("data[,",v,"_pram:=as.numeric(",v,"_pram)]")
        }
      } else {
        cmd <- paste0("data[,",v,"_pram:=as.factor(",v,"_pram)]")
      }
      eval(parse(text=cmd))
    }
    setkey(data, idvarpram)
    data[,c("idvarpram","tmpfactor_for_pram"):=NULL]
  } else {
    for (v in variables) {
      pV <- data[[v]]
      if ( is.factor(pV) ) {
        res <- do.pram(x=pV, pd=pd, alpha=alpha)$xpramed
        cmd <- paste0("data[,",v,"_pram:=res]")
      } else if ( is.numeric(pV) ) {
        res <- do.pram(x=as.factor(as.character(pV)), pd=pd, alpha=alpha)$xpramed
        cmd <- paste0("data[,",v,"_pram:=as.numeric(as.character(res))]")
      } else if ( is.character(pV) ) {
        res <- do.pram(x=as.factor(pV), pd=pd, alpha=alpha)$xpramed
        cmd <- paste0("data[,",v,"_pram:=as.factor(res)]")
      }
      eval(parse(text=cmd))
    }
  }
  res <- as.data.frame(data)
  class(res) <- "pram"
  invisible(res)
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
#' @export
print.pram <- function(x, ...) {
  x <- as.data.frame(unclass(x))
  x <- apply(x, 2, as.character)
  x[is.na(x)] <- "."  # NA comparisons -> cast to character (better solution?)
  pram_var <- colnames(x)[grep("pram", colnames(x))]
  var <- unlist(lapply(pram_var, function(x) substring(x, 1, nchar(x, type = "width") - 5)))

  df <- data.frame(variable = var, nrChanges = NA, percChanges = NA, stringsAsFactors = FALSE)
  cat("Number of changed observations: \n")
  cat("- - - - - - - - - - - \n")
  for (i in 1:length(pram_var)) {
    # FIXME: factor levels of x[,var[i]] and x[,pram_var[i]] not always the same! s <-
    # sum(x[,var[i]]!=x[,pram_var[i]])
    s <- sum(as.character(x[, var[i]]) != as.character(x[, pram_var[i]]))
    p <- round(s/nrow(x) * 100, 2)
    cat(var[i], " != ", pram_var[i], " : ", s, " (", p, "%)", "\n", sep = "")
    df[i, "nrChanges"] <- s
    df[i, "percChanges"] <- p
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
#' @author Matthias Templ
#' @seealso \code{\link{pram}}
#' @references Templ, M.  \emph{Statistical Disclosure Control for Microdata
#' Using the R-Package sdcMicro}, Transactions on Data Privacy, vol. 1, number
#' 2, pp. 67-85, 2008.  \url{http://www.tdp.cat/issues/abs.a004a08.php}
#' @keywords print
#' @export
#' @examples
#'
#' data(free1)
#' x <- free1[,"MARSTAT"]
#' x2 <- pram(x)
#' x2
#' summary(x2)
#'
summary.pram <- function(object, ...) {
  rr <- apply(rbind(object$x, object$xpramed), 2, paste, collapse = " --> ")
  result <- data.frame(table(rr))
  rownames(result) <- 1:nrow(result)
  colnames(result) <- c("transition", "Frequency")
  cat("\n ----------------------")
  cat("\n original frequencies:\n")
  print(table(object$x))
  cat("\n ----------------------")
  cat("\n frequencies after perturbation:\n")
  print(table(object$x_pram))  #
  cat("\n ----------------------")
  cat("\n transitions:\n")
  result
}
