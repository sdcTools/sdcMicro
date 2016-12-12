#' Local Suppression
#'
#' A simple method to perfom local suppression.
#'
#' Values of high risk (above the threshold) of a certain variable (parameter
#' keyVar) are suppressed.
#'
#' @name localSupp
#' @docType methods
#' @param obj object of class \code{\link{freqCalc}} or \code{\link{sdcMicroObj-class}}.
#' @param threshold threshold for individual risk
#' @param keyVar Variable on which some values might be suppressed
#' @return an updated object of class \code{\link{freqCalc}} or the \code{\link{sdcMicroObj-class}}
#' object with manipulated data.
#' @author Matthias Templ and Bernhard Meindl
#' @seealso \code{\link{freqCalc}}, \code{\link{indivRisk}}
#' @references Templ, M. \emph{Statistical Disclosure Control for Microdata
#' Using the R-Package sdcMicro}, Transactions on Data Privacy, vol. 1, number
#' 2, pp. 67-85, 2008. \url{http://www.tdp.cat/issues/abs.a004a08.php}
#' @keywords manip
#' @export
#' @examples
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' keyVars <- paste0("Key",1:4)
#' f <- freqCalc(francdat, keyVars=keyVars,w=8)
#' f
#' f$fk
#' f$Fk
#' ## individual risk calculation:
#' indivf <- indivRisk(f)
#' indivf$rk
#' ## Local Suppression
#' localS <- localSupp(f, keyVar="Key4", threshold=0.15)
#' f2 <- freqCalc(localS$freqCalc, keyVars=keyVars, w=8)
#' indivf2 <- indivRisk(f2)
#' indivf2$rk
#' identical(indivf$rk, indivf2$rk)
#'
#' ## select another keyVar and run localSupp once again,
#' # if you think the table is not fully protected
#'
#' ## for objects of class sdcMicro:
#' data(testdata)
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- localSupp(sdc, keyVar='urbrur', threshold=0.045)
#' print(sdc, type="ls")
#'
localSupp <- function(obj, threshold=0.15, keyVar) {
  localSuppX(obj=obj, threshold=threshold, keyVar=keyVar)
}

setGeneric("localSuppX", function(obj, threshold = 0.15, keyVar) {
  standardGeneric("localSuppX")
})

setMethod(f="localSuppX", signature=c(obj="sdcMicroObj"),
definition = function(obj, threshold=0.15, keyVar) {
  obj <- nextSdcObj(obj)

  manipData <- get.sdcMicroObj(obj, type="manipKeyVars")
  rk <- get.sdcMicroObj(obj, type="risk")$individual[, 1]

  if (!is.character(keyVar)) {
    stop("key variables need to be specified by their name!\n")
  }
  if ( length(keyVar)!=1) {
    stop("more than 1 key variable specified!\n")
  }

  cn <- colnames(get.sdcMicroObj(obj, type="origData"))[get.sdcMicroObj(obj, type="keyVars")]
  if (!keyVar %in% cn) {
    stop("invalid key variable specified!\n")
  }

  ls <- localSuppWORK(x=manipData, rk=rk, keyVar=keyVar, threshold=threshold)

  # create final output
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(ls$xAnon))
  ls$xAnon <- NULL
  class(ls) <- unclass("list")
  obj <- set.sdcMicroObj(obj, type="localSuppression", input=list(ls))

  # transfer suppression patterns if ghostVars is specified
  ghostVars <- get.sdcMicroObj(obj, type="ghostVars")
  if (!is.null(ghostVars)) {
    manipData <- get.sdcMicroObj(obj, type="manipKeyVars")
    manipGhostVars <- get.sdcMicroObj(obj, type="manipGhostVars")
    cn <- colnames(get.sdcMicroObj(obj, type="origData"))
    for (i in seq_along(ghostVars)) {
      # index of keyVar within manipData
      kV <- match(cn[ghostVars[[i]][[1]]], colnames(manipData))
      isna <- is.na(manipData[[kV]])

      # get indices of linked variables within ghostVars and
      # transfer suppression pattern
      vv <- match(cn[ghostVars[[i]][[2]]], colnames(manipGhostVars))
      for (j in 1:length(vv)) {
        manipGhostVars[[vv[j]]][isna] <- NA
      }
    }
    obj <- set.sdcMicroObj(obj, type="manipGhostVars", input=list(manipGhostVars))
  }
  obj <- calcRisks(obj)
  obj
})

setMethod(f="localSuppX", signature=c("ANY"),
definition = function(obj, threshold=0.15, keyVar) {
  if (!class(obj)=="freqCalc") {
    stop("'obj' must be of class 'freqCalc'\n")
  }
  rk <- indivRisk(obj)$rk
  x <- obj$freqCalc[,obj$keyVars]
  if ( length(keyVar)!=1) {
    stop("more than 1 key variable specified!\n")
  }
  if (is.numeric(keyVar)) {
    keyVar <- colnames(obj$freqCalc)[keyVar]
  }
  if (is.na(keyVar)) {
    stop("invalid key variable specified!\n")
  }
  res <- localSuppWORK(x=x, rk=rk, threshold=threshold, keyVar=keyVar)
  cat(res$newSupps,"observations has individual risks >=",threshold,"and were suppressed!\n")
  inpdf <- obj$freqCalc
  inpdf[,obj$keyVars] <- res$xAnon
  freqCalc(inpdf, keyVars=obj$keyVars, w=obj$w)
})

localSuppWORK <- function(x, rk, keyVar, threshold) {
  x <- as.data.table(x)

  na_before <- x[,lapply(.SD, function(x) { sum(is.na(x))})]

  TF <- rk > threshold
  if (any(TF)) {
    x[[keyVar]][which(TF)] <- NA
  }
  supps_total <- x[,lapply(.SD, function(x) { sum(is.na(x))})]

  importance <- rep(NA, ncol(x))
  importance[match(keyVar, colnames(x))] <- ncol(x)
  importance[is.na(importance)] <- sample(1:(ncol(x)-1))

  supps <- supps_total-na_before
  res <- list(xAnon=as.data.frame(x), supps=supps,
    totalSupps=supps_total, newSupps=sum(supps_total)-sum(na_before), anonymity=NA, keyVars=colnames(x),
    strataVars=NULL, importance=importance, k=NA, threshold=threshold, combs=NULL)
  class(res) <- "localSuppression"
  res
}
