#' Local Suppression
#'
#' A simple method to perfom local suppression.
#'
#' Values of high risk (above the threshold) of a certain variable (parameter
#' keyVar) are suppressed.
#'
#' @name localSupp
#' @aliases localSupp-methods localSupp,ANY-method localSupp,sdcMicroObj-method localSupp
#' @docType methods
#' @param obj object of class freqCalc or sdcMicroObj
#' @param threshold threshold for individual risk
#' @param keyVar Variable on which some values might be suppressed
#' @param ... see arguments below
#' \itemize{
#' \item{indivRisk}{object from class indivRisk}}
#' @return Manipulated data with suppressions or the \code{\link{sdcMicroObj-class}}
#' object with manipulated data.
#' @section Methods: \describe{
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}
#' \item{list("signature(obj = \"ANY\")")}{}}
#' @author Matthias Templ
#' @seealso \code{\link{freqCalc}}, \code{\link{indivRisk}}
#' @references Templ, M. \emph{Statistical Disclosure Control for Microdata
#' Using the R-Package sdcMicro}, Transactions on Data Privacy, vol. 1, number
#' 2, pp. 67-85, 2008. \url{http://www.tdp.cat/issues/abs.a004a08.php}
#' @keywords manip
#' @export
#' @examples
#'
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
#' f
#' f$fk
#' f$Fk
#' ## individual risk calculation:
#' indivf <- indivRisk(f)
#' indivf$rk
#' ## Local Suppression
#' localS <- localSupp(f, keyVar=2, indivRisk=indivf$rk, threshold=0.25)
#' f2 <- freqCalc(localS$freqCalc, keyVars=c(4,5,6), w=8)
#' indivf2 <- indivRisk(f2)
#' indivf2$rk
#' ## select another keyVar and run localSupp once again,
#' # if you think the table is not fully protected
#'
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- localSupp(sdc, keyVar='urbrur')
#'
setGeneric("localSupp", function(obj, threshold = 0.15, keyVar, ...) {
  standardGeneric("localSupp")
})

setMethod(f = "localSupp", signature = c("sdcMicroObj"),
definition = function(obj, threshold = 0.15, keyVar, ...) {
  manipData <- get.sdcMicroObj(obj, type = "manipKeyVars")
  keyVars <- colnames(manipData)
  rk <- get.sdcMicroObj(obj, type = "risk")$individual[, 1]
  TF <- rk > threshold
  if (!all(keyVar %in% colnames(manipData))) {
    stop("keyVar must be a defined categorical keyVariable!\n")
  }

  if (any(TF)) {
    manipData[which(TF), keyVar] <- NA
    obj <- nextSdcObj(obj)
    obj <- set.sdcMicroObj(obj, type = "manipKeyVars", input = list(manipData))
    a <- get.sdcMicroObj(obj, type = "origData")[, get.sdcMicroObj(obj, type = "keyVars")]
    b <- manipData
    w = which(is.na(a))
    r = w%%nrow(a)
    cc = ceiling(w/nrow(a))
    wb = which(is.na(b))
    rb = wb%%nrow(b)
    cb = ceiling(wb/nrow(b))
    d = data.frame(id = 1:ncol(a), before = NA, after = NA)
    d[, 2:3] <- t(sapply(1:ncol(a), function(x) c(sum(cc == x), sum(cb == x))))
    obj <- set.sdcMicroObj(obj, type = "localSuppression", input = list(list(d$after - d$before)))
    obj <- calcRisks(obj)
  }
  obj
})

setMethod(f = "localSupp", signature = c("ANY"),
definition = function(obj, threshold = 0.15, keyVar, indivRisk) {
  ## x ... object from class freqCalc keyVar ... variables used for local suppression, ordered
  ## indivRisk ... vector of individual risks fixme: better method for local suppression
  ## calculate risk a second (and third time) and choose another keyVar! no keyVars =
  ## x$keyVars + 1 ## indexG is now first
  if (class(obj) != "freqCalc") {
    stop("obj is not from class freqCalc")
  }
  obj$freqCalc[indivRisk > threshold, keyVar[1]] <- NA
  obj
})
