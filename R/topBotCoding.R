#' Top and Bottom Coding
#'
#' Function for Top and Bottom Coding.
#'
#' Extreme values are replaced by one value to reduce the disclosure risk.
#'
#' @name topBotCoding
#' @aliases topBotCoding-methods topBotCoding,ANY-method
#' topBotCoding,sdcMicroObj-method topBotCoding
#' @docType methods
#' @param obj vector or one-dimensional matrix or data.frame or object of class
#' \code{\link{sdcMicroObj-class}}
#' @param value limit, from where it should be top- or bottom-coded
#' @param replacement replacement value.
#' @param kind top or bottom
#' @param column xxx
#' @return Top or bottom coded data or modified \code{\link{sdcMicroObj-class}}.
#' @section Methods: \describe{
#' \item{list("signature(obj = \"ANY\")")}{}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @author Matthias Templ
#' @seealso \code{\link{indivRisk}}
#' @keywords manip
#' @export
#' @examples
#'
#' data(free1)
#' topBotCoding(free1[,"DEBTS"], value=9000, replacement=9100, kind="top")
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2, keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'            numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- topBotCoding(sdc, value=500000, replacement=1000, column="income")
#' testdataout <- extractManipData(sdc)
#'
setGeneric('topBotCoding', function(obj, value, replacement, kind="top", column=NULL) {standardGeneric('topBotCoding')})
setMethod(f='topBotCoding', signature=c('sdcMicroObj'),
definition=function(obj, value, replacement, kind="top", column=NULL) {
  manipNumVars <- get.sdcMicroObj(obj, type="manipNumVars")
  manipKeyVars <- get.sdcMicroObj(obj, type="manipKeyVars")
  o <- get.sdcMicroObj(obj, type="origData")
  obj <- nextSdcObj(obj)
  if(column%in%colnames(manipNumVars)){
    x <- manipNumVars[,column]
    manipNumVars[,column] <- topBotCodingWORK(x, value=value,replacement=replacement,kind=kind,column=column)
    obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(manipNumVars))
    obj <- dRisk(obj)
    obj <- dUtility(obj)
  }else if(column%in%colnames(manipKeyVars)){
    #warning("topBotCoding on a categorical key variable, but variable should be a numeric key variable")
    x <- manipKeyVars[,column]
    if(!is.numeric(x))
      stop("Only numeric variables can be top or bottom coded.")
    manipKeyVars[,column] <- topBotCodingWORK(x, value=value,replacement=replacement,kind=kind,column=column)
    obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(manipKeyVars))
    obj <- measure_risk(obj)
  } else if(column%in%colnames(manipPramVars)){
    x <- manipPramVars[,column]
    if(!is.numeric(x))
      stop("Only numeric variables can be top or bottom coded.")
    manipPramVars[,column] <- topBotCodingWORK(x, value=value,replacement=replacement,kind=kind,column=column)
    obj <- set.sdcMicroObj(obj, type="manipPramVars", input=list(manipPramVars))
    obj <- measure_risk(obj)
  } else if(column%in%colnames(o)){
    stop("topBotCoding on variable which is neither a categorical nor a numeric key variable.")
  }else
    stop("variable could not be found in the data set")
  invisible(obj)
})

setMethod(f='topBotCoding', signature=c("ANY"),
definition=function(obj, value, replacement, kind="top", column=NULL) {
  topBotCodingWORK(x=obj,value, replacement, kind=kind, column=column)
})

topBotCodingWORK <- function(x, value, replacement, kind="top", column=NULL){ #COLUMN FOR COMPATIBILITY WITH V4
  if( class(x) == "data.frame" ){
    if( kind == "top"){
      x[x > value, ] <- replacement
    } else{
      x[x < value, ] <- replacement
    }
  }
  if( class(x) %in% c("numeric","integer") ){
    if( kind == "top"){
      x[x > value ] <- replacement
    } else{
      x[x < value ] <- replacement
    }
  }
  if( class(x) == "factor"){
    x <- as.numeric(as.character(x))
    if( kind == "top"){
      x[x > value ] <- replacement
    } else{
      x[x < value ] <- replacement
    }
    x <- as.factor(x)
  }
  invisible(x)
}
