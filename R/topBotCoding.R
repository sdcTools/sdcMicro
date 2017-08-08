#' Top and Bottom Coding
#'
#' Function for Top and Bottom Coding.
#'
#' Extreme values larger or lower than \code{value} are replaced by a different value (\code{replacement} in order to reduce the disclosure risk.
#'
#' @name topBotCoding
#' @docType methods
#' @param obj a numeric vector, a \code{data.frame} or a \code{\link{sdcMicroObj-class}}-object
#' @param value limit, from where it should be top- or bottom-coded
#' @param replacement replacement value.
#' @param kind top or bottom
#' @param column variable name in case the input is a \code{data.frame} or an object of class \code{\link{sdcMicroObj-class}}.
#' @return Top or bottom coded data or modified \code{\link{sdcMicroObj-class}}.
#' @author Matthias Templ and Bernhard Meindl
#' @references 
#' Templ, M. and Kowarik, A. and Meindl, B. 
#' Statistical Disclosure Control for Micro-Data Using the R Package sdcMicro. 
#' \emph{Journal of Statistical Software}, \strong{67} (4), 1--36, 2015. \doi{10.18637/jss.v067.i04}
#' @seealso \code{\link{indivRisk}}
#' @keywords manip
#' @note top-/bottom coding of factors is no longer possible as of sdcMicro >=4.7.0
#' @export
#' @examples
#' data(free1)
#' res <- topBotCoding(free1[,"DEBTS"], value=9000, replacement=9100, kind="top")
#' max(res)
#'
#' data(testdata)
#' range(testdata$age)
#' testdata <- topBotCoding(testdata, value=80, replacement=81, kind="top", column="age")
#' range(testdata$age)
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2, keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'            numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- topBotCoding(sdc, value=500000, replacement=1000, column="income")
#' testdataout <- extractManipData(sdc)
topBotCoding <- function(obj, value, replacement, kind="top", column=NULL) {
  topBotCodingX(obj=obj, value=value, replacement=replacement, kind=kind, column=column)
}

setGeneric('topBotCodingX', function(obj, value, replacement, kind="top", column=NULL) {
  standardGeneric('topBotCodingX')
})

setMethod(f='topBotCodingX', signature=c('sdcMicroObj'),
definition=function(obj, value, replacement, kind="top", column=NULL) {
  obj <- nextSdcObj(obj)
  manipNumVars <- get.sdcMicroObj(obj, type="manipNumVars")
  manipKeyVars <- get.sdcMicroObj(obj, type="manipKeyVars")
  manipPramVars <- get.sdcMicroObj(obj, type="manipPramVars")
  o <- get.sdcMicroObj(obj, type="origData")
  if (length(column)!=1) {
    stop("length of argument 'column' > 1\n")
  }
  if (!column %in% colnames(o)) {
    stop("variable specified in 'column' can not be found!\n")
  }
  if(column%in%colnames(manipNumVars)){
    x <- manipNumVars[,column]
    manipNumVars[,column] <- topBotCoding(x, value=value,replacement=replacement,kind=kind)
    obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(manipNumVars))
    obj <- dRisk(obj)
    obj <- dUtility(obj)
  }else if(column%in%colnames(manipKeyVars)){
    x <- manipKeyVars[,column]
    manipKeyVars[,column] <- topBotCoding(x, value=value,replacement=replacement,kind=kind)
    obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(manipKeyVars))
    obj <- measure_risk(obj)
  } else if(column%in%colnames(manipPramVars)){
    x <- manipPramVars[,column]
    manipPramVars[,column] <- topBotCoding(x, value=value,replacement=replacement,kind=kind)
    obj <- set.sdcMicroObj(obj, type="manipPramVars", input=list(manipPramVars))
    obj <- measure_risk(obj)
  } else if(column%in%colnames(o)){
    #message("topBotCoding on variable which is neither a categorical nor a numeric key variable.")
    x <- o[[column]]
    o[[column]] <- topBotCoding(x, value=value,replacement=replacement,kind=kind)
    obj <- set.sdcMicroObj(obj, type="origData", input=list(o))
  }
  invisible(obj)
})

setMethod(f='topBotCodingX', signature=c("data.frame"),
definition=function(obj, value, replacement, kind="top", column=column) {
  if (length(column)!=1) {
    stop("length of argument 'column' > 1\n")
  }
  v <- obj[[column]]
  if ( !is.numeric(v)) {
    stop("specified column is not numeric. topBotCoding() can only be applied to numeric variables!\n")
  }
  v <- topBotCoding(obj=v,value=value, replacement=replacement, kind=kind) # 'numeric-method'
  obj[[column]] <- v
  obj
})

setMethod(f='topBotCodingX', signature=c("numeric"),
definition=function(obj, value, replacement, kind="top") {
  topBotCodingWORK(x=obj,value, replacement, kind=kind)
})

topBotCodingWORK <- function(x, value, replacement, kind="top"){ #COLUMN FOR COMPATIBILITY WITH V4
  if( kind == "top"){
    x[x > value] <- replacement
  } else{
    x[x < value] <- replacement
  }
  invisible(x)
}
