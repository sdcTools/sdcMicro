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
#' @docType methods
#' @param obj object of class \code{\link{sdcMicroObj-class}}
#' @param var name of the keyVariable to change
#' @param before vector of levels before recoding
#' @param after name of new level after recoding
#' @param addNA logical, if TRUE missing values in the input variables are added to the level specified in argument \code{after}.
#' @return the modified \code{\link{sdcMicroObj-class}}
#' @keywords methods
#' @author Bernhard Meindl
#' @references 
#' Templ, M. and Kowarik, A. and Meindl, B. 
#' Statistical Disclosure Control for Micro-Data Using the R Package sdcMicro. 
#' \emph{Journal of Statistical Software}, \strong{67} (4), 1--36, 2015. \doi{10.18637/jss.v067.i04}
#' @export
#' @examples
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' testdata2$urbrur <- as.factor(testdata2$urbrur)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- groupAndRename(sdc, var="urbrur", before=c("1","2"), after=c("1"))
groupAndRename <- function(obj, var, before, after, addNA=FALSE) {
  groupAndRenameX(obj=obj, var=var, before=before, after=after, addNA=addNA)
}

setGeneric("groupAndRenameX", function(obj, var, before, after, addNA=FALSE) {
  standardGeneric("groupAndRenameX")
})

setMethod(f="groupAndRenameX", signature=c("factor"),
definition=function(obj, var, before, after, addNA=FALSE) {
  if (!all(before %in% levels(obj))) {
    stop("some elements of 'before' are not valid levels in the input factor!\n")
  }
  if (any(duplicated(before))) {
    stop("each level from the original factor must be listed only once in argument 'before'!")
  }
  ll <- levels(obj)
  ll[ll %in% before] <- after[1]
  levels(obj) <- ll
  # add missing value (NA) to newly created level!
  if (addNA) {
    obj[is.na(obj)] <- after[1]
  }
  obj
})

setMethod(f="groupAndRenameX", signature=c("data.frame"),
definition=function(obj, var, before, after, addNA=FALSE) {
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
  obj[[var]] <- groupAndRename(obj[[var]], var=NULL, before=before, after=after, addNA=addNA)
  obj
})

setMethod(f="groupAndRenameX", signature=c("sdcMicroObj"),
definition=function(obj, var, before, after, addNA=FALSE) {
  obj <- nextSdcObj(obj)
  manipKey <- get.sdcMicroObj(obj, type="manipKeyVars")
  if (!var %in% colnames(manipKey)) {
    stop("variable specified in 'var' is not available in 'obj'!\n")
  }
  manipKey[[var]] <- groupAndRename(manipKey[[var]], var=var, before=before, after=after, addNA=addNA)
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(manipKey))
  # calculate risk
  obj <- calcRisks(obj)
  obj
})
