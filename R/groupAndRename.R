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
#' @param after vector of levels after recoding
#' @return the modified \code{\link{sdcMicroObj-class}}
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
groupAndRename <- function(obj, var, before, after) {
  groupAndRenameX(obj=obj, var=var, before=before, after=after)
}

setGeneric("groupAndRenameX", function(obj, var, before, after) {
  standardGeneric("groupAndRenameX")
})

setMethod(f="groupAndRenameX", signature=c("factor"),
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
  if (any(is.na(obj))) {
    obj <- addNA(obj)
  }
  obj
})

setMethod(f="groupAndRenameX", signature=c("data.frame"),
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

setMethod(f="groupAndRenameX", signature=c("sdcMicroObj"),
definition=function(obj, var, before, after) {
  obj <- nextSdcObj(obj)
  manipKey <- get.sdcMicroObj(obj, type="manipKeyVars")
  manipKey[[var]] <- groupAndRename(manipKey[[var]], var=var, before=before, after=after)
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(manipKey))
  # calculate risk
  obj <- calcRisks(obj)
  obj
})
