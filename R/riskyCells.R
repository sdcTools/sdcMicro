#' riskyCells
#'
#' Allows to compute risky (unweighted) combinations of key variables either up to a specified dimension or using
#' identification level. This mimics the approach taken in mu-argus.  
#'
#' @name riskyCells
#' @docType methods
#' @param obj a \code{data.frame}, \code{data.table} or an object of class \code{\link{sdcMicroObj-class}}
#' @param ... see possible arguments below
#' \itemize{
#' \item{keyVars: }{index or variable-names within \code{obj} that should be used for tabulation. In case \code{obj} is 
#' of class \code{\link{sdcMicroObj-class}}, this argument is not used and the pre-defined key-variables are used.}
#' \item{useIdentificationLevel: }{logical, specifies if tabulation should be done up to a specific 
#' dimension (\code{useIdentificationLevel=FALSE} using argument \code{maxDim}) or taking identification 
#' levels (\code{useIdentificationLevel=FALSE} using argument \code{level}) into account.}
#' \item{level: }{in case \code{useIdentificationLevel=TRUE}, this numeric vector specifies the importance of the key variables.
#' The construction of output tables follows the implementation in mu-argus, see e.g \url{http://neon.vb.cbs.nl/casc/Software/MUmanual5.1.pdf}.}
#' \item{maxDim: }{in case \code{useIdentificationLevel=FALSE}, this number specifies maximal number of variables to tablulate.}
#' \item{threshold: }{a numeric vector specifiying the thresholds at which cells are considered to be unsafe. In case a 
#' tabulation is done up to a specific level (\code{useIdentificationLevel=FALSE}), the thresholds my be 
#' specified differently for each dimension. In the other case, the same threshold is used for all tables.}
#' }
#' @return a \code{data.table} showing the number of unsafe cells, thresholds for any combination of the key variables. If
#' the input was a \code{\link{sdcMicroObj-class}} object and some modifications have been already applied to the categorical
#' key variables, the resulting output contains the number of unsafe cells both for the original and the modified data.
#' @keywords manip
#' @author Bernhard Meindl
#' @export
#' @examples
#' ## data.frame method / all combinations up to maxDim
#' riskyCells(testdata2, keyVars=c(1:5), threshold=c(50,25,10,5), 
#'   useIdentificationLevel=FALSE, maxDim=4)
#' riskyCells(testdata2, keyVars=c(1:5), threshold=10, 
#'   useIdentificationLevel=FALSE, maxDim=3)
#' 
#' ## data.frame method / using identification levels
#' riskyCells(testdata2, keyVars=c(1:6), threshold=20, 
#'   useIdentificationLevel=TRUE, level=c(1,1,2,3,3,5))
#' riskyCells(testdata2, keyVars=c(1,3,4,6), threshold=10, 
#'   useIdentificationLevel=TRUE, level=c(1,2,2,4))
#'
#' ## sdcMicroObj-method / all combinations up to maxDim
#' testdata2[1:6] <- lapply(1:6, function(x) { 
#'   testdata2[[x]] <- as.factor(testdata2[[x]]) 
#' })
#' sdc <- createSdcObj(testdata2, 
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' 
#' r0 <- riskyCells(sdc, useIdentificationLevel=FALSE, threshold=c(20,10,5), maxDim=3)
#' ## in case key-variables have been modified, we get counts for original and modified data
#' sdc <- groupAndRename(sdc, var="roof", before=c("5","6","9"), after=c("5+"))
#' r1 <- riskyCells(sdc, useIdentificationLevel=FALSE, threshold=c(10,5,3), maxDim=3)
#' 
#' ## sdcMicroObj-method / using identification levels
#' riskyCells(sdc, useIdentificationLevel=TRUE, threshold=10, level=c(c(1,1,3,4,5,5,5)))
riskyCells <- function(obj, ...) {
  riskyCellsX(obj=obj, ...)
}
setGeneric("riskyCellsX", function(obj, ...) {
  standardGeneric("riskyCellsX")
})

setMethod(f="riskyCellsX", signature=c(obj="data.frame"),
definition = function(obj, keyVars, useIdentificationLevel, ...) {
  stopifnot(is.logical(useIdentificationLevel))
  stopifnot(length(useIdentificationLevel)==1)
  
  if (is.character(keyVars)) {
    stopifnot(all(keyVars %in% colnames(obj)))
    keyVars <- match(keyVars, colnames(obj))
  }
  if (is.numeric(keyVars)) {
    stopifnot(all(keyVars %in% 1:ncol(obj)))
  }
  
  if (!is.data.table(obj)) {
    obj <- as.data.table(obj)
  }
  res <- riskyCellsWork(df=obj, keyVars=keyVars, useIdentificationLevel=useIdentificationLevel, ...)
  res
})

setMethod(f="riskyCellsX", signature = c(obj="sdcMicroObj"),
definition=function(obj, useIdentificationLevel=useIdentificationLevel, ...) {
  unsafe_cells <- NULL
  mod_keyvars <- !identical(obj@manipKeyVars, obj@origData[,obj@keyVars])
  
  res <- riskyCellsX(obj=obj@origData, keyVars=obj@keyVars, useIdentificationLevel=useIdentificationLevel, ...)
  if (mod_keyvars) {
    setnames(res,ncol(res),c("unsafe_cells_orig")) 
  }
  
  if (!identical(obj@manipKeyVars, obj@origData[,obj@keyVars])) {
    res2 <- riskyCellsX(obj=obj@manipKeyVars, keyVars=obj@keyVars, useIdentificationLevel=useIdentificationLevel, ...)
    res$unsafe_cells_manip <- res2[,unsafe_cells]
  }
  res
})

riskyCellsWork <- function(df, keyVars, useIdentificationLevel=FALSE, ...) {
  N <- th <- NULL
  
  riskycells_upto_dimension <- function(dat, keyVars, maxDim, threshold) {  
    if (length(threshold)==1) {
      threshold <- rep(threshold, maxDim)
    } else {
      stopifnot(length(threshold)==maxDim)
    }
    
    out <- list()
    length(out) <- maxDim 
    
    tmp <- dat[,keyVars, with=F]
    keyVars <- 1:ncol(tmp)
    for (dd in 1:maxDim) {
      todo_combs <- combn(names(tmp)[keyVars], dd)
      res <- matrix(NA, ncol=maxDim+1, nrow=ncol(todo_combs))
      
      for (i in 1:ncol(todo_combs)) {
        vv <- todo_combs[,i]
        setkeyv(tmp, vv)
        agg <- tmp[,.N, by=key(tmp)]
        agg <- agg[N<=threshold[dd]]
        
        nr_unsafe <- nrow(agg)
        res[i, 1] <- nr_unsafe
        res[i, 2:(2+length(vv)-1)] <- vv
      }
      res <- as.data.table(res)
      res[,th:=threshold[dd]]
      out[[dd]] <- as.data.table(res)
    }
    out <- rbindlist(out)
    setnames(out, c("unsafe_cells", paste0("dim", 1:maxDim),"threshold"))
    out
  }
  riskycells_using_identification_level <- function(dat, keyVars, level, threshold) {
    stopifnot(is.numeric(threshold))
    stopifnot(length(threshold)==1)
    stopifnot(threshold==as.integer(threshold))
    stopifnot(threshold>=1)
    
    dt <- data.table(v=names(dat)[keyVars], l=level)
    
    ## compute combinations
    maxDimN <- length(unique(dt$l))
    
    res <- riskycells_upto_dimension(dat, keyVars=keyVars, maxDim=maxDimN, threshold=threshold)
    
    # add levels to result 
    #ids <- 2:(ncol(res)-1)
    #dims <- do.call("cbind", 
    #lapply(ids, function(x) {
    #  dt$l[match(res[[x]], dt$v)]
    #}))
    #dims <- as.data.table(dims)
    #setnames(dims, paste0("dim",1:ncol(dims)))
    #res <- cbind(res, dims)
    
    # remove some combinations, aka implement the "dutch approach"
    #has_dups <- sapply(1:nrow(dims), function(x) {
    #  v <- na.omit(as.numeric(dims[x]))
    #  !any(duplicated(v)) | 1 %in% v | length(unique(v))==1
    #})
    #cbind(dims, has_dups)
    #dims[,has_dups:=has_dups]
    res
  }
  
  stopifnot("data.frame" %in% class(df))
  if (!is.data.table(df)) {
    df <- as.data.table(df)
  }
  
  params <- list(...)
  threshold <- params$threshold
  stopifnot(is.numeric(threshold))
  stopifnot(all(threshold==as.integer(threshold)))
  if (useIdentificationLevel) {
    level <- params$level
    stopifnot(is.numeric(level))
    stopifnot(length(level)==length(keyVars))
    stopifnot(all(level==as.integer(level)))
    res <- riskycells_using_identification_level(dat=df, keyVars=keyVars, level=level, threshold=threshold)
  } else {
    maxDim <- params$maxDim
    stopifnot(is.numeric(maxDim))
    
    stopifnot(maxDim<=length(keyVars))
    res <- riskycells_upto_dimension(dat=df, keyVars=keyVars, maxDim=maxDim, threshold=threshold)
  }
  res <- res[,c(2:ncol(res),1), with=F]
  res[]
}