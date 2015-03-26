generateStrata <- function(df,stratavars,name){
  strata <- rep("",nrow(df))
  for(i in seq_along(stratavars)){
    strata <- paste(strata,df[,stratavars[i]],sep="")
    if(length(stratavars)>i)
      strata <- paste(strata,"-",sep="")
  }
  df <- cbind(df,strata)
  colnames(df)[length(colnames(df))] <- name
  return(df)
}

setGeneric('removeDirectID', function(obj,var) {standardGeneric('removeDirectID')})
setMethod(f='removeDirectID', signature=c('sdcMicroObj'),
    definition=function(obj, var) {
      kV <- colnames(obj@origData)[get.sdcMicroObj(obj,"keyVars")]
      nV <- colnames(obj@origData)[get.sdcMicroObj(obj,"numVars")]
      wV <- colnames(obj@origData)[get.sdcMicroObj(obj,"weightVar")]
      sV <- colnames(obj@origData)[get.sdcMicroObj(obj,"strataVar")]
      hV <- colnames(obj@origData)[get.sdcMicroObj(obj,"hhId")]

      if(any(var%in%kV))
        stop("A direct identifier should not be seleceted as key variable.\n Therefore it can not be removed.")
      if(any(var%in%nV))
        stop("A direct identifier should not be seleceted as numerical key variable.\n Therefore it can not be removed.")
      if(any(var%in%wV))
        stop("A direct identifier should not be seleceted as weight variable.\n Therefore it can not be removed.")
      if(any(var%in%sV))
        stop("A direct identifier should not be seleceted as strata variable.\n Therefore it can not be removed.")
      if(any(var%in%hV))
        stop("A direct identifier should not be seleceted as cluster ID.\n Therefore it can not be removed.")

      o <- obj@origData
      if(any(!var%in%colnames(o)))
        stop("direct identifier variable not found on data set")
      o[,!colnames(o)%in%var,drop=FALSE]
      obj <- nextSdcObj(obj)
      obj@deletedVars <- c(obj@deletedVars,var)
      obj@origData <- o
      obj
    })
setGeneric('varToFactor', function(obj,var) {standardGeneric('varToFactor')})
setMethod(f='varToFactor', signature=c('sdcMicroObj'),
    definition=function(obj, var) {
      x <- get.sdcMicroObj(obj, type="manipKeyVars")
      obj <- nextSdcObj(obj)
      x[,var] <- as.factor(x[,var])
      obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(as.data.frame(x)))
      obj
    })

setGeneric('varToNumeric', function(obj,var) {standardGeneric('varToNumeric')})
setMethod(f='varToNumeric', signature=c('sdcMicroObj'),
    definition=function(obj, var) {
      x <- get.sdcMicroObj(obj, type="manipKeyVars")
      obj <- nextSdcObj(obj)
      suppressWarnings(tmpvar <- as.numeric(as.character(x[,var])))
      x[,var] <- tmpvar
      obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(as.data.frame(x)))
      obj
    })
setGeneric('groupVars', function(obj,var, before,after) {standardGeneric('groupVars')})
setMethod(f='groupVars', signature=c('sdcMicroObj'),
    definition=function(obj,var, before,after) {
      if ( length(before)!=length(after) ) {
        stop("Arguments 'before' and 'after' have different length!\n")
      }
      x <- get.sdcMicroObj(obj, type="manipKeyVars")
      obj <- nextSdcObj(obj)
      if ( !all(before %in% levels(x[,var])) ) {
        stop("some elements of 'before' are not valid levels in variable 'var'!\n")
      }
      if ( any(duplicated(before)) ) {
        stop("each level from the original factor must be listed only once in argument 'before'!")
      }
      for( i in 1:length(before) ) {
        levels(x[,var]) <- ifelse(levels(x[,var])==before[i], after, levels(x[,var]))
      }
      obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(x))
      obj
    })
setGeneric('renameVars', function(obj,var, before,after) {standardGeneric('renameVars')})
setMethod(f='renameVars', signature=c('sdcMicroObj'),
    definition=function(obj,var, before,after) {
      x <- get.sdcMicroObj(obj, type="manipKeyVars")
      obj <- nextSdcObj(obj)
      levels(x[,var]) <- ifelse(levels(x[,var])==before, after, levels(x[,var]))
      obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(x))
      obj
    })
