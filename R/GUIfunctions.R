setGeneric('removeDirectID', function(obj,var) {standardGeneric('removeDirectID')})
setMethod(f='removeDirectID', signature=c('sdcMicroObj'),
    definition=function(obj, var) {
      kV <- colnames(obj@origData)[get.sdcMicroObj(obj,"keyVars")]
      if(any(var%in%kV))
        stop("A direct identifier should not be seleceted as key variable.")
      o <- obj@origData
      if(any(!var%in%colnames(o)))
        stop("direct identifier variable not found on data set")
      o[,colnames(o)%in%var] <- NA
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
      x <- get.sdcMicroObj(obj, type="manipKeyVars")
      obj <- nextSdcObj(obj)
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
