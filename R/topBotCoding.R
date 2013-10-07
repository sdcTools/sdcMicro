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
      topBotCodingWORK(x=obj,value, replacement, kind="top", column=NULL)
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


