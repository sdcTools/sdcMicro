setGeneric('pram_strata', function(obj, variables=NULL,strata_variables=NULL,pd=0.8, alpha=0.5) {
      standardGeneric('pram_strata')
    })
setMethod(f='pram_strata', signature=c('ANY'),
    definition=function(obj, variables=NULL,strata_variables=NULL,pd=0.8, alpha=0.5) {
      .Deprecated("pram",package="sdcMicro",old="pram_strata")
      pram(obj,variables=variables,strata_variables=strata_variables,pd=pd,alpha=alpha)
    })
setGeneric('pram', function(obj, variables=NULL,strata_variables=NULL,pd=0.8, alpha=0.5) {
  standardGeneric('pram')
})
setMethod(f='pram', signature=c('sdcMicroObj'),
	definition=function(obj, variables=NULL,strata_variables=NULL,pd=0.8, alpha=0.5) { 
    pramVars <- get.sdcMicroObj(obj, type="pramVars")
    if ( length(pramVars) == 0 && is.null(variables) ) {
      stop("Error: slot pramVars is NULL and argument 'variables' was not specified!\nDefine one of them to use pram on these variables\n")
    }
    if(is.null(variables)){
      pramVars <- colnames(obj@origData)[get.sdcMicroObj(obj, type="pramVars")] 
    }else{
      pramVars <- variables
    }
  ### Get data from manipPramVars
  manipPramVars <- get.sdcMicroObj(obj, type="manipPramVars")
  strataVars <- get.sdcMicroObj(obj, type="strataVar")
  manipKeyVars <- get.sdcMicroObj(obj, type="manipKeyVars")
  kVar <- pramVars[pramVars%in%colnames(manipKeyVars)]
  pVar <- pramVars[pramVars%in%colnames(manipPramVars)]
  rVar <- pramVars[!pramVars%in%c(kVar,pVar)]
  

  if(length(kVar)>0){
    warning("If pram is applied on key variables, the k-anonymity and risk assessment are not useful anymore.\n")
    manipData <- manipKeyVars[,kVar,drop=FALSE]
  }
  if(length(pVar)>0){
    if(exists("manipData")) {
      manipData <- cbind(manipData,manipPramVars[,pVar,drop=FALSE])
    } else {
      manipData <- manipPramVars[,pVar,drop=FALSE]
    }      
  }
  if(length(rVar)>0){
    if(exists("manipData")) {
      manipData <- cbind(manipData,obj@origData[,rVar,drop=FALSE])
    } else {
      manipData <- obj@origData[,rVar,drop=FALSE]
    }      
  }  
  if(!exists("manipData")){
    manipData <- obj@origData[,pramVars,drop=FALSE]
  }
  if(!is.null(strata_variables)){
    sData <- get.sdcMicroObj(obj, type="origData")[,strata_variables,drop=FALSE]
    manipData <- cbind(manipData, sData)
    strataVars <- c(length(pramVars):length(manipData))
  }else if(length(strataVars)>0) {
	  sData <- get.sdcMicroObj(obj, type="origData")[,strataVars,drop=FALSE]
	  manipData <- cbind(manipData, sData)
	  strataVars <- c(length(pramVars):length(manipData))
  }
	
  res <- pramWORK(data=manipData,variables=pramVars,strata_variables=strataVars,pd=pd,alpha=alpha)
  tmp <- data.frame(unclass(res))
  manipData[,pramVars] <- tmp[,paste(pramVars,"_pram",sep=""),drop=FALSE]
  obj <- nextSdcObj(obj)
  
  if(length(pVar)>0){
    manipPramVars[,pVar] <- manipData[,pVar]
  }
  if(length(rVar)>0){
	  if ( is.null(manipPramVars))
		  manipPramVars <- manipData[,rVar,drop=FALSE]
	  else 
		  manipPramVars <- cbind(manipPramVars,manipData[,rVar,drop=FALSE])
  }
  obj <- set.sdcMicroObj(obj, type="manipPramVars", input=list(manipPramVars))
  
  if(length(kVar)>0){
    manipKeyVars[,kVar] <- manipData[,kVar]
    obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(manipKeyVars))
  }
  pram <- get.sdcMicroObj(obj, type="pram")
  if ( is.null(pram)) {
	  pram <- list()
  }
  pram$pd <- pd
  pram$alpha <- alpha

  pram$summary <- print.pram(res)
  obj <- set.sdcMicroObj(obj, type="pram", list(pram))
  pramVarInd <- standardizeInput(obj, pramVars)
  obj <- set.sdcMicroObj(obj, type="pramVars", input=list(pramVarInd))
  obj <- calcRisks(obj)
  obj
})

setMethod(f='pram', signature=c("data.frame"),
	definition=function(obj,variables=NULL,strata_variables=NULL,pd=0.8, alpha=0.5) { 
	  pramWORK(data=obj,variables=variables,strata_variables=strata_variables,pd=pd,alpha=alpha)
})

setMethod(f='pram', signature=c("matrix"),
	definition=function(obj,variables=NULL,strata_variables=NULL, pd=0.8, alpha=0.5) { 
	  pramWORK(data=obj,variables=variables,strata_variables=strata_variables,pd=pd,alpha=alpha)
})

setMethod(f='pram', signature=c("vector"),
	definition=function(obj,variables=NULL,strata_variables=NULL,pd=0.8, alpha=0.5) { 
    xx <- data.frame(x=obj, stringsAsFactors=FALSE)    
	  pramWORK(data=xx,variables="x",strata_variables=NULL,pd=pd,alpha=alpha)
  }
)

#require(sdcMicro)
#data(testdata)
#dat <- testdata[,c("urbrur","sex","roof")]
#dat$roof_pram <- 0

# handling of NA and NULL Values for weight-vector
## with strata
#x <- .Call("Pram",as.matrix(dat),-999,2,1,-1)
## only frequency
#x <- .Call("Pram",as.matrix(dat),-999,0,0,-1)
pramWORK <- function(data,variables=NULL,strata_variables=NULL,pd=0.8, alpha=0.5){
  # pram on a simple vector
  do.pram <- function(x, pd, alpha) {
    if(class(x) != "factor") {
      warning("pram makes only sense for categorical variables stored as factors")
    }    
    fac <- FALSE
    recoding <- FALSE
    xpramed <- x    
    
    if(class(x) == "factor"){ 
      fac <- TRUE
    } else{
      fac <- FALSE
      xpramed <- as.factor(xpramed)
    }    
    lev <- levels(xpramed)
    xpramed <- as.integer(as.factor(xpramed))
    
    # Recoding necessary if factors != 1:...
    recodeNAS <- FALSE
    nas <- which(is.na(xpramed))
    if(length(nas) > 0) {
      NAKat <- max(xpramed, na.rm=TRUE)+1	
      xpramed[nas] <- NAKat
      recodeNAS <- TRUE
    }	  
    
    if(min(xpramed, na.rm=TRUE)!=1 | max(xpramed, na.rm=TRUE) != length(unique(xpramed))) {
      recoding <- TRUE
      tmp <- xpramed
      xpramed <- rep(NA, length(tmp))
      un <- sort(unique(tmp))
      xpramedBack <- list()
      xpramedBack[[1]] <- un
      xpramedBack[[2]] <- 1:length(un)
      for (i in 1:length(un)) {
        xpramed[which(tmp==un[i])] <- i   
      }        
    }
    
    L <- length(table(xpramed))
    P <- matrix(, ncol=L, nrow=L)
    pds <- runif(L, min=pd, max=1)
    tri <- (1 - pds)/(L-1)
    for(i in seq(L)){
      P[i,] <- tri[i]
    }
    diag(P) <- pds
    p <- table(xpramed)/sum(as.numeric(xpramed))
    Qest <- P
    for(k in seq(L)){
      s <- sum(p*P[,k])
      for(j in seq(L)){
        Qest[k,j] <- P[j,k]*p[j]/s
      }
    }
    R <- P %*% Qest
    EI <- matrix(0, ncol=L, nrow=L)
    diag(EI) <- 1
    Rs <- alpha * R + (1 - alpha) * EI
    
    for(i in 1:length(xpramed)){
      xpramed[i] <- sample(1:L, 1, prob=Rs[xpramed[i],])
    }
    
    # Recoding necessary??
    if(recoding==TRUE) {
      xpramedFinal <- rep(NA, length(tmp))
      for (i in 1:length(xpramedBack[[1]]))	{
        xpramedFinal[which(xpramed==i)] <- xpramedBack[[1]][i]  
      }  
      xpramed <- xpramedFinal
    }
    
    if(recodeNAS==TRUE) {
      nas <- which(xpramed==NAKat)
      if(length(nas) > 0) {
        xpramed[nas] <- NA
      }	  	  
    }    
    if(fac == TRUE){
      if(length(unique(xpramed))==length(lev)) {
        xpramed <- factor(xpramed, labels=lev)
      } else{
        xpramed <- factor(xpramed, labels=lev[sort(unique(xpramed))]) 
      }
    }
    if(fac == FALSE & class(x) == "character") {
      xpramed <- as.character(factor(xpramed, labels=lev))
    }
    return(list(x=x, xpramed=xpramed))
  }
  
	if(is.null(variables)){
	  stop("Please define valid variables to pram!\n")
	}
    
  if(length(strata_variables)>0){
		data$idvarpram <- 1:nrow(data)
		fac <- list()
		chara <- list()
		f <- rep("",nrow(data))
		for(sv in strata_variables){
			f <- paste(f,as.character(data[,sv]),"_",sep="")
		}
		f <- as.factor(f)
		s <- split(data[,c(variables,"idvarpram"),drop=FALSE],f)
		for(i in 1:length(variables)){
			v <- variables[i]
			fac[[i]] <- FALSE
			if(!is.factor(data[,v])&is.character(data[,v])){
				fac[[i]] <- TRUE
				chara[[i]] <- TRUE
			}else if(!is.factor(data[,v])&is.numeric(data[,v])){
				chara[[i]] <- FALSE
				data[,v] <- as.character(data[,v])
			}else{
				fac[[i]] <- TRUE
				data[,v] <- as.character(data[,v])
			}
			for(si in 1:length(s)){
				s[[si]][,paste(v,"_pram",sep="")] <- as.character(do.pram(x=as.factor(s[[si]][,v]),pd=pd,alpha=alpha)$xpramed)
			}
		}
		r <- vector()
		for(si in 1:length(s)){
			r <- rbind(r,s[[si]])
		}
		for(i in 1:length(variables)){
			v <- variables[i]
			if(!fac[[i]]){
				if(chara[[i]]){
					data[,v] <- as.character(data[,v])
					data[,paste(v,"_pram",sep="")] <- as.character(r[,paste(v,"_pram",sep="")])
				}else{
					data[,v] <- as.numeric(data[,v])
					data[,paste(v,"_pram",sep="")] <- as.numeric(r[,paste(v,"_pram",sep="")])
				}
			}else{
				data[,v] <- as.factor(data[,v])
				data[,paste(v,"_pram",sep="")] <- as.factor(r[,paste(v,"_pram",sep="")])
			}			
		}
		data <- data[,-which(colnames(data)=="idvarpram"),drop=FALSE]
	}else{
		for(v in variables){
			if(is.factor(data[,v]))
				data[,paste(v,"_pram",sep="")] <- do.pram(x=data[,v],pd=pd,alpha=alpha)$xpramed
			else if(is.numeric(data[,v])){
				data[,paste(v,"_pram",sep="")] <- as.numeric(as.character(do.pram(x=as.factor(as.character(data[,v])),pd=pd,alpha=alpha)$xpramed))
			}else if(is.character(data[,v])){
				data[,paste(v,"_pram",sep="")] <- as.character(do.pram(as.factor(x=data[,v]),pd=pd,alpha=alpha)$xpramed)
			}
		}
	}
	res <- data
  class(res) <- "pram"
	invisible(res)	
}

print.pram <- function(x, ...){
  x <- as.data.frame(unclass(x))
	x <- apply(x, 2, as.character)
	x[is.na(x)] <- "." # NA comparisons -> cast to character (better solution?)
	pram_var <- colnames(x)[grep("pram",colnames(x))]
	var <- unlist(lapply(pram_var,function(x)substring(x,1,nchar(x,type="width")-5)))
	
	df <- data.frame(variable=var, nrChanges=NA, percChanges=NA, stringsAsFactors=FALSE)
	cat("Number of changed observations: \n")
	cat("- - - - - - - - - - - \n")
	for(i in 1:length(pram_var)){
		# FIXME: factor levels of x[,var[i]] and x[,pram_var[i]] not always the same!
		#s <- sum(x[,var[i]]!=x[,pram_var[i]])
		s <- sum(as.character(x[,var[i]])!=as.character(x[,pram_var[i]]))
		p <- round(s/nrow(x)*100,2)
		cat(var[i]," != ",pram_var[i]," : ",s," (",p,"%)","\n",sep="")
		df[i,"nrChanges"] <- s
		df[i,"percChanges"] <- p
	}
	return(invisible(df))
}
