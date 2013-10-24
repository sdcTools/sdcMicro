setGeneric('pram', function(obj, pramVar=NULL,pd=0.8, alpha=0.5) {standardGeneric('pram')})
setMethod(f='pram', signature=c('sdcMicroObj'),
    definition=function(obj, pramVar=NULL,pd=0.8, alpha=0.5) { 
      
      if ( length(pramVar) > 1 ) {
        stop("pram: parameter 'pramVar' must be of length 1!\n")
      }
      if(pramVar%in%colnames(obj@manipKeyVars)){
        x <- as.factor(obj@manipKeyVars[,pramVar])
      }else if(pramVar%in%colnames(obj@manipPramVars)){
        x <- as.factor(obj@manipPramVars[,pramVar])
      }else{
        x <- as.factor(obj@origData[,pramVar])
      }
      pW <- pramWORK(x=x,pd=pd,alpha=alpha)
      summarypW <- summary(pW)
      obj <- nextSdcObj(obj)
      if(pramVar%in%colnames(obj@manipKeyVars)){
		warning("If pram is applied on key variables, the k-anonymity and risk assesement are not reasonable anymore.\n")
        obj@manipKeyVars[,pramVar] <- pW$xpramed
        obj@pramVars <- c(obj@pramVars,match(pramVar,colnames(obj@origData))) 
      }else if(pramVar%in%colnames(obj@manipPramVars)){
        obj@manipPramVars[,pramVar] <- pW$xpramed
      }else{
        x <- obj@origData[,pramVar,drop=FALSE]
        x[,pramVar] <- pW$xpramed
		
		mPv <- get.sdcMicroObj(obj, type="manipPramVars")
		if ( is.null(mPv) ) {
			obj <- set.sdcMicroObj(obj, type="manipPramVars", input=list(x)) 
		} else {
			obj <- set.sdcMicroObj(obj, type="manipPramVars", input=list(cbind(mPv,x))) 
		}
        obj@pramVars <- c(obj@pramVars,match(pramVar,colnames(obj@origData)))
      }
      obj <- calcRisks(obj)  
      obj <- set.sdcMicroObj(obj, type="pram", input=list(summarypW))
      obj
    })
setMethod(f='pram', signature=c("ANY"),
    definition=function(obj, pramVar=NULL,pd=0.8, alpha=0.5) { 
      pramWORK(x=obj,pramVar=pramVar,pd=pd,alpha=alpha)
    })
pramWORK <- function(x, pramVar=NULL, pd=0.8, alpha=0.5){ #COLUMN FOR V4 COMPATIBILITY
  fac <- FALSE
  recoding <- FALSE
  xpramed <- x
  if(class(x) != "factor") warning("pram makes only sense for categorical variables stored as factors")
  
  if(class(x) == "factor"){ 
    fac <- TRUE
  } else{
    fac <- FALSE
    xpramed <- as.factor(xpramed)
  }	  
  lev <- levels(xpramed)
#	  xpramed <- as.numeric(as.character(xpramed))
  
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
    for (i in 1:length(un)) 
      xpramed[which(tmp==un[i])] <- i	 
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
  
  #Qest <-  sapply(seq(L), function(i) apply(P, 1, function(x) p[i]*x)[,i])/p*P
  
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
    if(length(unique(xpramed))==length(lev))
      xpramed <- factor(xpramed, labels=lev)
    else{
      xpramed <- factor(xpramed, labels=lev[sort(unique(xpramed))]) 
    }
  }
  if(fac == FALSE & class(x) == "character") xpramed <- as.character(factor(xpramed, labels=lev))
  res <- list(x=x, xpramed=xpramed, pd=pd, P=P, Rs=Rs, alpha=alpha)
  class(res) <- "pram"
  invisible(res)
}


