setGeneric('localSuppression', function(obj,k=2, importance=NULL,...) {standardGeneric('localSuppression')})
setMethod(f='localSuppression', signature=c('sdcMicroObj'),
    definition=function(obj,k=2, importance=NULL,...) { 
      ### get data from origData
      # x <- get.sdcMicroObj(obj, type="origData")
      # keyVars <- get.sdcMicroObj(obj, type="keyVars")
      
      ### get data from manipKeyVars
      x <- get.sdcMicroObj(obj, type="manipKeyVars")    
      keyVars <- c(1:length(x))
      
      ls <- localSuppressionWORK(x=x, k=k,importance=importance,keyVars=keyVars, ...)
      
      obj <- nextSdcObj(obj)
      
      obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(ls$xAnon))
      
      a <- get.sdcMicroObj(obj, type="origData")[,get.sdcMicroObj(obj, type="keyVars")]
      b <- ls$xAnon
      w = which(is.na(a))
      r = w%%nrow(a)
      cc = ceiling(w/nrow(a))
      wb = which(is.na(b))
      rb = wb%%nrow(b)
      cb = ceiling(wb/nrow(b))
      d = data.frame(id=1:ncol(a), before=NA, after=NA)
      d[,2:3] <- t(sapply(1:ncol(a), function (x) c(sum(cc==x), sum(cb==x))))
      obj <- set.sdcMicroObj(obj, type="localSuppression", input=list(list(d$after-d$before)))
      
      obj <- calcRisks(obj)
      
      obj
    })
setMethod(f='localSuppression', signature=c("data.frame"),
    definition=function(obj,k=2, importance=NULL,...) { 
      localSuppressionWORK(x=obj,k=k,importance=importance,...)
    })
setMethod(f='localSuppression', signature=c("matrix"),
    definition=function(obj,k=2, importance=NULL,...) { 
      localSuppressionWORK(x=obj,k=k,importance=importance,...)
    })


localSuppressionWORK <- function(x, keyVars,  k=2, importance=NULL) {
	my.dist <- function(dat, keyVars, ind) {
		l <- lapply(1:length(keyVars), function(x) { dat[,keyVars[x]] != dat[ind,keyVars[x]] }) 
		for (i in seq_along(l) ) {
			xind <- which(is.na(l[[i]]))
			if ( length(xind) > 0 )  {
				l[[i]][xind] <- FALSE
			}
		}
		dists <- Reduce("+", l)
		dists[ind] <- 0
		dists
	}
  if(is.numeric(keyVars))
    keyVars <- colnames(x)[keyVars]
  if ( is.null(importance) ) {
    xx <- apply(x[,keyVars,drop=FALSE], 2, function(x) { length(table(x)) } )
    importance <- match(xx, sort(xx, decreasing=FALSE))
  } else {
    if ( length(setdiff(sort(importance), 1:length(keyVars))) > 0 ) {
      stop("importance vector needs to be discrete numbers between 1 and the number of key-variables!\n")
    } 	
  }
  
  # nr supps before doing anything
  NABefore <- is.na(x)
  totalNABefore <- length(which(NABefore))
  NAinKey <- apply(x[, keyVars], 2, function(x) length(which(is.na(x))))

  ##############
  #The dataset is reduced to a smaller dataset in the following way
  #1)All missing values are initialized with the first unique value of the corresponding keyVariable
  #2)fk is computed
  #3)from groups with fk>k, all observations except k observations are removed
  #4)Afterwards the old lS-Algo is applied
  #5)The last step is to merge the smaller k-anonymized data set back to the original data set
	#with initial NAs introduced again
  x$idvarextraforsls <- 1:nrow(x)
  xKeys <- data.table(x[,c(keyVars,"idvarextraforsls")])
  for(kV in keyVars){
    cmd <- paste("xKeys[is.na(",kV,"),",kV,":=unique(xKeys$",kV,")[1]]")
    eval(parse(text=cmd))
  }
  cmd <- paste("xKeys <- xKeys[",paste("!is.na(",keyVars,")",collapse="&",sep=""),"]",sep="")
  eval(parse(text=cmd))
  setkeyv(xKeys,keyVars)
  cmd <- paste("erg <- xKeys[,list(fk=.N),by=list(",paste(keyVars,collapse=","),")]",sep="")
  eval(parse(text=cmd))
  xKeys <- merge(xKeys,erg)
  weg <- fkd <-idvarextraforsls <- fk <- NA#for CHECK-NOTES
  
  erg <- xKeys[fk>k] # more than k
  erg[,fkd:=fk-k]
  if(nrow(erg)>0){
    cmd <- paste("erg2 <- erg[,tail(.SD,fkd[1]),by=list(",paste(keyVars,collapse=","),")]",sep="")
    eval(parse(text=cmd))
    xKeys <- data.table(x)
    setkey(xKeys,"idvarextraforsls")
    erg2 <- erg2[,list(idvarextraforsls)]
    erg2[,weg:=1]
    setkey(erg2,"idvarextraforsls")
    xKeys <- merge(xKeys,erg2,all=TRUE)
    x <- data.frame(xKeys[is.na(weg),])
  }
  ##############
  ff <- freqCalc(x, keyVars=keyVars)
  rk <- indivRisk(ff)
  runInd <- TRUE
  
  importanceI <- (length(importance)+1)-importance
  
  while ( runInd ) {
    ind.problem <- which(ff$fk < k)
    ind.problem  <- ind.problem[order(rk$rk[ind.problem],decreasing=TRUE)]
    for ( i in 1:length(ind.problem ) ) {
      dists <- my.dist(x, keyVars, ind.problem[i])
      if ( length(which(dists==0)) >= k ) {
        break
      }
      
      minDist <- sort(unique(dists))[2]
      ind <- which(dists==minDist)
      if ( length(ind) > 0 ) {
        colInd <- NULL
        colIndsSorted <- keyVars[order(importanceI)]
        while( is.null(colInd) ) {
          for ( cc in colIndsSorted ) {
            z <- which(x[ind.problem[i],cc]!=x[ind,cc] & !is.na(x[ind,cc]))
            if ( length(z) > 0 ) {
              colInd <- cc
              break
            }							
          }
        }
        x[ind.problem[i],colInd] <- NA
      } else {
        stop("Error\n")
      }
    }
    ff <- freqCalc(x, keyVars=keyVars)
    rk <- indivRisk(ff)
    if ( all(ff$fk >= k) ) {
      runInd <- FALSE
    } 
  }
  ###
  if(nrow(erg)>0){
    xrem <- data.table(idvarextraforsls=x[,"idvarextraforsls"],weg=1)
    x <- data.table(x[,-ncol(x)])
  
    setkey(xrem,"idvarextraforsls")
    xKeys[,weg:=NULL]
    setkey(xKeys,"idvarextraforsls")
    xKeys <- merge(xKeys,xrem,all=TRUE)
    xKeys <- xKeys[is.na(weg),]
    xKeys[,weg:=NULL]
    x <- rbind(x,xKeys)
    setkey(x,"idvarextraforsls")
    x[,idvarextraforsls:=NULL]
    x <- data.frame(x)
    if(any(NABefore))
      x[NABefore] <- NA
  }
  ##
  ## preparing the output:
  totalNA <- length(which(is.na(x)))
  supps <- apply(x[, keyVars], 2, function(x) length(which(is.na(x)))) - NAinKey
  names(supps) <- colnames(x)[keyVars]
  
  res <- list(xAnon=x, supps=supps, totalSupps=totalNABefore-totalNA, 
      anonymity=TRUE, keyVars=keyVars, importance=importance, k=k)
  
  class(res) <- "localSuppression"
  invisible(res)	
}
