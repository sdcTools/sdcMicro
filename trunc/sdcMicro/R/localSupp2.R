localSupp2 <- function(x, keyVars, w, importance=rep(1, length(keyVars)), method="minimizeSupp", k=1){
  ## x ... matrix, no factors
  ## keyVar ... key variables
  ## w ... sampling weights
  ## method ... "minimizeSupp", "minimizeRisk"
  ## k ... k-anonymity
  ## importance ... weights for key variables
  
  .Deprecated("localSuppression")
  

  y <- x
  
  w1 <- which( apply(x, 2, is.factor) )
  w2 <- which( apply(x, 2, is.character) )
  for( i in w1 ) x[,i] <- as.integer(as.factor(x[,i]))
  for( i in w2 ) x[,i] <- as.integer(as.factor(x[,i]))   
  
  x <- as.matrix(x)
  
  ind <- which(importance == 0)
  if( length(ind) > 0 ){ 
    realVars <- keyVars[-ind]
    importance <- importance[-ind]
  } else realVars = keyVars
  
  importanceI <- rep(0,ncol(x))
  importanceI[realVars] <- importance
  wi <- which(importanceI > 0)
  importanceI[wi] <- 1/importanceI[wi]
   

  #importance <- 1/importance ## da nach which.min gesucht wird.
  
  #if( length(keyVarsNotSupp) > 0 ){
  #  realVars <-  keyVar[-which(keyVar %in% keyVarsNotSupp)]
  #}

  ### maxiter Abschaetzung und Zeit
  ff = freqCalc(x, keyVars=keyVars, w=w)
  maxiter <- sum(sapply(1:k, function(x) length(which(ff$fk == x))))
  iter = 1
  m=1
  zeit1 <- function(x){
    i = which(ff$fk==1)[1]
    ffs <- rep(NA, length(realVars))
    ## innere Schleife zeilenweise:
    for(j in realVars){
      actualObs <- x[i, j]
      x[i, j] <- NA
      ffs[j] <- length(which(freqCalc(x, keyVars=keyVars, w=w)$fk == m))*importanceI[j]
      x[i, j] <- actualObs
      ffs[which(is.na(x[i,]))]  <- NA
    }
    x[i, which.min(ffs)] <- NA
    ff <- freqCalc(x, keyVars=keyVars, w=w)
    iter <- iter + 1
  }
  zeit2 <- function(x){
    i = which(ff$fk==1)[1]
    ffs <- rep(NA, length(realVars))
    ## innere Schleife zeilenweise:
    for(j in realVars){
      actualObs <- x[i, j]
      ffs[which(is.na(x[i,]))]  <- NA
      x[i, j] <- NA
      ffs[j] <- length(which(freqCalc(x, keyVars=keyVars, w=w)$fk == m))*importanceI[j]
      x[i, j] <- actualObs
    }
    x[i, which.min(ffs)] <- NA
    iter <- iter + 1
  }
  s1 <- system.time(zeit1(x))[1]
  print(paste("estimated time for method minimizeSupp:", round(maxiter * s1), "(in seconds)")) 
  print(paste("estimated time for method fastSupp:", round(maxiter * system.time(zeit2(x))[1]), "(in seconds)")) 
         
    
  iter <- 1
  if( s1 > 15 ){
  cat("\n(estimated computing time in percent:)\n")
  cat("************************************************************ 100 % \n")
  cat("(progress:)\n")
  flush.console()
  }
  
  ## integers fuer schnellere Rechenzeit:
  x <- apply(x, 2, function(x) as.integer(as.factor(x)))
  
  ### method: minimizeSupp 
  if( method == "minimizeSupp" ){
  ## ganz auessere Schleife mit k:
  for(m in 1:k){
  ff <- freqCalc(x, keyVars=keyVars, w=w)
  ## aeussere Schleife:
  for(i in which(ff$fk==m)){
    ffs <- rep(NA, length(realVars))
    ## innere Schleife zeilenweise:
    for(j in realVars){
      actualObs <- x[i, j]
      x[i, j] <- NA
      ffs[j] <- length(which(freqCalc(x, keyVars=keyVars, w=w)$fk == m))*importanceI[j]
      x[i, j] <- actualObs
      ffs[which(is.na(x[i,]))]  <- NA
    }
    x[i, which.min(ffs)] <- NA
    ff <- freqCalc(x, keyVars=keyVars, w=w)
    ## status algorithmus:
    if( iter %in% floor(seq(maxiter/60, maxiter, maxiter/60))){ 
      cat("*")
      flush.console()
    } 
    iter <- iter + 1
  }
  }
  }
  
  ### method: fastSupp
  if( method == "fastSupp"){
  ## ganz auessere Schleife mit k:
  for(m in 1:k){  
    for(i in which(ff$fk==m)){  
      ffs <- rep(NA, length(realVars))    
      ## innere Schleife zeilenweise:
      for(j in realVars){    
        actualObs <- x[i, j]
        x[i, j] <- NA
        ffs[j] <- length(which(freqCalc(x, keyVars=keyVars, w=w)$fk == m))*importanceI[j]
        x[i, j] <- actualObs
        ffs[which(is.na(x[i,]))]  <- NA
      } 
      x[i, which.min(ffs)] <- NA 
      ## status algorithmus:     
      if( iter %in% floor(seq(maxiter/60, maxiter, maxiter/60))){ 
        cat("*")
        flush.console()
      } 
      iter <- iter + 1      
    }
  }
  }
         
  cat("\n")
 if( all( ff$fk >= k) ){
    cat("\n")
    print(paste(k, "-anonymity has been reached", sep=""))
    cat("\n")
    anon <- TRUE
  } else{
    cat("\n")
    print(paste(k, "-anonymity is not achieved. Re-apply localSupp2 on this result or use function localSupp2Wrapper ", sep=""))
    cat("\n")
    anon <- FALSE
  }

  ## preparing the output:
  totalNA <- length(which(is.na(y)))
  NAinKey <- apply(y[, keyVars], 2, function(x) length(which(is.na(x))))
  supps <- apply(x[, keyVars], 2, function(x) length(which(is.na(x)))) - NAinKey
  names(supps) <- colnames(y)[keyVars]
  ## missings von x in y einsetzen:
    y[is.na(x)] <- NA
    
  
  res <- list(xAnon=y, supps=supps, totalSupps=length(which(is.na(x))) - totalNA, 
              anonymity=anon, keyVars=keyVars, importance=importance, k=k)
  
  class(res) <- "localSupp2"
  invisible(res)
}

