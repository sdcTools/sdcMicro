localSupp2Wrapper <- function(x, keyVars, w, importance=rep(1, length(keyVars)), method="minimizeSupp", kAnon=2) {

  .Deprecated("localSuppression")	
	
  tmp <- x
  w1 <- which( apply(tmp, 2, is.factor) )
  w2 <- which( apply(tmp, 2, is.character) )
  for( i in w1 ) tmp[,i] <- as.integer(as.factor(tmp[,i]))
  for( i in w2 ) tmp[,i] <- as.integer(as.factor(tmp[,i]))

  x <- as.matrix(x)

  
  
  
  indikator <- FALSE
  i <- 0
  totalNA <- length(which(is.na(tmp)))
  NAinKey <- apply(tmp[, keyVars],2, function(x) length(which(is.na(x))))
  supps.old <- 999999999999
  while(indikator==FALSE) {
    tmp <- localSupp2(tmp, keyVars, w, importance=importance, method="minimizeSupp", k=kAnon)$xAnon
    supps.new <- sum(is.na(tmp)) #as.vector(apply(tmp, 2, function(x) { length(which(is.na(x)))}))
    ff <- freqCalc(tmp, keyVars, w)
    if((ff$n1==0 & ff$n2==0) || supps.old == supps.new ){#(paste(supps.old, collapse="") == paste(supps.new, collapse=""))) {
      indikator <- TRUE
      supps <- sum(is.na(tmp)) #apply(tmp, 2, function(x) { length(which(is.na(x)))})
      #totalSupps <- sum(supps)
      #rk <- indivRisk(ff)
    }
    supps.old <- supps.new
    i <- i+1
  }
  if( all( ff$fk >= kAnon) ){
    cat("\n")
    print(paste(kAnon, "-anonymity after ", i, " iterations.", sep=""))
    cat("\n")
    anon <- TRUE
  } else{
    cat("\n")
    print(paste("It is not possible to achieve ", kAnon, "-anonymity. Allow suppressions in more than ",
         length(which(importance!=0)), " key variables", sep=""))
    cat("\n")
    anon <- FALSE
  }
  rk <- indivRisk(ff)
  supps <- apply(tmp[, keyVars],2, function(x) length(which(is.na(x)))) - NAinKey
  names(supps) <- colnames(tmp)[keyVars]
  x[is.na(tmp)] <- NA
  res <- list(xAnon=x, supps=supps, totalSupps=length(which(is.na(tmp))) - totalNA, it=i, anonymity=anon,
              keyVars=keyVars, importance=importance, k=kAnon )
  class(res) <- "localSupp2"
  invisible(res)
}