`indivRisk` <-
function(x, method="approx", qual=1, survey=TRUE){
  ## x ... object from freqCalc
  if(length(colnames(x$freqCalc)) > 0) knames <- colnames(x$freqCalc)[x$keyVars] else knames <- NULL
  if( survey == TRUE ){
  P <- ncol(x$freqCalc)
  N <- dim(x$freqCalc)[1]
  fk <- x$fk
  Fk <- x$Fk
  pk <- fk/Fk
  #pk = pk-0.0001
  rk <- rep(0, N)
  if( method == "exact" ){
    A <- (pk^(1-fk) - 1)/(fk-1)
    B <- function(fk,pk,i){
      (fk-1-i)^2 / ((i+1)*(fk-2-i)) * (pk^(i+2-fk)-1)/(pk^(i+1-fk)-1)
    }
    BB <- function(fk, pk){
      bb <- 0
      for(m in 0:(fk-3)) {
		b <- 1
        for(m2 in 0:m){
          b <- b * B(fk, pk, m2)
        }
        bb <- bb+(-1)^(m+1)*b
      }
      bb
    }
    #r <- (pk/(1-pk)) * (A * (1 + (-1)^1 * B(0) ))
    eins <- (pk/(1-pk))^fk
    drei <- (-1)^fk * log(pk)
    rk <- rep(0, N)
    for(k in 1:N){
      if( fk[k] > 2 ){
        rk[k] <- eins[k] * ((A[k] * (1 + BB(fk[k], pk[k]) )) + drei[k])
      }
      if( fk[k] == 2 ){
        rk[k] <- (pk[k]/(1-pk[k])) - (((pk[k]/(1-pk[k]))^2) * log(1/pk[k]))
      }
      if( fk[k] == 1 ){
        rk[k] <- (pk[k]/(1-pk[k])) * log(1/pk[k])
      }
    }
  }
  if( method == "approx" ){
    rk <- rep(0, N)
    for(k in 1:N){
      if( fk[k] > 2 ){
        rk[k] <- pk[k] / (fk[k] - (1-pk[k]))
      }
      if( fk[k] == 2 ){
        rk[k] <- (pk[k]/(1-pk[k])) - (((pk[k]/(1-pk[k]))^2) * log(1/pk[k]))
      }
      if( fk[k] == 1 ){
        rk[k] <- (pk[k]/(1-pk[k])) * log(1/pk[k])
      }
    }    
  }
  rk <- rk * qual
  rk <- list(rk=rk, method=method, qual=qual, fk=x$fk, knames=knames)
  }
  if(survey == FALSE){
    rk <- list(rk=1/x$fk, method=NA, qual=NA, fk=x$fk, knames=knames)
  }
  class(rk) <- "indivRisk"
  invisible(rk)
}

