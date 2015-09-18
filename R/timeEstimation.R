# TODO: Better estimate for the computation in a computer??  Author: Alexander Kowarik n:
# nrow dataset nkey: number of key variables nmean: mean number of categories
predictTime <- function(n, nkey, nmean) {
  coef() * (5.297e-12 * n^2 + 1.178e-06 * n * nkey + -2.973e-07 * n * nmean)  #FreqCalc
}

coefTime <- function() {
  ## very very very simple coefficent for computation time in comparison to my computer
  t <- Sys.time()
  a <- rnorm(5e+05)
  rm(a)
  max(0.1, as.numeric(Sys.time() - t)/0.06)
}

# genDat <- function(n=10,nkey=2,nmean=4){ nmeans <-
# round(abs(rnorm(nkey-1,mean=nmean,sd=sqrt(nmean/4)))) nmeans <-
# c(nmeans,nmean*nkey-sum(nmeans)) cols <- list() for(i in 1:nkey){ cols[[i]] <-
# sample(1:nmeans[i],n,rep=TRUE) } d <- data.frame(do.call(cbind,cols)) colnames(d) <-
# paste('key',1:nkey,sep='') return(d) } Code to estimate the coefficents coef() setwd()
# require(sdcMicro) timeFreq <- function(n=10,nkey=2,nmean=4,REP=3){ dat <-
# genDat(n=n,nkey=nkey,nmean=nmean) t <- vector() for(i in 1:REP){ tt <- Sys.time() f <-
# freqCalc(dat,keyVars=1:nkey) t <- c(t,as.numeric(Sys.time()-tt)) } mean(t) }
# timeFreq(n=8e6,nkey=6,nmean=5,REP=3) predictTime(n=8e6,nkey=6,nmean=5) REP <- 5 ns <-
# c(1e2,1e3,5e3,1e4,3e4,1e5,4e5,1e6) nkeys <- c(3,5,7,10) nmeans <- c(2,4,6,20) simgrid <-
# expand.grid(n=ns,nkey=nkeys,nmean=nmeans) ergsim <-
# apply(simgrid,1,function(x)timeFreq(n=x[1],nkey=x[2],nmean=x[3],REP=REP)) ergsim1 <-
# cbind(ergsim,simgrid) mod <- lm(ergsim~0+I(n^2)+n:nkey+n:nmean,data=ergsim1) summary(mod)
