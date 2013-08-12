setGeneric('dataGen', function(obj, ...) {standardGeneric('dataGen')})
setMethod(f='dataGen', signature=c('sdcMicroObj'),
    definition=function(obj, ...) { 
      x <- get.sdcMicroObj(obj, type="manipNumVars")
      
      xn <- dataGenWORK(x=x, n=nrow(x))
      
      obj <- nextSdcObj(obj)
      
      obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(as.data.frame(xn)))
      
      obj <- dRisk(obj)
      obj <- dRiskRMD(obj)
      obj <- dUtility(obj)
      
      obj
    })
setMethod(f='dataGen', signature=c("data.frame"),
    definition=function(obj, ...) { 
      dataGenWORK(x=obj,...)
    })
setMethod(f='dataGen', signature=c("matrix"),
    definition=function(obj, ...) { 
      dataGenWORK(x=obj,...)
    })


dataGenWORK <- function(x, n=200){
  
  ##Generate a random $n^{'} \times m$ matrix $A$ in such way that the covariance matrix $\Sigma_aa = I$.
  M <- matrix(rnorm(n*ncol(x)), ncol=ncol(x))
  ##Use the Cholesky decomposition on C to obtain $C = U^t U$, where $U$ is an upper triangular matrix.
  C <- cov(x)
  chC <- chol(C)
  ##Obtain the synthetic data set $X^{'} = A U$
  Xn <- M %*% chC
  cm <- colMeans(x)
  Xn <- t(t(Xn) +  cm)
  Xn
  
}
