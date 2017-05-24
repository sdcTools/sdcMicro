#' Fast generation of synthetic data
#'
#' Fast generation of (primitive) synthetic multivariate normal data.
#'
#' Uses the cholesky decomposition to generate synthetic data with approx. the
#' same means and covariances.  For details see at the reference.
#'
#' @name dataGen
#' @docType methods
#' @param obj an \code{\link{sdcMicroObj-class}}-object or a \code{data.frame}
#' @param ... see possible arguments below
#' \itemize{
#' \item{n}{amount of observations for the generated data}}
#' @return the generated synthetic data.
#' @note With this method only multivariate normal distributed data with
#' approxiomately the same covariance as the original data can be generated
#' without reflecting the distribution of real complex data, which are, in
#' general, not follows a multivariate normal distribution.
#' @author Matthias Templ
#' @seealso \code{\link{sdcMicroObj-class}}, \code{\link{shuffle}}
#' @references Have a look at
#' \url{https://web.archive.org/web/20110906030928/http://crises2-deim.urv.cat/docs/publications/lncs/443.pdf}
#' @keywords manip
#' @export
#' @examples
#' data(mtcars)
#' cov(mtcars[,4:6])
#' cov(dataGen(mtcars[,4:6]))
#' pairs(mtcars[,4:6])
#' pairs(dataGen(mtcars[,4:6]))
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- dataGen(sdc)
dataGen <- function(obj, ...) {
  dataGenX(obj=obj, ...)
}

setGeneric("dataGenX", function(obj, ...) {
  standardGeneric("dataGenX")
})

setMethod(f="dataGenX", signature=c("sdcMicroObj"),
definition=function(obj, ...) {
  x <- get.sdcMicroObj(obj, type = "manipNumVars")
  xn <- dataGenWORK(x = x, n = nrow(x))

  obj <- nextSdcObj(obj)
  obj <- set.sdcMicroObj(obj, type = "manipNumVars", input = list(as.data.frame(xn)))
  obj <- dRisk(obj)
  obj <- dRiskRMD(obj)
  obj <- dUtility(obj)
  obj
})

setMethod(f="dataGenX", signature=c("data.frame"), definition=function(obj, ...) {
  dataGenWORK(x = obj, ...)
})

dataGenWORK <- function(x, n = 200) {
  ## Generate a random $n^{'} \times m$ matrix $A$ in such way that the covariance matrix
  ## $\Sigma_aa = I$.
  M <- matrix(rnorm(n * ncol(x)), ncol = ncol(x))
  ## Use the Cholesky decomposition on C to obtain $C = U^t U$,
  ## where $U$ is an upper triangular matrix.
  C <- cov(x)
  chC <- chol(C)
  ## Obtain the synthetic data set $X^{'} = A U$
  Xn <- M %*% chC
  cm <- colMeans(x)
  Xn <- t(t(Xn) + cm)
  Xn
}
