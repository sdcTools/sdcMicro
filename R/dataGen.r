#' Fast generation of synthetic data
#'
#' Fast generation of (primitive) synthetic multivariate normal data.
#'
#' Uses the cholesky decomposition to generate synthetic data with approx. the
#' same means and covariances. For details see at the reference.
#'
#' @name dataGen
#' @docType methods
#' @param obj an \code{\link{sdcMicroObj-class}}-object or a \code{data.frame}
#' @param ... see possible arguments below
#' \describe{
#' \item{n:}{ amount of observations for the generated data, defaults to 200}
#' \item{use:}{ howto compute covariances in case of missing values, see also argument \code{use} in \code{\link{cov}}.
#' The default choice is 'everything', other possible choices are 'all.obs', 'complete.obs', 'na.or.complete' or 'pairwise.complete.obs'.}}
#' @return the generated synthetic data.
#' @note With this method only multivariate normal distributed data with
#' approxiomately the same covariance as the original data can be generated
#' without reflecting the distribution of real complex data, which are, in
#' general, not follows a multivariate normal distribution.
#' @author Matthias Templ
#' @seealso \code{\link{sdcMicroObj-class}}, \code{\link{shuffle}}
#' @references Mateo-Sanz, Martinez-Balleste, Domingo-Ferrer. Fast Generation of Accurate Synthetic Microdata. 
#' International Workshop on Privacy in Statistical Databases PSD 2004: Privacy in Statistical Databases, pp 298-306.
#' @export
#' @examples
#' data(mtcars)
#' \donttest{
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
#' }
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

dataGenWORK <- function(x, n=200, use="everything") {
  stopifnot(use %in% c("everything", "all.obs", "complete.obs", "na.or.complete", "pairwise.complete.obs"))

  ## Generate a random $n^{'} \times m$ matrix $A$ in such way that the covariance matrix
  ## $\Sigma_aa = I$.
  M <- matrix(stats::rnorm(n * ncol(x)), ncol = ncol(x))
  ## Use the Cholesky decomposition on C to obtain $C = U^t U$,
  ## where $U$ is an upper triangular matrix.
  C <- cov(x, use=use)
  chC <- chol(C)
  ## Obtain the synthetic data set $X^{'} = A U$
  Xn <- M %*% chC
  cm <- colMeans(x, na.rm=TRUE)
  Xn <- t(t(Xn) + cm)
  Xn
}
