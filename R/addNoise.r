#' Adding noise to perturb data
#'
#' Various methods for adding noise to perturb continuous scaled variables.
#'
#' If \sQuote{obj} is of class \code{\link{sdcMicroObj-class}} all continuous key
#' variables are selected per default. If \sQuote{obj} is of class
#' \dQuote{data.frame} or \dQuote{matrix}, the continuous variables have to be
#' specified.
#'
#' Method \sQuote{additive} adds noise completely at random to each variable
#' depending on there size and standard deviation. \sQuote{correlated} and
#' method \sQuote{correlated2} adds noise and preserves the covariances as
#' descriped in R. Brand (2001) or in the reference given below.  Method
#' \sQuote{restr} takes the sample size into account when adding noise.  Method
#' \sQuote{ROMM} is an implementation of the algorithm ROMM (Random
#' Orthogonalized Matrix Masking) (Fienberg, 2004).  Method \sQuote{outdect}
#' adds noise only to outliers.  The outliers are idedentified with univariate
#' and robust multivariate procedures based on a robust mahalanobis distancs
#' calculated by the MCD estimator.
#'
#' @name addNoise
#' @aliases addNoise addNoise-methods addNoise,data.frame-method
#' addNoise,matrix-method addNoise,sdcMicroObj-method
#' @docType methods
#' @param obj either a data frame, matrix or a \code{\link{sdcMicroObj-class}} that should be perturbed
#' @param variables vector with names of variables that should be perturbed
#' @param noise amount of noise (in percentages)
#' @param method choose between \sQuote{additive}, \sQuote{correlated},
#' \sQuote{correlated2}, \sQuote{restr}, \sQuote{ROMM}, \sQuote{outdect}
#' @param ... see possible arguments below
#' @return If \sQuote{obj} was of class \code{\link{sdcMicroObj-class}} the corresponding
#' slots are filled, like manipNumVars, risk and utility.
#'
#' If \sQuote{obj} was of class \dQuote{data.frame} or \dQuote{matrix} an
#' object of class \dQuote{micro} with following entities is returned:
#' \item{x}{ the original data }
#' \item{xm}{ the modified (perturbed) data}
#' \item{method}{method used for perturbation}
#' \item{noise}{amount of noise}
#' @section Methods: \describe{
#' \item{list("signature(obj = \"data.frame\")")}{}
#' \item{list("signature(obj = \"matrix\")")}{}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @author Matthias Templ
#' @seealso \code{\link{sdcMicroObj-class}}, \code{\link{summary.micro}}
#' @references Domingo-Ferrer, J. and Sebe, F. and Castella, J., \dQuote{On the
#' security of noise addition for privacy in statistical databases}, Lecture
#' Notes in Computer Science, vol. 3050, pp. 149-161, 2004.  ISSN 0302-9743.
#' Vol. Privacy in Statistical Databases, eds. J. Domingo-Ferrer and V. Torra,
#' Berlin: Springer-Verlag.
#' \url{http://crises-deim.urv.cat/webCrises/publications/isijcr/lncs3050OntheSec.pdf},
#'
#' Ting, D. Fienberg, S.E. and Trottini, M. \dQuote{ROMM Methodology for
#' Microdata Release} Joint UNECE/Eurostat work session on statistical data
#' confidentiality, Geneva, Switzerland, 2005,
#' \url{http://www.unece.org/fileadmin/DAM/stats/documents/ece/ces/ge.46/2005/wp.11.e.pdf}
#'
#' Ting, D., Fienberg, S.E., Trottini, M.  \dQuote{Random orthogonal matrix
#' masking methodology for microdata release}, International Journal of
#' Information and Computer Security, vol. 2, pp. 86-105, 2008.
#'
#' Templ, M. and Meindl, B., \emph{Robustification of Microdata Masking Methods
#' and the Comparison with Existing Methods}, Lecture Notes in Computer
#' Science, Privacy in Statistical Databases, vol. 5262, pp. 177-189, 2008.
#'
#' Templ, M.  \emph{New Developments in Statistical Disclosure Control and
#' Imputation: Robust Statistics Applied to Official Statistics},
#' Suedwestdeutscher Verlag fuer Hochschulschriften, 2009, ISBN: 3838108280,
#' 264 pages.
#'
#' Templ, M. and Meindl, B. and Kowarik, A.: \emph{Statistical Disclosure Control for 
#' Micro-Data Using the R Package sdcMicro}, Journal of Statistical Software, 
#' 67 (4), 1--36, 2015.
#' @keywords manip
#' @export
#' @examples
#'
#' data(Tarragona)
#' a1 <- addNoise(Tarragona)
#' a1
#'
#' data(testdata)
#' testdata[, c('expend','income','savings')] <-
#' addNoise(testdata[,c('expend','income','savings')])$xm
#'
#' ## for objects of class sdcMicroObj:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- addNoise(sdc)
#'
setGeneric("addNoise", function(obj, variables = NULL, noise = 150, method = "additive", ...) {
  standardGeneric("addNoise")
})

setMethod(f = "addNoise", signature = c("sdcMicroObj"),
definition = function(obj, variables, noise = 150, method = "additive", ...) {
  x <- get.sdcMicroObj(obj, type = "manipNumVars")
  if (missing(variables)) {
    variables <- 1:ncol(x)
  }

  obj <- nextSdcObj(obj)
  if (length(variables) == 1) {
    x1tmp <- cbind(0, x[, variables])
    x[, variables] <- addNoiseWORK(x1tmp, noise = noise, method = method, ...)$xm[, 2, drop = FALSE]
  } else {
    x[, variables] <- addNoiseWORK(x = x[, variables], noise = noise, method = method, ...)$xm
  }
  manipData <- x
  colnames(manipData) <- colnames(x)
  obj <- set.sdcMicroObj(obj, type = "manipNumVars", input = list(as.data.frame(manipData)))

  obj <- dRisk(obj)
  obj <- dUtility(obj)
  obj
})

setMethod(f = "addNoise", signature = c("data.frame"),
definition = function(obj, variables, noise = 150, method = "additive", ...) {
  if (missing(variables)) {
    variables <- 1:ncol(obj)
  }
  addNoiseWORK(x = obj[, variables, drop = FALSE], noise = noise, method = method, ...)
})

setMethod(f = "addNoise", signature = c("matrix"),
definition = function(obj, variables, noise = 150, method = "additive", ...) {
  if (missing(variables)) {
    variables <- 1:ncol(obj)
  }
  addNoiseWORK(x = obj[, variables], noise = noise, method = method, ...)
})

addNoiseWORK <- function(x, noise = 150, method = "additive", p = 0.001, delta = 0.1) {
  N <- dim(x)[1]
  P <- dim(x)[2]
  y = x
  wnoise <- noise/100
  addN <- apply(x, 2, median) * wnoise
  eps <- matrix(0, ncol = P, nrow = N)
  if (method == "additive") {
    x <- apply(x, 2, function(x) {
      x + rnorm(N, 0, noise * sd(x, na.rm = TRUE))  #1.96 * sd(x)/sqrt(N) * wnoise)
    })
  } else if (method == "correlated") {
    if (dim(x)[2] < 2)
      stop("must have more than 2 variables")
    # x <- x + mvrnorm(N, colMeans(x,na.rm = TRUE), Sigma = noise/100 * cov(na.omit(x)))
    x <- x + mvrnorm(N, rep(0, ncol(x)), Sigma = noise/100 * cov(na.omit(x)))
  } else if (method == "correlated2") {
    d1 <- sqrt(1 - delta^2)
    x <- apply(x, 2, function(x) {
      x * d1 + delta * rnorm(N, mean = (1 - d1)/delta * mean(x, na.rm = TRUE), sd = sd(x,
        na.rm = TRUE))
    })
  } else if (method == "restr") {
    if (N < 500) {
      cc <- sqrt((N - 1 - wnoise)/((N + 1) * (1 + wnoise)))
    } else {
      cc <- sqrt((N - 1)/(N + N * wnoise - 1))
    }
    d <- (1 - cc) * colMeans(x, na.rm = TRUE)
    x <- cc * x + d
  } else if (method == "ROMM") {
    orthonormalization <- function(u = NULL, basis = TRUE, norm = TRUE) {
      if (is.null(u))
        return(NULL)
      if (!(is.matrix(u)))
        u <- as.matrix(u)
      p <- nrow(u)
      n <- ncol(u)
      if (prod(abs(La.svd(u)$d) > 1e-08) == 0)
        stop("colinears vectors")
      if (p < n) {
        warning("too much vectors to orthogonalize.")
        u <- as.matrix(u[, 1:p])
        n <- p
      }
      if (basis & (p > n)) {
        base <- diag(p)
        coef.proj <- crossprod(u, base)/diag(crossprod(u))
        base2 <- base - u %*% matrix(coef.proj, nrow = n, ncol = p)
        norm.base2 <- diag(crossprod(base2))
        base <- as.matrix(base[, order(norm.base2) > n])
        u <- cbind(u, base)
        n <- p
      }
      v <- u
      if (n > 1) {
        for (i in 2:n) {
          coef.proj <- c(crossprod(u[, i], v[, 1:(i - 1)]))/diag(crossprod(v[, 1:(i -
          1)]))
          v[, i] <- u[, i] - matrix(v[, 1:(i - 1)], nrow = p) %*% matrix(coef.proj,
          nrow = i - 1)
        }
      }
      if (norm) {
        coef.proj <- 1/sqrt(diag(crossprod(v)))
        v <- t(t(v) * coef.proj)
      }
      return(v)
    }

    # require(far) print('please load package far')
    ROMM <- function(x, p1 = p) {
      M <- matrix(rnorm(N * N), ncol = N, nrow = N)
      I <- diag(1, N)
      P <- I + p1 * M
      Torthon <- orthonormalization(P)
      x <- Torthon %*% as.matrix(x)
    }
    cn1 <- colnames(x)
    rn1 <- rownames(x)
    x <- ROMM(x, p)
    colnames(x) <- cn1
    rownames(x) <- rn1
  } else if (method == "outdect") {
    q1 <- apply(x, 2, quantile, probs = 0.99, na.rm = TRUE)
    r <- list()
    for (i in 1:ncol(x)) {
      r[[i]] <- which(x[, i] > q1[i])
    }
    univOutlier <- unlist(r)
    limit <- sqrt(qchisq(0.975, dim(x)[2]))
    xMcd <- covMcd(na.omit(x), alpha = 1/2)
    rd <- sqrt(mahalanobis(x, xMcd$center, xMcd$cov))
    rdOutlier <- which(rd > limit)
    outliers <- unique(sort(c(univOutlier, rdOutlier)))
    for (i in 1:P) {
      x[outliers, i] <- x[outliers, i] + rnorm(length(outliers), 0, 1.96 * sd(x[, i],
        na.rm = TRUE)/sqrt(N) * wnoise)
    }
  } else {
    stop("method must be one of the following:\n \"correlated\", \"outdect\", \"ROMM\", \"restr\", \"correlated2\", \"additive\"\n")
  }
  colnames(x) <- colnames(y)
  res <- list(x = y, xm = x, method = paste("addNoise:", method), noise = noise)
  class(res) <- "micro"
  res
}
