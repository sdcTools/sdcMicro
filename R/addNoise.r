#' Adding noise to perturb data
#'
#' Various methods for adding noise to perturb continuous scaled variables.
#'
#' If \sQuote{obj} is of class \code{\link{sdcMicroObj-class}}, all continuous key
#' variables are selected per default. If \sQuote{obj} is of class
#' \dQuote{data.frame} or \dQuote{matrix}, the continuous variables have to be
#' specified.
#'
#' Method \sQuote{additive} adds noise completely at random to each variable
#' depending on its size and standard deviation. \sQuote{correlated} and
#' method \sQuote{correlated2} adds noise and preserves the covariances as
#' described in R. Brand (2001) or in the reference given below. Method
#' \sQuote{restr} takes the sample size into account when adding noise. Method
#' \sQuote{outdect}
#' adds noise only to outliers. The outliers are identified with univariate
#' and robust multivariate procedures based on a robust mahalanobis distances
#' calculated by the MCD estimator.
#'
#' Method \sQuote{ROMM} is random orthogonal matrix masking (Ting, Fienberg and
#' Trottini, 2008). The perturbed data have exactly the same sample means and the
#' same sample covariance matrix as the original data. The implementation follows a
#' suggestion of Øyvind Langsrud (see function \code{RegSDCgen()} of his R package
#' \pkg{RegSDC}, \url{https://github.com/olangsrud/RegSDC}): the centred data are
#' decomposed as \eqn{QR} by a QR decomposition, normally distributed noise with
#' standard deviation \code{p} is added to the orthonormal matrix \eqn{Q}, the result
#' is centred and orthonormalized again and finally multiplied by \eqn{R}. The amount
#' of perturbation is set by argument \code{p} (default \code{0.001}); \code{noise} is
#' not used. With \code{p = 0} the data are not changed. For small \code{p} each value
#' is perturbed by noise with a standard deviation of roughly
#' \eqn{p \sqrt{n - 1}}{p * sqrt(n - 1)} times the standard deviation of its variable
#' (\eqn{n} the number of observations), for large \code{p} the perturbed values are
#' essentially unrelated to the original ones, apart from the preserved means and
#' covariances. Rows with missing values are not perturbed and are returned unchanged
#' (with a warning); the means and covariances are then preserved for the complete
#' rows. More complete rows than variables are required. Constant variables are
#' returned unchanged.
#'
#' @name addNoise
#' @param obj either a \code{data.frame} or a \code{\link{sdcMicroObj-class}} that should be perturbed
#' @param variables vector with names of variables that should be perturbed
#' @param noise amount of noise (in percentages)
#' @param method choose between \sQuote{additive}, \sQuote{correlated},
#' \sQuote{correlated2}, \sQuote{restr}, \sQuote{ROMM}, \sQuote{outdect}
#' @param ... further arguments: \code{p} (a non-negative number, default
#' \code{0.001}), the amount of perturbation for method \sQuote{ROMM}, and
#' \code{delta} (default \code{0.1}), the amount of noise for method
#' \sQuote{correlated2}. See Details.
#' @return If \sQuote{obj} was of class \code{\link{sdcMicroObj-class}} the corresponding
#' slots are filled, like manipNumVars, risk and utility.
#'
#' If \sQuote{obj} was of class \dQuote{data.frame} or \dQuote{matrix} an
#' object of class \dQuote{micro} with following entities is returned:
#' \item{x}{ the original data }
#' \item{xm}{ the modified (perturbed) data}
#' \item{method}{method used for perturbation}
#' \item{noise}{amount of noise}
#' @author Matthias Templ and Bernhard Meindl
#' @seealso \code{\link{sdcMicroObj-class}}, \code{\link{summary.micro}}
#' @references
#' Domingo-Ferrer, J. and Sebe, F. and Castella, J., \dQuote{On the
#' security of noise addition for privacy in statistical databases}, Lecture
#' Notes in Computer Science, vol. 3050, pp. 149-161, 2004.  ISSN 0302-9743.
#' Vol. Privacy in Statistical Databases, eds. J. Domingo-Ferrer and V. Torra,
#' Berlin: Springer-Verlag.
#'
#' Ting, D. Fienberg, S.E. and Trottini, M. \dQuote{ROMM Methodology for
#' Microdata Release} Joint UNECE/Eurostat work session on statistical data
#' confidentiality, Geneva, Switzerland, 2005,
#' \url{https://www.unece.org/fileadmin/DAM/stats/documents/ece/ces/ge.46/2005/wp.11.e.pdf}
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
#' 67 (4), 1--36, 2015. \doi{10.18637/jss.v067.i04}
#'
#' Templ, M. Statistical Disclosure Control for Microdata: Methods and Applications in R.
#' \emph{Springer International Publishing}, 287 pages, 2017. ISBN 978-3-319-50272-4.
#' \doi{10.1007/978-3-319-50272-4}
#' @keywords manip
#' @export
#' @rdname addNoise
#' @examples
#'
#' data(Tarragona)
#' \donttest{
#' a1 <- addNoise(Tarragona)
#' a1
#' }
#'
#' data(testdata)
#' \donttest{
#' # donttest because Examples with CPU time > 2.5 times elapsed time
#' testdata[, c('expend','income','savings')] <-
#' addNoise(testdata[,c('expend','income','savings')])$xm
#'
#' ## for objects of class sdcMicroObj:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- addNoise(sdc)
#' }

addNoise <- function(obj, variables=NULL, noise=150, method="additive", ...) {
  addNoiseX(obj=obj, variables=variables, noise=noise, method=method, ...)
}

setGeneric("addNoiseX", function(obj, variables=NULL, noise=150, method="additive", ...) {
  standardGeneric("addNoiseX")
})

setMethod(f="addNoiseX", signature=c("sdcMicroObj"),
definition=function(obj, variables, noise=150, method="additive", ...) {
  x <- get.sdcMicroObj(obj, type="manipNumVars")
  if (missing(variables) | is.null(variables)) {
    variables <- 1:ncol(x)
  }

  obj <- nextSdcObj(obj)
  if (length(variables) == 1) {
    x1tmp <- cbind(0, x[, variables])
    x[, variables] <- addNoiseWORK(x1tmp, noise=noise, method=method, ...)$xm[, 2, drop=TRUE]
  } else {
    x[, variables] <- addNoiseWORK(x=x[, variables], noise=noise, method=method, ...)$xm
  }
  manipData <- x
  colnames(manipData) <- colnames(x)
  obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(as.data.frame(manipData)))

  obj <- dRisk(obj)
  obj <- dUtility(obj)
  obj
})

setMethod(f="addNoiseX", signature=c("data.frame"),
definition=function(obj, variables, noise=150, method="additive", ...) {
  if (missing(variables)|is.null(variables)) {
    variables <- 1:ncol(obj)
  }
  addNoiseWORK(x=obj[, variables, drop=FALSE], noise=noise, method=method, ...)
})

addNoiseWORK <- function(x, noise=150, method="additive", p=0.001, delta=0.1) {
  addNoise_additive <- function(x, noise) {
    N <- nrow(x)
    x <- apply(x, 2, function(x) {
      x + stats::rnorm(N, 0, noise/100 * sd(x, na.rm=TRUE))  #1.96 * sd(x)/sqrt(N) * wnoise)
    })
    x
  }
  addNoise_additive0 <- function(x, noise) {
    N <- nrow(x)
    x <- apply(x, 2, function(x) {
      noise <- stats::rnorm(N, 0, noise/100 * sd(x, na.rm=TRUE))  #1.96 * sd(x)/sqrt(N) * wnoise)
      noise[x == 0] <- 0
      x + noise
    })
    x
  }
  addNoise_correlated <- function(x, noise) {
    if (ncol(x) < 2) {
      stop("'addNoise_correlated' works only for >= 2 variables!\n")
    }
    N <- nrow(x)
    x + mvrnorm(N, rep(0, ncol(x)), Sigma=noise/100 * cov(na.omit(x)))
    # better?
    #x + mvrnorm(N, rep(0, ncol(x)), Sigma=noise/100 * cov(x, use="pairwise.complete.obs"))
  }
  addNoise_correlated2 <- function(x, delta) {
    N <- nrow(x)
    d1 <- sqrt(1 - delta^2)
    x <- apply(x, 2, function(x) {
      x * d1 + delta * stats::rnorm(N, mean=(1 - d1)/delta * mean(x, na.rm=TRUE), sd=sd(x, na.rm=TRUE))
    })
    x
  }
  addNoise_restr <- function(x, noise) {
    N <- nrow(x)
    wnoise <- noise/100
    if (N < 500) {
      cc <- sqrt((N - 1 - wnoise)/((N + 1) * (1 + wnoise)))
    } else {
      cc <- sqrt((N - 1)/(N + N * wnoise - 1))
    }
    d <- (1 - cc) * colMeans(x, na.rm=TRUE)
    x <- cc * x + d
    x
  }
  # Random orthogonal matrix masking that preserves the sample means and the
  # sample covariance matrix exactly. QR formulation suggested by O. Langsrud
  # in issue #271 (cf. RegSDCgen() in his R package RegSDC); extended here by the
  # handling of missing values, rank-deficient data and the signs of the basis.
  addNoise_ROMM <- function(x, p) {
    if (!is.numeric(p) || length(p) != 1 || is.na(p) || p < 0) {
      stop("addNoise: for method 'ROMM', argument 'p' must be a single non-negative number.\n")
    }
    xm <- as.matrix(x)
    cc <- stats::complete.cases(xm)
    n <- sum(cc)
    k <- ncol(xm)
    if (n <= k) {
      stop("addNoise: method 'ROMM' needs more complete rows than variables (",
        n, " complete rows, ", k, " variables).\n")
    }
    if (n < nrow(xm)) {
      warning("addNoise (method 'ROMM'): ", nrow(xm) - n, " rows with missing values ",
        "were not perturbed and are returned unchanged.\n", call. = FALSE)
    }
    if (p == 0) {
      return(xm)
    }
    y <- xm[cc, , drop=FALSE]
    mu <- colMeans(y)
    # centred data = Q1 %*% R1; qr() moves (near-)collinear columns to the end,
    # so the columns of R1 have to be put back into the original order
    qr1 <- qr(sweep(y, 2, mu))
    R1 <- qr.R(qr1)[, order(qr1$pivot), drop=FALSE]
    # perturb the orthonormal basis, centre it and orthonormalize it again
    z <- qr.Q(qr1) + p * matrix(stats::rnorm(n * k), nrow=n, ncol=k)
    qr2 <- qr(sweep(z, 2, colMeans(z)))
    if (qr2$rank < k) {
      stop("addNoise: method 'ROMM' failed, the perturbed basis is rank deficient.\n")
    }
    # the QR decomposition is unique only up to the signs of the columns of Q;
    # with a positive diagonal of R the new basis tends to the old one for p -> 0
    Q2 <- sweep(qr.Q(qr2), 2, sign(diag(qr.R(qr2))), "*")
    xm[cc, ] <- sweep(Q2 %*% R1, 2, mu, "+")
    xm
  }
  addNoise_outdect <- function(x, noise) {
    wnoise <- noise/100
    N <- nrow(x)
    P <- ncol(x)
    q1 <- apply(x, 2, stats::quantile, probs=0.99, na.rm=TRUE)
    r <- list()
    for (i in 1:ncol(x)) {
      r[[i]] <- which(x[,i] > q1[i])
    }
    univOutlier <- unlist(r)
    limit <- sqrt(stats::qchisq(0.975, dim(x)[2]))
    xMcd <- covMcd(na.omit(x), alpha=1/2)
    rd <- sqrt(stats::mahalanobis(x, xMcd$center, xMcd$cov))
    rdOutlier <- which(rd > limit)
    outliers <- unique(sort(c(univOutlier, rdOutlier)))
    for (i in 1:P) {
      nn <- stats::rnorm(length(outliers), 0, 1.96 * sd(x[,i], na.rm=TRUE)/sqrt(N) * wnoise)
      x[outliers, i] <- x[outliers, i] + nn
    }
    x
  }

  if (!method %in% c("additive","additive0","correlated","correlated2","restr","ROMM","outdect")) {
    stop("addNoiseWORK: 'method' must be one of the following: 'additive', 'correlated', 'correlated2', 'restr', 'ROMM' or 'outdect'.\n")
  }
  if (method == "additive") {
    xm <- addNoise_additive(x=x, noise=noise)
  }
  if (method == "additive0") {
    xm <- addNoise_additive0(x=x, noise=noise)
  }
  if (method == "correlated") {
    xm <- addNoise_correlated(x=x, noise=noise)
  }
  if (method == "correlated2") {
    xm <- addNoise_correlated2(x=x, delta=delta)
  }
  if (method == "restr") {
    xm <- addNoise_restr(x=x, noise=noise)
  }
  if (method == "ROMM") {
    xm <- addNoise_ROMM(x=x, p=p)
  }
  if (method == "outdect") {
    xm <- addNoise_outdect(x=x, noise=noise)
    p <- NA
  }

  # create output
  # reset non-used parameters
  if (method!="ROMM") p <- NA
  if (method!="correlated2") delta <- NA
  if ( method %in%c("ROMM","correlated2")) noise <- NA
  colnames(xm) <- colnames(x)
  res <- list(x=x, xm=xm, method=paste("addNoise:", method), noise=noise, p=p, delta=delta)
  class(res) <- "addNoise"
  res
}
