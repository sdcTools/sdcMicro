#' Microaggregation
#'
#' Function to perform various methods of microaggregation.
#'
#' On \url{http://neon.vb.cbs.nl/casc/Glossary.htm} one can found the
#' \dQuote{official} definition of microaggregation:
#'
#' Records are grouped based on a proximity measure of variables of interest,
#' and the same small groups of records are used in calculating aggregates for
#' those variables. The aggregates are released instead of the individual
#' record values.
#'
#' The recommended method is \dQuote{rmd} which forms the proximity using
#' multivariate distances based on robust methods. It is an extension of the
#' well-known method \dQuote{mdav}.  However, when computational speed is
#' important, method \dQuote{mdav} is the preferable choice.
#'
#' While for the proximity measure very different concepts can be used, the
#' aggregation itself is naturally done with the arithmetic mean.
#' Nevertheless, other measures of location can be used for aggregation,
#' especially when the group size for aggregation has been taken higher than 3.
#' Since the median seems to be unsuitable for microaggregation because of
#' being highly robust, other mesures which are included can be chosen. If a
#' complex sample survey is microaggregated, the corresponding sampling weights
#' should be determined to either aggregate the values by the weighted
#' arithmetic mean or the weighted median.
#'
#' This function contains also a method with which the data can be clustered
#' with a variety of different clustering algorithms. Clustering observations
#' before applying microaggregation might be useful.  Note, that the data are
#' automatically standardised before clustering.
#'
#' The usage of clustering method \sQuote{Mclust} requires package mclust02,
#' which must be loaded first. The package is not loaded automatically, since
#' the package is not under GPL but comes with a different licence.
#'
#' The are also some projection methods for microaggregation included.  The
#' robust version \sQuote{pppca} or \sQuote{clustpppca} (clustering at first)
#' are fast implementations and provide almost everytime the best results.
#'
#' Univariate statistics are preserved best with the individual ranking method
#' (we called them \sQuote{onedims}, however, often this method is named
#' \sQuote{individual ranking}), but multivariate statistics are strong
#' affected.
#'
#' With method \sQuote{simple} one can apply microaggregation directly on the
#' (unsorted) data. It is useful for the comparison with other methods as a
#' benchmark, i.e. replies the question how much better is a sorting of the
#' data before aggregation.
#'
#' @name microaggregation
#' @aliases microaggregation-methods microaggregation,ANY-method
#' microaggregation,data.frame-method microaggregation,matrix-method
#' microaggregation,sdcMicroObj-method microaggregation
#' @docType methods
#' @param obj either an object of class sdcMicroObj or a data frame or matrix
#' @param variables variables to microaggregate. For NULL:If obj is of class
#' sdcMicroObj the categorical key variables are chosen per default. For
#' data.frames and matrices all columns are chosen per default.
#' @param aggr aggregation level (default=3)
#' @param strata_variables by-variables for applying microaggregation only
#' within strata defined by the variables
#' @param method pca, rmd, onedims, single, simple, clustpca, pppca,
#' clustpppca, mdav, clustmcdpca, influence, mcdpca
#' @param nc number of cluster, if the chosen method performs cluster analysis
#' @param weights sampling weights. If obj is of class sdcMicroObj the vector
#' of sampling weights is chosen automatically. If determined, a weighted
#' version of the aggregation measure is chosen automatically, e.g. weighted
#' median or weighted mean.
#' @param clustermethod clustermethod, if necessary
#' @param opt experimental
#' @param measure aggregation statistic, mean, median, trim, onestep (default=mean)
#' @param trim trimming percentage, if measure=trim
#' @param varsort variable for sorting, if method= single
#' @param transf transformation for data x
#' @return If \sQuote{obj} was of class \code{\link{sdcMicroObj-class}} the corresponding
#' slots are filled, like manipNumVars, risk and utility.  If \sQuote{obj} was
#' of class \dQuote{data.frame} or \dQuote{matrix} an object of class
#' \dQuote{micro} with following entities is returned:
#' \itemize{
#' \item{mx}{the aggregated data}
#' \item{x}{original data}
#' \item{method}{method}
#' \item{aggr}{aggregation level}
#' \item{measure}{ proximity measure for aggregation}
#' \item{fot}{ correction factor, necessary if totals calculated and n divided by aggr
#' is not an integer.}}
#' @section Methods: \describe{
#' \item{list("signature(obj = \"ANY\")")}{}
#' \item{list("signature(obj = \"data.frame\")")}{}
#' \item{list("signature(obj = \"matrix\")")}{}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @author Matthias Templ
#'
#' For method \dQuote{mdav}: This work is being supported by the International
#' Household Survey Network (IHSN) and funded by a DGF Grant provided by the
#' World Bank to the PARIS21 Secretariat at the Organisation for Economic
#' Co-operation and Development (OECD).  This work builds on previous work
#' which is elsewhere acknowledged.
#'
#' Author for the integration of the code for mdav in R: Alexander Kowarik.
#' @seealso \code{\link{summary.micro}}, \code{\link{plotMicro}},
#' \code{\link{valTable}}
#' @references
#' \url{http://www.springerlink.com/content/v257655u88w2/?sortorder=asc&p\_o=20}
#'
#' Templ, M. and Meindl, B., \emph{Robust Statistics Meets SDC: New Disclosure
#' Risk Measures for Continuous Microdata Masking}, Lecture Notes in Computer
#' Science, Privacy in Statistical Databases, vol. 5262, pp. 113-126, 2008.
#'
#' Templ, M.  \emph{Statistical Disclosure Control for Microdata Using the
#' R-Package sdcMicro}, Transactions on Data Privacy, vol. 1, number 2, pp.
#' 67-85, 2008.  \url{http://www.tdp.cat/issues/abs.a004a08.php}
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
#' m1 <- microaggregation(Tarragona, method="onedims", aggr=3)
#' ## summary(m1)
#' data(testdata)
#' m2 <- microaggregation(testdata[1:100,c("expend","income","savings")],
#'   method="mdav", aggr=4)
#' summary(m2)
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- microaggregation(sdc)
#'
setGeneric("microaggregation", function(obj, variables = NULL, aggr = 3, strata_variables = NULL,
  method = "mdav", weights = NULL, nc = 8, clustermethod = "clara", opt = FALSE, measure = "mean",
  trim = 0, varsort = 1, transf = "log") {
  standardGeneric("microaggregation")
})

setMethod(f = "microaggregation", signature = c("sdcMicroObj"),
definition = function(obj, variables = NULL, aggr = 3, strata_variables = NULL, method = "mdav",
  weights = NULL, nc = 8, clustermethod = "clara", opt = FALSE, measure = "mean", trim = 0,
  varsort = 1, transf = "log") {

  x <- get.sdcMicroObj(obj, type = "manipNumVars")
  if (is.null(variables)) {
    variables <- colnames(x)
  }
  strataVars <- get.sdcMicroObj(obj, type = "strataVar")

  if ( !is.null(strata_variables) ) {
    # case 1: character vector
    if ( is.character(strata_variables) ) {
      if ( any(strata_variables %in% variables) ) {
        err <- "Some variables that should be used for stratification of the dataset should also be microaggregated."
        err <- paste0(err,"\nThis is not possible. Please check your inputs!\n")
        stop(err)
      }
      sx <- get.sdcMicroObj(obj, type = "origData")[, strata_variables, drop=FALSE]
      x <- cbind(x, strata=apply(sx, 1, paste0, collapse="_"))
    }
    if ( class(strata_variables) %in% c("integer","numeric","factor") ) {
      sx <- data.table(strata=strata_variables)
      if ( nrow(sx) != nrow(x) ) {
        stop("Dimension of 'strata_variables' does not match with dimension of dataset!\n")
      }
      x <- cbind(x, strata=sx$strat)
    }
    strataVars <- "strata"
  } else if (length(strataVars) > 0) {
    sx <- get.sdcMicroObj(obj, type = "origData")[, strataVars, drop = FALSE]
    x <- cbind(x, sx)
    strataVars <- tail(colnames(x),1)
  }

  if (!is.null(weights) && !is.null(get.sdcMicroObj(obj, type = "weightVar"))) {
    weights <- get.sdcMicroObj(obj, type = "origData")[, get.sdcMicroObj(obj, type = "weightVar")]
  }

  res <- microaggregationWORK(x, variables = variables, aggr = aggr, strata_variables = strataVars,
    method = method, weights = weights, nc = nc, clustermethod = clustermethod, opt = opt,
    measure = measure, trim = trim, varsort = varsort, transf = transf)
  obj <- nextSdcObj(obj)
  x[, variables] <- res$mx[, variables]

  obj <- set.sdcMicroObj(obj, type = "manipNumVars", input = list(as.data.frame(x[, colnames(obj@origData)[obj@numVars], drop = FALSE])))
  obj <- dRisk(obj)
  obj <- dUtility(obj)
  obj
})

setMethod(f = "microaggregation", signature = c("data.frame"),
definition = function(obj, variables = NULL, aggr = 3, strata_variables = NULL, method = "mdav",
  weights = NULL, nc = 8, clustermethod = "clara", opt = FALSE, measure = "mean", trim = 0,
  varsort = 1, transf = "log") {

  if (is.null(variables))
    variables <- colnames(obj)
  microaggregationWORK(x = obj, variables = variables, aggr = aggr, strata_variables = strata_variables,
    method = method, weights = weights, nc = nc, clustermethod = clustermethod, opt = opt,
    measure = measure, trim = trim, varsort = varsort, transf = transf)
})

setMethod(f = "microaggregation", signature = c("matrix"),
definition = function(obj, variables = NULL, aggr = 3, strata_variables = NULL, method = "mdav",
  weights = NULL, nc = 8, clustermethod = "clara", opt = FALSE, measure = "mean", trim = 0,
  varsort = 1, transf = "log") {

  if (is.null(variables))
    variables <- colnames(obj)
  microaggregationWORK(x = obj, variables = variables, aggr = aggr, strata_variables = strata_variables,
    method = method, weights = weights, nc = nc, clustermethod = clustermethod, opt = opt,
    measure = measure, trim = trim, varsort = varsort, transf = transf)
})

microaggregationWORK <- function(x, variables = colnames(x), method = "mdav", aggr = 3,
  weights = NULL, nc = 8, clustermethod = "clara", opt = FALSE, measure = "mean", trim = 0,
  varsort = 1, transf = "log", strata_variables = NULL) {
  factorOfTotals <- function(x, aggr) {
    n <- dim(x)[1]
    abgerundet <- floor(n/aggr)
    fot <- n/abgerundet
    return(fot)
  }
  if (length(variables) == 1) {
    res <- list()
    res$mx <- mafast(x, variables = variables, by = strata_variables, aggr = aggr, measure = eval(parse(text = measure)))
    res$x <- x
    res$method <- "mafast"
    res$aggr <- aggr
    res$measure <- measure
    res$fot <- factorOfTotals(x, aggr)
    class(res) <- "micro"
    return(res)
  }
  blow = TRUE
  weightedQuantile <- function(x, weights = NULL, probs = seq(0, 1, 0.25), sorted = FALSE,
    na.rm = FALSE) {
    if (!is.numeric(x))
      stop("'x' must be a numeric vector")
    n <- length(x)
    if (n == 0 || (!isTRUE(na.rm) && any(is.na(x)))) {
      return(rep.int(NA, length(probs)))
    }
    if (!is.null(weights)) {
      if (!is.numeric(weights))
        stop("'weights' must be a numeric vector") else if (length(weights) != n) {
        stop("'weights' must have the same length as 'x'")
      } else if (!all(is.finite(weights)))
        stop("missing or infinite weights")
      if (any(weights < 0))
        warning("negative weights")
      if (!is.numeric(probs) || all(is.na(probs)) || isTRUE(any(probs < 0 | probs > 1))) {
        stop("'probs' must be a numeric vector with values in [0,1]")
      }
      if (all(weights == 0)) {
        warning("all weights equal to zero")
        return(rep.int(0, length(probs)))
      }
    }
    if (isTRUE(na.rm)) {
      indices <- !is.na(x)
      x <- x[indices]
      if (!is.null(weights))
        weights <- weights[indices]
    }
    if (!isTRUE(sorted)) {
      order <- order(x)
      x <- x[order]
      weights <- weights[order]
    }
    if (is.null(weights))
      rw <- (1:n)/n else rw <- cumsum(weights)/sum(weights)
    q <- sapply(probs, function(p) {
      if (p == 0)
        return(x[1]) else if (p == 1)
        return(x[n])
      select <- min(which(rw >= p))
      if (rw[select] == p)
        mean(x[select:(select + 1)]) else x[select]
    })
    return(unname(q))
  }
  weightedMedian <- function(x, weights = NULL, sorted = FALSE, na.rm = FALSE) {
    weightedQuantile(x, weights, probs = 0.5, sorted = sorted, na.rm = na.rm)
  }

  rownames(x) <- 1:nrow(x)
  stopifnot(method %in% c("simple", "single", "onedims", "pca", "mcdpca", "pppca", "clustmcdpca",
    "clustpppca", "clustpca", "rmd", "mdav", "influence", "mdavold"))
  # if (method == 'rmd') { blow = FALSE warning('object$blow have been set to TRUE and \n
  # object$xm == object$blowxm \n--------\n') }
  indexMicro <- function(x, aggr) {
    n <- dim(x)[1]
    if (n < 2 * aggr) {
      stop(paste("Too less observations (", n, ") for aggregate = ", aggr, sep = ""))
    }
    aa <- seq(1, n, aggr)
    j <- 1
    teiler <- n/aggr
    d1 <- 1:n
    index <- list()
    if (teiler %in% 1:n) {
      for (i in 1:length(aa)) {
        index[[i]] <- d1[j:(j + aggr - 1)]
        j <- j + aggr
      }
    } else {
      for (i in 1:(length(aa) - 2)) {
        index[[i]] <- d1[j:(j + aggr - 1)]
        j <- j + aggr
      }
      index[[i + 1]] <- d1[(j):n]
    }
    index
  }
  means <- function(x, index, measure, trim = 0) {
    m <- matrix(ncol = ncol(x), nrow = length(index))
    if (measure == "mean" & is.null(weights)) {
      for (i in 1:length(index)) {
        m[i, ] <- colMeans(x[index[[i]], ])
      }
    }
    if (measure == "median" & is.null(weights)) {
      for (i in 1:length(index)) {
        m[i, ] <- apply(x[index[[i]], ], 2, median)
      }
    }
    if (measure == "mean" & !is.null(weights)) {
      for (i in 1:length(index)) {
        m[i, ] <- apply(x[index[[i]], ], 2, function(x) weighted.mean(x, w = weights[index[[i]]]))
      }
    }
    if (measure == "median" & !is.null(weights)) {
      for (i in 1:length(index)) {
        m[i, ] <- apply(x[index[[i]], ], 2, function(x) weightedMedian(x, weights = weights[index[[i]]]))
      }
    }
    if (measure == "trim") {
      for (i in 1:length(index)) {
        for (j in 1:length(index[[i]])) {
          m[i, ] <- apply(x[index[[i]], ], 2, mean, trim = trim)
        }
      }
    }
    if (measure == "onestep") {
      y <- x
      constant <- 3/1.486
      for (i in 1:length(index)) {
        m1 <- apply(x[index[[i]], ], 2, median)
        m2 <- apply(x[index[[i]], ], 2, mad)
        limit1 <- m1 + constant * m2
        limit2 <- m1 - constant * m2
        for (ii in 1:length(index[[i]])) {
          if (any(x[index[[i]][ii], ] > limit1)) {
          w <- which(x[index[[i]][ii], ] > limit1)
          le <- length(w)
          y[index[[i]][ii], w] <- limit1[w]
          }
          if (any(x[index[[i]][ii], ] < limit2)) {
          w <- which(x[index[[i]][ii], ] < limit2)
          le <- length(w)
          y[index[[i]][ii], w] <- limit2[w]
          }
          m[i, ] <- colMeans(y[index[[i]], ])
        }
      }
    }
    colnames(m) <- colnames(x)
    return(m)
  }
  blowup <- function(x, mr, aggr) {
    n <- dim(x)[1]
    aa <- seq(1, n, aggr)
    j <- 1
    teiler <- n/aggr
    d1 <- 1:n
    xx <- matrix(0, ncol = ncol(x), nrow = nrow(x))
    if (teiler %in% 1:n) {
      for (i in 1:length(aa)) {
        for (s in j:(j + aggr - 1)) {
          xx[s, ] <- as.matrix(mr[i, , drop = FALSE])
        }
        j <- j + aggr
      }
    } else {
      for (i in 1:(length(aa) - 2)) {
        for (s in j:(j + aggr - 1)) {
          xx[s, ] <- as.matrix(mr[i, , drop = FALSE])
        }
        j <- j + aggr
      }
      for (s in j:n) {
        xx[s, ] <- mr[i + 1, ]
      }
    }
    rownames(xx) <- rownames(x)
    xx
  }
  clust <- function(x, nc, clustermethod = "clara", opt = FALSE, transf = "log") {
    if (transf == "none") {
      y <- x
    }
    if (transf == "log") {
      y <- scale(log(x))
    }
    if (transf == "boxcox") {
      lambda <- powerTransform(x)$lambda
      y <- scale(bcPower(x, lambda))
    }
    if (clustermethod == "clara") {
      a <- clara(x, nc)
      clustresult <- a$clust
      centers <- a$med
      size <- a$clusinfo[, 1]
    }
    if (clustermethod == "pam") {
      a <- pam(x, nc)
      clustresult <- a$clust
      centers <- a$med
      size <- a$clusinfo[, 1]
    }
    if (clustermethod == "kmeans") {
      a <- kmeans(x, nc)
      centers <- a$centers
      clustresult <- a$cluster
      size <- a$size
    }
    if (clustermethod == "cmeans") {
      a <- cmeans(x, nc)
      centers <- a$centers
      clustresult <- a$cluster
      size <- a$size
      res@mem <- a$mem
    }
    if (clustermethod == "bclust") {
      a <- bclust(x, nc)
      centers <- a$centers
      groesse <- rep(0, nc)
      for (i in seq(nc)) {
        groesse[i] <- length(which(a$cluster == i))
      }
      size <- groesse
      clustresult <- a$cluster
    }
    # if (clustermethod == 'Mclust' && opt == FALSE) { if(!exists('lm', mode='function'))
    # stop('library(mclust) have to be installed and loaded first') a <- Mclust(x, nc, nc)
    # centers <- t(a$mu) groesse <- rep(0, nc) for (i in seq(nc)) { groesse[i] <-
    # length(which(a$classification == i)) } size <- groesse clustresult <- a$classification }
    # if (clustermethod == 'Mclust' && opt == TRUE) { if(!exists('lm', mode='function'))
    # stop('library(mclust) have to be installed and loaded first') a <- Mclust(x, 2, nc)
    # centers <- t(a$mu) nc <- a$G groesse <- rep(0, nc) for (i in seq(nc)) { groesse[i] <-
    # length(which(a$classification == i)) } size <- groesse clustresult <- a$classification }
    list(centers = centers, clustresult = clustresult, nc = nc)
  }
  prcompRob <- function(X, k = 0, sca = "mad", scores = TRUE) {
    n <- nrow(X)
    p <- ncol(X)
    if (k == 0) {
      p1 <- min(n, p)
    } else {
      p1 <- k
    }
    S <- rep(1, p1)
    V <- matrix(1:(p * p1), ncol = p1, nrow = p)
    P <- diag(p)
    m <- apply(X, 2, median)
    Xcentr <- scale(X, center = m, scale = FALSE)
    for (k in 1:p1) {
      B <- Xcentr %*% P
      Bnorm <- sqrt(apply(B^2, 1, sum))
      A <- diag(1/Bnorm) %*% B
      Y <- A %*% P %*% t(X)
      if (sca == "mad")
        s <- apply(Y, 1, mad)
      # if (sca == 'tau') s <- apply(Y, 1, scale.tau) if (sca == 'A') s <- apply(Y, 1, scale.a)
      j <- order(s)[n]
      S[k] <- s[j]
      V[, k] <- A[j, ]
      if (V[1, k] < 0)
        V[, k] <- (-1) * V[, k]
      P <- P - (V[, k] %*% t(V[, k]))
    }
    if (scores) {
      list(scale = S, loadings = V, scores = Xcentr %*% V)
    } else list(scale = S, loadings = V)
  }
  xall <- x
  if (!is.null(strata_variables)) {
    if (!all(strata_variables %in% colnames(x)))
      stop("strata_variables are not found in the data set!") else {
      byvar <- rep("", nrow(x))
      for (i in 1:length(strata_variables)) {
        byvar <- paste(byvar, x[, strata_variables[i]], sep = "-")
      }
      xsp <- split(x, as.factor(byvar))
    }
  } else {
    xsp <- list(dataset = x)
  }
  reslist <- list()
  for (spind in 1:length(xsp)) {
    x <- xsp[[spind]][, variables, drop = FALSE]
    fot <- factorOfTotals(x, aggr)
    if (method == "simple" || method == "single" || method == "pca" || method == "mcdpca" ||
      method == "pppca") {
      clustering <- FALSE
    } else {
      clustering <- TRUE
    }
    if (method == "simple") {
      index <- indexMicro(x, aggr)
      m <- means(x = x, index = index, measure = measure, trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
      }
      res <- list(x = x, method = method, clustering = clustering, aggr = aggr, nc = nc,
        xm = m, roundxm = mr, clustermethod = clustermethod, measure = measure, trim = trim,
        varsort = varsort, transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    } else if (method == "single") {
      sortvec <- sort(x[, varsort], index.return = TRUE)$ix
      xx <- x[sortvec, ]
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure, trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering, aggr = aggr, nc = nc,
        xm = m, roundxm = mr, clustermethod = clustermethod, measure = measure, trim = trim,
        varsort = varsort, transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    } else if (method == "onedims") {
      i <- dim(x)[2]
      xx <- sapply(1:i, function(i) {
        x[order(x[, i]), i]
      })
      xxx <- sapply(1:i, function(i) {
        rank(x[, i], ties.method = "min")
      })
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure, trim = trim)
      mr <- round(m)
      blow = TRUE
      b <- blowup(x, m, aggr)
      y <- x
      for (i in 1:dim(x)[2]) {
        y[, i] <- b[xxx[, i], i]
      }
      res <- list(x = x, method = method, clustering = clustering, aggr = aggr, nc = nc,
        xm = m, roundxm = mr, clustermethod = clustermethod, measure = measure, trim = trim,
        varsort = varsort, transf = transf, blow = blow, blowxm = y, fot = fot)
    } else if (method == "pca") {
      p <- princomp(scale(x))
      s1 <- sort(p$scores[, 1], index.return = TRUE)$ix
      xx <- x[s1, ]
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure, trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering, aggr = aggr, nc = nc,
        xm = m, roundxm = mr, clustermethod = clustermethod, measure = measure, trim = trim,
        varsort = varsort, transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    } else if (method == "mcdpca") {
      x.mcd <- cov.mcd(x, cor = TRUE)
      x.scale <- scale(x, x.mcd$center, sqrt(diag(x.mcd$cor)))
      p <- princomp(x.scale, covmat = x.mcd)
      s1 <- sort(p$scores[, 1], index.return = TRUE)$ix
      xx <- x[s1, ]
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure, trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering, aggr = aggr, nc = nc,
        xm = m, roundxm = mr, clustermethod = clustermethod, measure = measure, trim = trim,
        varsort = varsort, transf = transf, blowup = blowup, blowxm = blowxm, fot = fot)
    } else if (method == "pppca") {
      p <- prcompRob(x)
      s1 <- sort(p$scores[, 1], index.return = TRUE)$ix
      xx <- x[s1, ]
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure, trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering, aggr = aggr, nc = nc,
        xm = m, roundxm = mr, clustermethod = clustermethod, measure = measure, trim = trim,
        varsort = varsort, transf = transf, blowup = blowup, blowxm = blowxm, fot = fot)
    } else if (method == "influence") {
      ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod, opt = FALSE, transf = "log")
      cent <- matrix(ac.scale$centers, ncol = nc, byrow = TRUE)
      j <- matrix(ncol = 1, nrow = nc)
      vmax <- matrix(ncol = 1, nrow = nc)
      for (i in 1:nc) {
        j[i, ] <- max(cent[, i])
        vmax[i, ] <- which(cent[, i] == j[i, ])
      }
      ncols <- c(1:ncol(x))
      xx <- list()
      for (i in 1:nc) {
        w <- which(ac.scale$clustresult == i)
        s <- x[w, , drop = FALSE]
        xx[[i]] <- s[order(s[, vmax[i]]), ]
      }
      for (i in 1:nc) {
        if (i == 1) {
          yy <- matrix(unlist(xx[[i]]), ncol = ncol(x), dimnames = list(rownames(xx[[i]]),
          colnames(xx[[i]])))
        }
        if (i > 1) {
          yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x), dimnames = list(rownames(xx[[i]]),
          colnames(xx[[i]]))))
        }
      }
      xx <- yy
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure, trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(yy)
      }
      res <- list(x = x, method = method, clustering = clustering, aggr = aggr, nc = ac.scale$nc,
        xm = m, roundxm = mr, clustermethod = clustermethod, measure = measure, trim = trim,
        varsort = varsort, transf = transf, blowup = blowup, blowxm = blowxm, fot = fot)
    } else if (method == "clustpca") {
      ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod, opt = FALSE, transf = "log")
      cent <- matrix(ac.scale$centers, ncol = nc, byrow = TRUE)
      xx <- list()
      for (i in 1:nc) {
        w <- which(ac.scale$clustresult == i)
        if (length(w) < dim(x)[2]) {
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[order(y[, varsort]), ]
        } else {
          p <- princomp(scale(x[w, , drop = FALSE]))$scores[, 1]
          psortind <- sort(p, index.return = TRUE)$ix
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[psortind, ]
        }
      }
      for (i in 1:nc) {
        if (i == 1) {
          yy <- matrix(unlist(xx[[i]]), ncol = ncol(x), dimnames = list(rownames(xx[[i]]),
          colnames(xx[[i]])))
        }
        if (i > 1) {
          yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x), dimnames = list(rownames(xx[[i]]),
          colnames(xx[[i]]))))
        }
      }
      xx <- yy
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure, trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering, aggr = aggr, nc = ac.scale$nc,
        xm = m, roundxm = mr, clustermethod = clustermethod, measure = measure, trim = trim,
        varsort = varsort, transf = transf, blowup = blowup, blowxm = blowxm, fot = fot)
    } else if (method == "clustmcdpca") {
      ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod, opt = FALSE, transf = "log")
      cent <- matrix(ac.scale$centers, ncol = nc, byrow = TRUE)
      xx <- list()
      for (i in 1:nc) {
        w <- which(ac.scale$clustresult == i)
        if (length(w) < dim(x)[2]) {
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[order(y[, varsort]), ]
        } else {
          x.mcd <- cov.mcd(x[w, ], cor = TRUE)
          x.scale <- scale(x[, w], x.mcd$center, sqrt(diag(x.mcd$cor)))
          p <- princomp(x.scale, covmat = x.mcd)$scores[, 1]
          psortind <- sort(p, index.return = TRUE)$ix
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[psortind, ]
        }
      }
      for (i in 1:nc) {
        if (i == 1) {
          yy <- matrix(unlist(xx[[i]]), ncol = ncol(x), dimnames = list(rownames(xx[[i]]),
          colnames(xx[[i]])))
        }
        if (i > 1) {
          yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x), dimnames = list(rownames(xx[[i]]),
          colnames(xx[[i]]))))
        }
      }
      xx <- yy
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure, trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering, aggr = aggr, nc = ac.scale$nc,
        xm = m, roundxm = mr, clustermethod = clustermethod, measure = measure, trim = trim,
        varsort = varsort, transf = transf, blowup = blowup, blowxm = blowxm, fot = fot)
    } else if (method == "clustpppca") {
      ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod, opt = FALSE, transf = transf)
      cent <- matrix(ac.scale$centers, ncol = nc, byrow = TRUE)
      xx <- list()
      for (i in 1:nc) {
        w <- which(ac.scale$clustresult == i)
        if (length(w) < dim(x)[2]) {
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[order(y[, varsort]), ]
        } else {
          p <- prcompRob(x[w, , drop = FALSE], 1)$scores
          psortind <- sort(p, index.return = TRUE)$ix
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[psortind, ]
        }
      }
      for (i in 1:nc) {
        if (i == 1) {
          yy <- matrix(unlist(xx[[i]]), ncol = ncol(x), dimnames = list(rownames(xx[[i]]),
          colnames(xx[[i]])))
        }
        if (i > 1) {
          yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x), dimnames = list(rownames(xx[[i]]),
          colnames(xx[[i]]))))
        }
      }
      xx <- yy
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure, trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering, aggr = aggr, nc = ac.scale$nc,
        xm = m, roundxm = mr, clustermethod = clustermethod, measure = measure, trim = trim,
        varsort = varsort, transf = transf, blowup = blowup, blowxm = blowxm, fot = fot)
    } else if (method == "rmd") {
      # dyn.load('functionsRMD.dll')
      y <- x
      cm <- colMeans(x, na.rm = TRUE)
      csd <- apply(x, 2, sd, na.rm = TRUE)
      len <- nrow(y)

      y <- apply(y, 2, function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))

      # eventuell C-Code (bringt aber nicht sonderlich viel!  wenn dann noch irgendwo
      # dyn.load('functionsRMD.dll') hinschreiben for (i in 1:ncol(y)) { y[,i] <-
      # .C('standardise', erg=as.double(y[,i]), as.integer(len), as.double(cm[i]),
      # as.double(csd[i]))$erg }

      d <- as.matrix(dist(y))
      set.seed(123)
      rr <- covMcd(y)
      md <- mahalanobis(y, center = rr$center, cov = rr$cov)
      diag(d) <- 0
      kn <- function(ds, aggr) {
        w <- rep(0, aggr)
        for (i in 1:aggr) {
          w[i] <- which.min(ds)
          ds[w[i]] <- NA
        }
        return(w)
      }

      for (i in 1:(floor(dim(x)[1]/aggr) - 1)) {
        s <- which.max(md)
        w <- kn(d[, s], aggr)
        d[w, ] <- NA
        md[w] <- NA
        y[w, ] <- rep(colMeans(y[w, ]), each = aggr)
      }
      w <- which(!is.na(d[, 1]))
      y[w, ] <- rep(colMeans(y[w, ]), each = length(w))
      for (i in 1:dim(x)[2]) {
        y[, i] <- as.numeric((y[, i] * csd[i]) + cm[i])
        # eventuell C-Code ausfuehren: Bringt aber nicht viel und Probleme beim Vergleich mit der
        # bisherigen Version y[,i] <- as.numeric(.C('restandardise', erg=as.double(y[,i]),
        # as.integer(nrow(x)), as.double(cm[i]), as.double(csd[i]))$erg)
      }

      res <- list(x = x, method = method, clustering = clustering, aggr = aggr, nc = nc,
        xm = y, roundxm = round(y), clustermethod = clustermethod, measure = measure,
        trim = trim, varsort = varsort, transf = transf, blow = TRUE, blowxm = y, fot = fot)
    } else if (method == "mdav") {
      ######## R method mdav - deprecated, because faster c++ is available
      resX <- mdav(x, variables = NULL, weights = NULL, K = aggr, missing = -999)
      res <- list(x = x, method = method, clustering = NULL, aggr = aggr, nc = NULL,
        xm = NULL, roundxm = NULL, clustermethod = NULL, measure = "mean", trim = NULL,
        varsort = NULL, transf = NULL, blow = NULL, blowxm = resX, fot = fot)
    } else if (method == "mdavold") {
      ######## R method mdavold - deprecated, because faster c++ is available
      maxDistinct <- function(d, j) {
        return(which(d == max(d), arr.ind = TRUE)[1, j])
      }
      distToVec <- function(x) {
        b <- as.matrix(dist(rbind(x, maxD)))[1, -1]
        return(b)
      }
      d <- as.matrix(dist(x))
      maxD <- matrix(ncol = ncol(x), nrow = 1)
      distVecs <- matrix(ncol = dim(x)[1], nrow = 1)
      maxD <- x[maxDistinct(d, 1), ]
      findNearest <- function(x, d) {
        ind1 <- which(d == max(d), arr.ind = TRUE)[1, 1]
        distVecs1 <- d[ind1, ]
        maxD1 <- x[ind1, ]
        ind2 <- as.vector(which.max(d[ind1, ]))
        maxD2 <- x[ind2, ]
        distVecs2 <- d[ind2, ]
        s1 <- sort(distVecs1, index.return = TRUE)$ix[1:aggr]
        s2 <- sort(distVecs2, index.return = TRUE)$ix[1:aggr]
        sk <- rbind(x[s1, , drop = FALSE], x[s2, , drop = FALSE])
        x <- x[-c(s1, s2), , drop = FALSE]
        d <- as.matrix(dist(x))
        list(sk = sk, x = x, d = d)
      }
      a <- findNearest(x, d)
      b <- a$sk
      while (dim(a$x)[1] > 2 * aggr) {
        a <- findNearest(a$x, a$d)
        b <- rbind(b, a$sk)
      }
      if (dim(a$x)[1] <= 2 * aggr) {
        b <- rbind(b, a$x)
      }
      index <- indexMicro(b, aggr)
      m <- means(x = b, index = index, measure = measure, trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(b, m, aggr)
      }
      res <- list(x = b, method = method, clustering = clustering, aggr = aggr, nc = nc,
        xm = m, roundxm = mr, clustermethod = clustermethod, measure = measure, trim = trim,
        varsort = varsort, transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    }
    reslist[[spind]] <- res
  }
  res <- reslist[[1]]
  if (length(reslist) > 1) {
    blowxm <- vector()
    fot <- vector()
    for (i in 1:length(reslist)) {
      blowxm <- rbind(blowxm, reslist[[i]]$blowxm)
      fot <- c(fot, reslist[[i]]$fot)
    }
    res$x <- xall
    res$blowxm <- blowxm
    names(fot) <- substring(names(xsp), 2)
    res$fot <- fot
  }

  res$x <- res$x[order(as.numeric(rownames(res$x))), ]
  res$blowxm <- res$blowxm[order(as.numeric(rownames(res$blowxm))), ]
  res$blowxm <- res$blowxm[1:nrow(xall), ]
  class(res) <- "micro"
  res$mx <- res$blowxm
  resv <- c("mx", "x", "method", "aggr", "measure", "fot")
  res1 <- list()
  for (v in resv) res1[[v]] <- res[[v]]
  # measure = measure, trim = trim, varsort = varsort, transf = transf, blow = blow, blowxm =
  # blowxm, fot = fot
  class(res1) <- "micro"
  return(res1)
}

#' Print method for objects from class micro
#'
#' Print method for objects from class micro.
#'
#' @param x object from class micro
#' @param \dots Additional arguments passed through.
#' @return information about method and aggregation level from objects of class
#' micro.
#' @author Matthias Templ
#' @seealso \code{\link{microaggregation}}
#' @keywords print
#' @method print micro
#' @export
#' @examples
#'
#' data(free1)
#' m1 <- microaggregation(free1[, 31:34], method="onedims", aggr=3)
#' m1
#'
print.micro <- function(x, ...) {
  cat(paste("\n Object created with method", x$method, "and aggregation level", x$aggr))
  cat("\n -------------------------\n")
  cat("x ... original values \n")
  print(summary(x$x))
  cat("\n -------------------------\n")
  cat("mx ... microaggregated values\n")
  print(summary(x$mx))
  cat("\n -------------------------\n")

  cat("Try names(your object from class micro) for more details")
  cat("\n")
}

#' Summary method for objects from class micro
#'
#' Summary method for objects from class \sQuote{micro}.
#'
#' This function computes several measures of information loss, such as
#'
#' @param object objects from class micro
#' @param \dots Additional arguments passed through.
#' @return
#' \item{meanx }{A conventional summary of the original data}
#' \item{meanxm }{A conventional summary of the microaggregated data}
#' \item{amean }{average relative absolute deviation of means}
#' \item{amedian}{average relative absolute deviation of medians}
#' \item{aonestep }{average relative absolute deviation of onestep from median}
#' \item{devvar }{average relative absolute deviation of variances}
#' \item{amad }{average relative absolute deviation of the mad}
#' \item{acov }{average relative absolute deviation of covariances}
#' \item{arcov }{average relative absolute deviation of robust (with mcd) covariances}
#' \item{acor }{average relative absolute deviation of correlations}
#' \item{arcor }{average relative absolute deviation of robust (with mcd) correlations}
#' \item{acors }{average relative absolute deviation of rank-correlations}
#' \item{adlm }{average absolute deviation of lm regression coefficients (without intercept)}
#' \item{adlts}{average absolute deviation of lts regression coefficients (without intercept)}
#' \item{apcaload }{average absolute deviation of pca loadings}
#' \item{apppacaload }{average absolute deviation of robust (with projection pursuit approach) pca loadings}
#' \item{atotals }{average relative absolute deviation of totals}
#' \item{pmtotals }{average relative deviation of totals}
#' @author Matthias Templ
#' @seealso \code{\link{microaggregation}}, \code{\link{valTable}}
#' @references Templ, M. \emph{Statistical Disclosure Control for Microdata
#' Using the R-Package sdcMicro}, Transactions on Data Privacy, vol. 1, number
#' 2, pp. 67-85, 2008. \url{http://www.tdp.cat/issues/abs.a004a08.php}
#' @keywords print
#' @method summary micro
#' @export
#' @examples
#'
#' data(Tarragona)
#' m1 <- microaggregation(Tarragona, method="onedims", aggr=3)
#' ## summary(m1)
#'
summary.micro <- function(object, ...) {
  prcompRob <- function(X, k = 0, sca = "mad", scores = TRUE) {
    ## Copyright: Croux and Filzmoser
    n <- nrow(X)
    p <- ncol(X)
    if (k == 0) {
      p1 <- min(n, p)
    } else {
      p1 <- k
    }
    S <- rep(1, p1)
    V <- matrix(1:(p * p1), ncol = p1, nrow = p)
    P <- diag(p)
    m <- apply(X, 2, median)
    Xcentr <- scale(X, center = m, scale = FALSE)
    for (k in 1:p1) {
      B <- Xcentr %*% P
      Bnorm <- sqrt(apply(B^2, 1, sum))
      A <- diag(1/Bnorm) %*% B
      Y <- A %*% P %*% t(X)
      if (sca == "mad")
        s <- apply(Y, 1, mad)
      # if (sca == 'tau') s <- apply(Y, 1, scale.tau) if (sca == 'A') s <- apply(Y, 1, scale.a)
      j <- order(s)[n]
      S[k] <- s[j]
      V[, k] <- A[j, ]
      if (V[1, k] < 0)
        V[, k] <- (-1) * V[, k]
      P <- P - (V[, k] %*% t(V[, k]))
    }
    if (scores) {
      list(scale = S, loadings = V, scores = Xcentr %*% V)
    } else list(scale = S, loadings = V)
  }

  x1 <- as.data.frame(object$x)
  x2 <- if (length(as.data.frame(object$mx)) > 0)
    as.data.frame(object$mx) else as.data.frame(object$mx)
  colnames(x2) <- colnames(x1)
  amx <- mapply(mean, x1)
  amxn <- mapply(mean, x2)
  amean <- sum(abs(amx - amxn)/(abs(amx)))
  meds1 <- mapply(median, x1)
  meds2 <- mapply(median, x2)
  amedian <- sum(abs(meds1 - meds2)/abs(meds1), na.rm = TRUE)
  onestep <- function(x) {
    y <- x
    constant <- 3/1.486
    m1 <- mapply(median, x)
    m2 <- mapply(mad, x)
    limit1 <- m1 + constant * m2
    limit2 <- m1 - constant * m2
    for (i in 1:dim(x)[2]) {
      if (any(x[, i] > limit1[i])) {
        w <- which(x[, i] > limit1[i])
        le <- length(w)
        y[w, i] <- limit1[i]
      }
      if (any(x[, i] < limit2[i])) {
        w <- which(x[, i] < limit2[i])
        le <- length(w)
        y[w, i] <- limit2[i]
      }
    }
    y
  }
  aox <- onestep(x1)
  aox <- mapply(mean, aox)
  aoxm <- onestep(x2)
  aoxm <- mapply(mean, aoxm)
  aonestep <- sum(abs(aox - aoxm)/abs(aox), na.rm = TRUE)
  devvar <- sum(abs(var(x1) - var(x2))/abs(var(x1)))/length(x1)
  amx <- mapply(mad, x1)
  amxn <- mapply(mad, x2)
  amad <- sum(abs(amx - amxn)/(abs(amx)), na.rm = TRUE)
  acov <- sum(abs(cov(x1) - cov(x2))/abs(cov(x1)))/(2 * length(x1))
  # if (robCov == TRUE) { arcov <- sum(abs(covMcd(x1)$cov -
  # covMcd(x2)$cov)/abs(covMcd(x1)$cov))/(2 * length(x1)) } else {
  arcov <- NA
  # }
  acor <- sum(abs(cor(x1) - cor(x2))/abs(cor(x1)))/(2 * length(x2))
  # if (robCov == TRUE) { arcor <- sum(abs(covMcd(x1, cor = TRUE)$cor - covMcd(x2, cor =
  # TRUE)$cor)/abs(covMcd(x1, cor = TRUE)$cor))/(2 * length(x1)) } else {
  arcor <- NA
  # }
  acors <- sum(abs(cor(x1, method = "spearman") - cor(x2, method = "spearman"))/abs(cor(x1,
                                                                                        method = "spearman")))/(2 * length(x1))
  l1 <- lm(as.matrix(x1[, 1]) ~ as.matrix(x1[, -1]))$coeff
  l2 <- lm(as.matrix(x2[, 1]) ~ as.matrix(x2[, -1]))$coeff
  adlm <- sum(abs(l1[2:length(l1)] - l2[2:length(l2)]), na.rm = TRUE)
  # if (robReg == TRUE) { l1 <- lqs(as.matrix(x1[, 1]) ~ as.matrix(x1[, -1]), method =
  # 'lts')$coeff l2 <- lqs(as.matrix(x2[, 1]) ~ as.matrix(x2[, -1]), method = 'lts')$coeff
  # adlts <- sum(abs(l1[2:length(l1)] - l2[2:length(l2)])) } else {
  adlts <- NA
  # }
  if (dim(x1)[1] > dim(x1)[2] && dim(x2)[1] > dim(x2)[2]) {
    p1 <- princomp(x1)
    p2 <- princomp(x2)
    cp1 <- colMeans(p1$load)
    cp2 <- colMeans(p2$load)
    apcaload <- sum(abs(cp1 - cp2)/abs(cp1))
  } else {
    apcaload = "too less observations"
  }
  if (dim(x1)[1] > dim(x1)[2] && dim(x2)[1] > dim(x2)[2]) {
    p1 <- prcompRob(x1)
    p2 <- prcompRob(x2)
    cp1 <- colMeans(p1$load)
    cp2 <- colMeans(p2$load)
    apppcaload <- sum(abs(cp1 - cp2)/abs(cp1))
  } else {
    apppcaload = "too less observations"
  }
  cmx1 <- apply(x1, 2, sum)
  cmx2 <- apply(x2, 2, sum) * object$fot
  atotals <- sum(abs((cmx1 - cmx2)/cmx1))
  pmtotals <- sum((cmx2 - cmx1)/cmx1)
  util1 <- dUtility(x1, x2)
  deigenvalues <- dUtility(x1, x2, method = "eigen")
  risk0 <- dRisk(x1, x2)
  r <- dRiskRMD(x1, x2, k = 0.7)
  risk1 <- r$risk1
  risk2 <- r$risk2
  wrisk1 <- r$wrisk1
  wrisk2 <- r$wrisk2
  list(meansx = summary(x1), meansxm = summary(x2), amean = amean, amedian = amedian, aonestep = aonestep,
       devvar = devvar, amad = amad, acov = acov, arcov = arcov, acor = acor, arcor = arcor,
       acors = acors, adlm = adlm, adlts = adlts, apcaload = apcaload, apppcaload = apppcaload,
       totalsOrig = cmx1, totalsMicro = cmx2, atotals = atotals, pmtotals = pmtotals, util1 = util1,
       deigenvalues = deigenvalues, risk0 = risk0, risk1 = risk1, risk2 = risk2, wrisk1 = wrisk1,
       wrisk2 = wrisk2)
}

