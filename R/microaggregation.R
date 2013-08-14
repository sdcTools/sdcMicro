setGeneric('microaggregation', function(obj,variables,aggr=3,strata_variables=NULL,method="mdav",weights=NULL, nc = 8,
        clustermethod = "clara", opt = FALSE, measure = "mean", trim = 0, varsort = 1, transf = "log") {
      standardGeneric('microaggregation')})
setMethod(f='microaggregation', signature=c('sdcMicroObj'),
    definition=function(obj,variables,aggr=3,strata_variables=NULL,method="mdav",weights=NULL, nc = 8,
        clustermethod = "clara", opt = FALSE, measure = "mean", trim = 0, varsort = 1, transf = "log") { 
      x <- get.sdcMicroObj(obj, type="manipNumVars")
      if(!is.null(strata_variables)){
        sx <- get.sdcMicroObj(obj, type="origData")[,strata_variables[!strata_variables%in%variables],drop=FALSE]
        x <- cbind(x,sx)
      }
      
      if(!is.null(weights)&&!is.null(get.sdcMicroObj(obj, type="weightVar"))){
        weights <- get.sdcMicroObj(obj, type="origData")[,get.sdcMicroObj(obj, type="weightVar")]
      }
      res <- microaggregationWORK(x, variables=variables, aggr=aggr,strata_variables=strata_variables,method=method,
          weights=weights,nc=nc,clustermethod=clustermethod,opt=opt,
          measure=measure,trim=trim,varsort=varsort,transf=transf)
      obj <- nextSdcObj(obj)
      x[,variables] <- res$mx
      
      obj <- set.sdcMicroObj(obj, type="manipNumVars", input=list(as.data.frame(x)))
      
      obj <- dRisk(obj)
#      obj <- dRiskRMD(obj)
      obj <- dUtility(obj)
      
      obj
    })
setMethod(f='microaggregation', signature=c("data.frame"),
    definition=function(obj,variables,aggr=3,strata_variables=NULL,method="mdav",weights=NULL, nc = 8,
        clustermethod = "clara", opt = FALSE, measure = "mean", trim = 0, varsort = 1, transf = "log") { 
      microaggregationWORK(x=obj,variables=variables,aggr=aggr,strata_variables=strata_variables,method=method,
          weights=weights,nc=nc,clustermethod=clustermethod,opt=opt,measure=measure,trim=trim,varsort=varsort,
          transf=transf)
    })
setMethod(f='microaggregation', signature=c("matrix"),
    definition=function(obj,variables,aggr=3,strata_variables=NULL,method="mdav",weights=NULL, nc = 8,
        clustermethod = "clara", opt = FALSE, measure = "mean", trim = 0, varsort = 1, transf = "log") { 
      microaggregationWORK(x=obj,variables=variables,aggr=aggr,strata_variables=strata_variables,method=method,
          weights=weights,nc=nc,clustermethod=clustermethod,opt=opt,measure=measure,trim=trim,varsort=varsort,
          transf=transf)
    })


microaggregationWORK <- function (x, variables=colnames(x),method = "mdav", aggr = 3, weights=NULL, nc = 8,
    clustermethod = "clara",
    opt = FALSE, measure = "mean", trim = 0, varsort = 1, transf = "log", strata_variables=NULL
)
#,blow = TRUE, blowxm = 0)
{
  blow=TRUE
  weightedQuantile <- function (x, weights = NULL, probs = seq(0, 1, 0.25), sorted = FALSE, na.rm = FALSE) {
    if (!is.numeric(x)) 
      stop("'x' must be a numeric vector")
    n <- length(x)
    if (n == 0 || (!isTRUE(na.rm) && any(is.na(x)))) {
      return(rep.int(NA, length(probs)))
    }
    if (!is.null(weights)) {
      if (!is.numeric(weights)) 
        stop("'weights' must be a numeric vector")
      else if (length(weights) != n) {
        stop("'weights' must have the same length as 'x'")
      }
      else if (!all(is.finite(weights))) 
        stop("missing or infinite weights")
      if (any(weights < 0)) 
        warning("negative weights")
      if (!is.numeric(probs) || all(is.na(probs)) || isTRUE(any(probs < 
                  0 | probs > 1))) {
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
      rw <- (1:n)/n
    else rw <- cumsum(weights)/sum(weights)
    q <- sapply(probs, function(p) {
          if (p == 0) 
            return(x[1])
          else if (p == 1) 
            return(x[n])
          select <- min(which(rw >= p))
          if (rw[select] == p) 
            mean(x[select:(select + 1)])
          else x[select]
        })
    return(unname(q))
  }
  weightedMedian <- function (x, weights = NULL, sorted = FALSE, na.rm = FALSE) {
    weightedQuantile(x, weights, probs = 0.5, sorted = sorted, 
        na.rm = na.rm)
  }
  
  rownames(x) <- 1:nrow(x)
  stopifnot(method %in% c("simple", "single", "onedims", "pca",
          "mcdpca", "pppca", "clustmcdpca", "clustpppca", "clustpca",
          "rmd", "mdav", "influence","mdavold"))
  #if (method == "rmd") {
  #    blow = FALSE
  #    warning("object$blow have been set to TRUE and \n
  #             object$xm == object$blowxm \n--------\n")
  #}
  factorOfTotals <- function(x, aggr) {
    n <- dim(x)[1]
    abgerundet <- floor(n/aggr)
    fot <- solve(abgerundet, n)
    fot
  }
  indexMicro <- function(x, aggr) {
    n <- dim(x)[1]
    if (n < 2 * aggr) {
      stop(paste("Too less observations (", n, ") for aggregate = ",
              aggr, sep = ""))
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
    }
    else {
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
        m[i, ] <- apply(x[index[[i]], ], 2, function(x) weighted.mean(x, w=weights[index[[i]]]))
      }
    }
    if (measure == "median" & !is.null(weights)) {
      for (i in 1:length(index)) {
        m[i, ] <- apply(x[index[[i]], ], 2, function(x) weightedMedian(x, weights=weights[index[[i]]]))
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
    }
    else {
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
  clust <- function(x, nc, clustermethod = "clara", opt = FALSE,
      transf = "log") {
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
    #       if (clustermethod == "Mclust" && opt == FALSE) {
#			if(!exists("lm",  mode="function")) stop("library(mclust) have to be installed and loaded first") 
#            a <- Mclust(x, nc, nc)
#            centers <- t(a$mu)
#            groesse <- rep(0, nc)
#            for (i in seq(nc)) {
#                groesse[i] <- length(which(a$classification ==
#                  i))
#            }
#            size <- groesse
#            clustresult <- a$classification
#        }
#        if (clustermethod == "Mclust" && opt == TRUE) {
#			if(!exists("lm",  mode="function")) stop("library(mclust) have to be installed and loaded first")
#            a <- Mclust(x, 2, nc)
#            centers <- t(a$mu)
#            nc <- a$G
#            groesse <- rep(0, nc)
#            for (i in seq(nc)) {
#                groesse[i] <- length(which(a$classification ==
#                  i))
#            }
#            size <- groesse
#            clustresult <- a$classification
#        }
    list(centers = centers, clustresult = clustresult, nc = nc)
  }
  prcompRob <- function(X, k = 0, sca = "mad", scores = TRUE) {
    n <- nrow(X)
    p <- ncol(X)
    if (k == 0) {
      p1 <- min(n, p)
    }
    else {
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
      #if (sca == "tau")
      #    s <- apply(Y, 1, scale.tau)
      #if (sca == "A")
      #    s <- apply(Y, 1, scale.a)
      j <- order(s)[n]
      S[k] <- s[j]
      V[, k] <- A[j, ]
      if (V[1, k] < 0)
        V[, k] <- (-1) * V[, k]
      P <- P - (V[, k] %*% t(V[, k]))
    }
    if (scores) {
      list(scale = S, loadings = V, scores = Xcentr %*%
              V)
    }
    else list(scale = S, loadings = V)
  }
  xall <- x
  if(!is.null(strata_variables)){
    if(!all(strata_variables%in%colnames(x)))
      stop("strata_variables are not found in the data set!")
    else{
      byvar <- rep("",nrow(x))
      for(i in 1:length(strata_variables)){
        byvar <- paste(byvar,x[,strata_variables[i]],sep="-")
      }
      xsp <- split(x,as.factor(byvar))
    }
  }else{
    xsp <- list(dataset=x)
  }
  reslist <- list()
  for(spind in 1:length(xsp)){
    x <- xsp[[spind]][,variables,drop=FALSE]
    fot <- factorOfTotals(x, aggr)
    if (method == "simple" || method == "single" || method ==
        "pca" || method == "mcdpca" || method == "pppca") {
      clustering <- FALSE
    }
    else {
      clustering <- TRUE
    }
    if (method == "simple") {
      index <- indexMicro(x, aggr)
      m <- means(x = x, index = index, measure = measure, trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
      }
      res <- list(x = x, method = method, clustering = clustering,
          aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod,
          measure = measure, trim = trim, varsort = varsort,
          transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    }else if (method == "single") {
      sortvec <- sort(x[, varsort], index.return = TRUE)$ix
      xx <- x[sortvec, ]
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure,
          trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering,
          aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod,
          measure = measure, trim = trim, varsort = varsort,
          transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    }else if (method == "onedims") {
      i <- dim(x)[2]
      xx <- sapply(1:i, function(i) {
            x[order(x[, i]), i]
          })
      xxx <- sapply(1:i, function(i) {
            rank(x[, i], ties.method = "min")
          })
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure,
          trim = trim)
      mr <- round(m)
      blow = TRUE
      b <- blowup(x, m, aggr)
      y <- x
      for (i in 1:dim(x)[2]) {
        y[, i] <- b[xxx[, i], i]
      }
      res <- list(x = x, method = method, clustering = clustering,
          aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod,
          measure = measure, trim = trim, varsort = varsort,
          transf = transf, blow = blow, blowxm = y, fot = fot)
    }else if (method == "pca") {
      p <- princomp(scale(x))
      s1 <- sort(p$scores[, 1], index.return = TRUE)$ix
      xx <- x[s1, ]
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure,
          trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering,
          aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod,
          measure = measure, trim = trim, varsort = varsort,
          transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    }else if (method == "mcdpca") {
      x.mcd <- cov.mcd(x, cor = TRUE)
      x.scale <- scale(x, x.mcd$center, sqrt(diag(x.mcd$cor)))
      p <- princomp(x.scale, covmat = x.mcd)
      s1 <- sort(p$scores[, 1], index.return = TRUE)$ix
      xx <- x[s1, ]
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure,
          trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering,
          aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod,
          measure = measure, trim = trim, varsort = varsort,
          transf = transf, blowup = blowup, blowxm = blowxm,
          fot = fot)
    }else if (method == "pppca") {
      p <- prcompRob(x)
      s1 <- sort(p$scores[, 1], index.return = TRUE)$ix
      xx <- x[s1, ]
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure,
          trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering,
          aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod,
          measure = measure, trim = trim, varsort = varsort,
          transf = transf, blowup = blowup, blowxm = blowxm,
          fot = fot)
    }else if (method == "influence") {
      ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod,
          opt = FALSE, transf = "log")
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
          yy <- matrix(unlist(xx[[i]]), ncol = ncol(x),
              dimnames = list(rownames(xx[[i]]), colnames(xx[[i]])))
        }
        if (i > 1) {
          yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x),
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]]))))
        }
      }
      xx <- yy
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure,
          trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(yy)
      }
      res <- list(x = x, method = method, clustering = clustering,
          aggr = aggr, nc = ac.scale$nc, xm = m, roundxm = mr,
          clustermethod = clustermethod, measure = measure,
          trim = trim, varsort = varsort, transf = transf,
          blowup = blowup, blowxm = blowxm, fot = fot)
    }else if (method == "clustpca") {
      ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod,
          opt = FALSE, transf = "log")
      cent <- matrix(ac.scale$centers, ncol = nc, byrow = TRUE)
      xx <- list()
      for (i in 1:nc) {
        w <- which(ac.scale$clustresult == i)
        if (length(w) < dim(x)[2]) {
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[order(y[, varsort]), ]
        }
        else {
          p <- princomp(scale(x[w, , drop = FALSE]))$scores[,
              1]
          psortind <- sort(p, index.return = TRUE)$ix
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[psortind, ]
        }
      }
      for (i in 1:nc) {
        if (i == 1) {
          yy <- matrix(unlist(xx[[i]]), ncol = ncol(x),
              dimnames = list(rownames(xx[[i]]), colnames(xx[[i]])))
        }
        if (i > 1) {
          yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x),
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]]))))
        }
      }
      xx <- yy
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure,
          trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering,
          aggr = aggr, nc = ac.scale$nc, xm = m, roundxm = mr,
          clustermethod = clustermethod, measure = measure,
          trim = trim, varsort = varsort, transf = transf,
          blowup = blowup, blowxm = blowxm, fot = fot)
    }else if (method == "clustmcdpca") {
      ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod,
          opt = FALSE, transf = "log")
      cent <- matrix(ac.scale$centers, ncol = nc, byrow = TRUE)
      xx <- list()
      for (i in 1:nc) {
        w <- which(ac.scale$clustresult == i)
        if (length(w) < dim(x)[2]) {
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[order(y[, varsort]), ]
        }
        else {
          x.mcd <- cov.mcd(x[w, ], cor = TRUE)
          x.scale <- scale(x[, w], x.mcd$center, sqrt(diag(x.mcd$cor)))
          p <- princomp(x.scale, covmat = x.mcd)$scores[,
              1]
          psortind <- sort(p, index.return = TRUE)$ix
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[psortind, ]
        }
      }
      for (i in 1:nc) {
        if (i == 1) {
          yy <- matrix(unlist(xx[[i]]), ncol = ncol(x),
              dimnames = list(rownames(xx[[i]]), colnames(xx[[i]])))
        }
        if (i > 1) {
          yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x),
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]]))))
        }
      }
      xx <- yy
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure,
          trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering,
          aggr = aggr, nc = ac.scale$nc, xm = m, roundxm = mr,
          clustermethod = clustermethod, measure = measure,
          trim = trim, varsort = varsort, transf = transf,
          blowup = blowup, blowxm = blowxm, fot = fot)
    }else if (method == "clustpppca") {
      ac.scale <- clust(x = x, nc = nc, clustermethod = clustermethod,
          opt = FALSE, transf = transf)
      cent <- matrix(ac.scale$centers, ncol = nc, byrow = TRUE)
      xx <- list()
      for (i in 1:nc) {
        w <- which(ac.scale$clustresult == i)
        if (length(w) < dim(x)[2]) {
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[order(y[, varsort]), ]
        }
        else {
          p <- prcompRob(x[w, , drop = FALSE], 1)$scores
          psortind <- sort(p, index.return = TRUE)$ix
          y <- x[w, , drop = FALSE]
          xx[[i]] <- y[psortind, ]
        }
      }
      for (i in 1:nc) {
        if (i == 1) {
          yy <- matrix(unlist(xx[[i]]), ncol = ncol(x),
              dimnames = list(rownames(xx[[i]]), colnames(xx[[i]])))
        }
        if (i > 1) {
          yy <- rbind(yy, matrix(unlist(xx[[i]]), ncol = ncol(x),
                  dimnames = list(rownames(xx[[i]]), colnames(xx[[i]]))))
        }
      }
      xx <- yy
      index <- indexMicro(xx, aggr)
      m <- means(x = xx, index = index, measure = measure,
          trim = trim)
      mr <- round(m)
      if (blow == TRUE) {
        blowxm <- blowup(x, m, aggr)
        rownames(blowxm) <- rownames(xx)
      }
      res <- list(x = x, method = method, clustering = clustering,
          aggr = aggr, nc = ac.scale$nc, xm = m, roundxm = mr,
          clustermethod = clustermethod, measure = measure,
          trim = trim, varsort = varsort, transf = transf,
          blowup = blowup, blowxm = blowxm, fot = fot)
    } else if (method == "rmd") {
      #dyn.load("functionsRMD.dll")
      y <- x
      cm <- colMeans(x, na.rm = TRUE)
      csd <- sd(x, na.rm = TRUE)
      len <- nrow(y)
      
      y <- apply(y, 2, function(x) (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE))
      
      # eventuell C-Code (bringt aber nicht sonderlich viel!
      # wenn dann noch irgendwo  dyn.load("functionsRMD.dll")  hinschreiben
      #for (i in 1:ncol(y)) {
      #    y[,i] <- .C("standardise", erg=as.double(y[,i]), as.integer(len), as.double(cm[i]), as.double(csd[i]))$erg
      #}
      
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
        w <- kn(d[,s], aggr)
        d[w,] <- NA
        md[w] <- NA
        y[w, ] <- rep(colMeans(y[w, ]), each = aggr)
      }
      w <- which(!is.na(d[, 1]))
      y[w, ] <- rep(colMeans(y[w, ]), each = length(w))
      for (i in 1:dim(x)[2]) {
        y[,i] <- as.numeric((y[, i] * csd[i]) + cm[i])
        # eventuell C-Code ausf�hren: Bringt aber nicht viel und Probleme beim Vergleich mit der bisherigen Version
        #y[,i] <- as.numeric(.C("restandardise", erg=as.double(y[,i]), as.integer(nrow(x)), as.double(cm[i]), as.double(csd[i]))$erg)
      }
      
      res <- list(x = x, method = method, clustering = clustering,
          aggr = aggr, nc = nc, xm = y, roundxm = round(y),
          clustermethod = clustermethod, measure = measure,
          trim = trim, varsort = varsort, transf = transf,
          blow = TRUE, blowxm = y, fot = fot)
    }else if (method == "mdav") {
      ########R method mdav - deprecated, because faster c++ is available		
      resX <- mdav(x,variables=NULL,weights=NULL,K=aggr,missing=-999)
      res <- list(x = x, method = method, clustering = NULL,
          aggr = aggr, nc = NULL, xm = NULL, roundxm = NULL, clustermethod = NULL,
          measure = "mean", trim = NULL, varsort = NULL,
          transf = NULL, blow = NULL, blowxm = resX, fot = fot)
    }else if(method=="mdavold"){
      ########R method mdavold - deprecated, because faster c++ is available      
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
      res <- list(x = b, method = method, clustering = clustering,
          aggr = aggr, nc = nc, xm = m, roundxm = mr, clustermethod = clustermethod,
          measure = measure, trim = trim, varsort = varsort,
          transf = transf, blow = blow, blowxm = blowxm, fot = fot)
    }
    reslist[[spind]]<- res
  }
  res <- reslist[[1]]
  if(length(reslist)>1){
    blowxm <- vector()
    fot <- vector()
    for(i in 1:length(reslist)){
      blowxm <- rbind(blowxm,reslist[[i]]$blowxm)
      fot <- c(fot,reslist[[i]]$fot)
    }
    res$x <- xall
    res$blowxm <- blowxm
    names(fot) <- substring(names(xsp),2)
    res$fot <- fot
  }
  
  res$x <- res$x[order(as.numeric(rownames(res$x))),]
  res$blowxm <- res$blowxm[order(as.numeric(rownames(res$blowxm))),]
  res$blowxm <- res$blowxm[1:nrow(xall),]
  class(res) <- "micro"
  res$mx <- res$blowxm
  resv <- c("mx","x","method", "aggr","measure","fot")
  res1 <- list()
  for(v in resv)
    res1[[v]]<-res[[v]]
#            measure = measure, trim = trim, varsort = varsort,
#            transf = transf, blow = blow, blowxm = blowxm, fot = fot
  class(res1) <- "micro"  
  invisible(res1)
}
