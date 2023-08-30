#' Data-Utility measures
#'
#' [dUtility()] allows to compute different measures of data-utility based
#' on various distances using original and perturbed variables.
#'
#' The standardised distances of the perturbed data values to the original ones
#' are measured. The following measures are available:
#' - `"IL1`: sum of absolute distances between original and perturbed variables 
#' scaled by absolute values of the original variables
#' - `"IL1s`: measures the absolute distances between original
#' and perturbed ones, scaled by the standard deviation of original variables times 
#' the square root of `2`. 
#' - `"eigen`; compares the eigenvalues of original and perturbed data
#' - `"robeigen`; compares robust eigenvalues of original and perturbed data
#' @name dUtility
#' @docType methods
#' @param obj original data or object of class [sdcMicroObj-class]
#' @param ... see arguments below
#' - xm: perturbed data
#' - method: method IL1, IL1s or eigen. More methods are implemented in
#' summary.micro()
#' @return data utility or modified entry for data utility the [sdcMicroObj-class].
#' @author Matthias Templ
#' @seealso [dRisk()], [dRiskRMD()]
#' @references for IL1 and IL1s: see Mateo-Sanz, Sebe, Domingo-Ferrer. 
#' Outlier Protection in Continuous Microdata Masking.
#' International Workshop on Privacy in Statistical Databases.
#' PSD 2004: Privacy in Statistical Databases pp 201-215.
#'
#' Templ, M. and Meindl, B., `Robust Statistics Meets SDC: New Disclosure
#' Risk Measures for Continuous Microdata Masking`, Lecture Notes in Computer
#' Science, Privacy in Statistical Databases, vol. 5262, pp. 113-126, 2008.
#' @keywords manip
#' @export
#' @md
#' @examples
#' data(free1)
#' free1 <- as.data.frame(free1)
#' \donttest{
#' m1 <- microaggregation(free1[, 31:34], method="onedims", aggr=3)
#' m2 <- microaggregation(free1[, 31:34], method="pca", aggr=3)
#' dRisk(obj=free1[, 31:34], xm=m1$mx)
#' dRisk(obj=free1[, 31:34], xm=m2$mx)
#' dUtility(obj=free1[, 31:34], xm=m1$mx)
#' dUtility(obj=free1[, 31:34], xm=m2$mx)
#' data(Tarragona)
#' x <- Tarragona[, 5:7]
#' y <- addNoise(x)$xm
#' dRiskRMD(x, xm=y)
#' dRisk(x, xm=y)
#' dUtility(x, xm = y, method = "IL1")
#' dUtility(x, xm = y, method = "IL1s")
#' dUtility(x, xm = y, method = "eigen")
#' dUtility(x, xm = y, method = "robeigen")
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' ## this is already made internally:
#' ## sdc <- dUtility(sdc)
#' ## and already stored in sdc
#' }
dUtility <- function(obj, ...) {
  dUtilityX(obj=obj, ...)
}
setGeneric("dUtilityX", function(obj, ...) {
  standardGeneric("dUtilityX")
})

setMethod(f="dUtilityX", signature=c("sdcMicroObj"), definition=function(obj, ...) {
  numVars <- get.sdcMicroObj(obj, type = "numVars")
  x <- get.sdcMicroObj(obj, type = "origData")[, numVars, drop = F]
  xm <- get.sdcMicroObj(obj, type = "manipNumVars")
  utility <- get.sdcMicroObj(obj, type = "utility")
  utility$il1 <- dUtilityWORK(x = x, xm = xm, method = "IL1", ...)
  utility$il1s <- dUtilityWORK(x = x, xm = xm, method = "IL1s", ...)
  utility$eigen <- dUtilityWORK(x = x, xm = xm, method = "eigen", ...)
  # utility$robeigen <- dUtilityWORK(x=x, xm=xm, method='robeigen',...)
  obj <- set.sdcMicroObj(obj, type = "utility", input = list(utility))
  obj
})

setMethod(f="dUtilityX", signature=c("data.frame"), definition=function(obj, ...) {
  dUtilityWORK(x = obj, ...)
})

dUtilityWORK <- function(x, xm, method = "IL1s") {
  if (dim(x)[1] != dim(xm)[1]) {
    warnMsg <- "dimension of perturbed data and original data are different\n"
    obj <- addWarning(obj, warnMsg=warnMsg, method="dUtility", variable=NA)
    warning(warnMsg)
    xm <- xm[1:dim(x)[1], ]
  }
  if (method == "IL1") {
    a <- x
    for (i in 1:dim(x)[2]) {
      a[[i]] <- 100 * abs(x[[i]] - xm[[i]]) / (abs(x[[i]])) 
    }
    a[a == "Inf"] <- NA
    infLoss1 <- sum(a, na.rm = TRUE)
    return(infLoss1)
  }
  if (method == "IL1old") {
    a <- x
    for (i in 1:dim(x)[2]) {
      a[[i]] <- abs(x[[i]] - xm[[i]])/(sd(x[[i]], na.rm = TRUE) * sqrt(2))
    }
    infLoss1 <- 1/(dim(x)[2] * dim(x)[1]) * sum(a, na.rm = TRUE)
    return(infLoss1)
  }
  if (method == "IL1s") {
    a <- x
    for (i in 1:dim(x)[2]) {
      a[[i]] <- abs(x[[i]] - xm[[i]]) / (sd(x[[i]], na.rm = TRUE) * sqrt(2))
    }
    infLoss1 <- sum(a, na.rm = TRUE)
    return(infLoss1)
  }
  if (method == "eigen") {
    e1 <- try(eigen(var(scale(x), na.rm = TRUE, use = "pairwise.complete.obs"))$values, silent = TRUE)
    e2 <- try(eigen(var(scale(xm), na.rm = TRUE, use = "pairwise.complete.obs"))$values, silent = TRUE)
    d <- try(sum(abs(e1 - e2)/e1), silent = TRUE)
    if("try-error" %in% class(d)){
      d <- NA
    }
    return(d)
  }
  if (method == "robeigen") {
    e1 <- try(eigen(covMcd(scale(x))$cov)$values, silent = TRUE)
    e2 <- try(eigen(covMcd(scale(xm))$cov)$values, silent = TRUE)
    d <- try(sum(abs(e1 - e2)/e1), silent = TRUE)
    if("try-error" %in% class(d)){
      d <- NA
    }
    return(d)
  }
}


#' Additional Information-Loss measures
#' 
#' Measures [IL_correl()] and [IL_variables()] were proposed by Andrzej Mlodak and are (theoretically) bounded between `0` and `1`.
#' 
#' - `IL_correl()`: is a information-loss measure that can be applied to common numerically scaled variables in `x` and `xm`. It is based
#' on diagonal entries of inverse correlation matrices in the original and perturbed data. 
#' - `IL_variables()`: for common-variables in `x` and `xm` the individual distance-functions depend on the class of the variable; 
#' specifically these functions are different for numeric variables, ordered-factors and character/factor variables. The individual distances
#' are summed up and scaled by `n * m` with `n` being the number of records and `m` being the number of (common) variables. 
#' @author Bernhard Meindl <bernhard.meindl@@statistik.gv.at>
#' @details Details can be found in the references below
#' 
#' The implementation of [IL_correl()] differs slightly with the original proposition from Mlodak, A. (2020) as 
#' the constant multiplier was changed to `1 / sqrt(2)` instead of `1/2` for better efficiency and interpretability
#' of the measure.
#' 
#' @param x an object coercible to a `data.frame` representing the original dataset
#' @param xm an object coercible to a `data.frame` representing the perturbed, modified dataset
#' @param digits number digits used for rounding when displaying results
#' @param ... additional parameter for print-methods; currently ignored
#' @references Mlodak, A. (2020). Information loss resulting from statistical disclosure control of output data,
#' Wiadomosci Statystyczne. The Polish Statistician, 2020, 65(9), 7-27, DOI: 10.5604/01.3001.0014.4121
#' 
#' Mlodak, A. (2019). Using the Complex Measure in an Assessment of the Information Loss Due to the Microdata Disclosure Control, 
#' Przegląd Statystyczny, 2019, 66(1), 7-26, 
#' DOI: 10.5604/01.3001.0013.8285
#' @return the corresponding information-loss measure
#' @export
#' @rdname il_additional
#' @md
#' @examples
#' data("Tarragona", package = "sdcMicro")
#' res1 <- addNoise(obj = Tarragona, variables = colnames(Tarragona), noise = 100)
#' IL_correl(x = as.data.frame(res1$x), xm = as.data.frame(res1$xm))
#' 
#' res2 <- addNoise(obj = Tarragona, variables = colnames(Tarragona), noise = 25) 
#' IL_correl(x = as.data.frame(res2$x), xm = as.data.frame(res2$xm))
IL_correl <- function(x, xm) {
  # compute inverse of correlation matrix and return diagonal entries
  .get_inverse_diags <- function(df) {
    inv_mat <- tryCatch(solve(cor(df, use = "complete.obs")), error = function(e) e)
    if (inherits(inv_mat, "simpleError")) {
      stop("error while computing inverse of correlation matrix", call. = FALSE)
    }
    diag(inv_mat)
  }
  
  stopifnot(is.data.frame(x), is.data.frame(xm))
  
  x <- data.table::setDT(x)
  xm <- data.table::setDT(xm)
  
  cn <- intersect(names(x), names(xm))
  if (length(cn) == 0) {
    stop("please supply data.frames with overlapping variable names", call. = FALSE)
  }
  
  # compute overlapping numeric variables
  numerics_x <- sort(cn[sapply(cn, function(k) {
    is.numeric(x[[k]])
  })])
  
  numerics_y <- sort(cn[sapply(cn, function(k) {
    is.numeric(xm[[k]])
  })])
  
  stopifnot(all.equal(numerics_x, numerics_y))
  
  df_o <- x[, numerics_x, with = FALSE, drop = FALSE]
  df_p <- xm[, numerics_y, with = FALSE, drop = FALSE]
  
  # compute diagonal entries
  diags_o <- .get_inverse_diags(df = df_o)
  diags_p <- .get_inverse_diags(df = df_p)
  
  # compute inner denominator in formula
  denom_o <- sqrt(sum(diags_o^2));  
  denom_p <- sqrt(sum(diags_p^2))
  
  # compute lambda
  res <- sqrt(sum(((diags_o / denom_o) - (diags_p / denom_p))^2)) 
  #res <- 0.5 * res # original proposal
  res <- res  * (1 / sqrt(2))
  
  attr(res, "nr_vars") <- ncol(df_o)
  attr(res, "n_x") <- nrow(x)
  attr(res, "n_y") <- nrow(xm)
  class(res) <- "il_correl"
  res
}

#' @method print il_correl
#' @rdname il_additional
#' @export
print.il_correl <- function(x, digits = 3, ...) {
  cat("Number of records (x): ",  attributes(x)$n_y ," | ")
  cat("Number of records (xm): ",  attributes(x)$n_y ,"\n")
  cat("Number of common numeric variables: ",  attributes(x)$nr_vars, "\n")
  cat("Overall information loss: ", round(x, digits = digits), "\n")
  invisible(x)
}

#' @export
#' @rdname il_additional
#' @examples
#' 
#' # creating test-inputs
#' n <- 150
#' x <- xm <- data.frame(
#'   v1 = factor(sample(letters[1:5], n, replace = TRUE), levels = letters[1:5]),
#'   v2 = rnorm(n),
#'   v3 = runif(3),
#'   v4 = ordered(sample(LETTERS[1:3], n, replace = TRUE), levels = c("A", "B", "C"))
#' )
#' xm$v1[1:5] <- "a"
#' xm$v2 <- rnorm(n, mean = 5)
#' xm$v4[1:5] <- "A"
#' IL_variables(x, xm)
IL_variables <- function(x, xm) {
  dist_nom <- function(x, p) {
    if (!is.character(x) & !inherits(x, "factor")) {
      stop("`x` must be an (ordered) factor or character", call. = FALSE)
    }
    if (!is.character(x) & !inherits(p, "factor")) {
      stop("`p` must be an (ordered) factor or character", call. = FALSE)
    }
    stopifnot(length(x) == length(p))
    
    if (is.factor(x)) x <- as.character(x)
    if (is.factor(p)) p <- as.character(p)
    
    dists <- rep(1, length(x))
    na_x <- is.na(x)
    na_p <- is.na(p)
    
    dists <- rep(1, length(x))
    dists[x == p] <- 0
    dists[na_x | na_p] <- 1
    dists[na_x & na_p] <- 0
    sum(dists)
  }
  dist_ordered <- function(x, p) {
    stopifnot(is.factor(x)); stopifnot("ordered" %in% class(x))
    stopifnot(is.factor(p)); stopifnot("ordered" %in% class(p))
    stopifnot(all.equal(levels(x), levels(p)))
    
    max_diff <- length(levels(x)) - 1
    
    # single NA -> max_diff
    # both NA -> 0
    d <- abs(as.numeric(x) - as.numeric(p))
    d[is.na(x) | is.na(p)] <- max_diff
    d[is.na(x) & is.na(p)] <- 0
    sum(d / max_diff)
  }
  dist_num <- function(x, p) {
    stopifnot(is.numeric(x))
    stopifnot(is.numeric(p))
    
    # max. possible difference
    max_diff <- max(abs(range(p, na.rm = TRUE) - rev(range(x, na.rm = TRUE))))
    
    v <- abs(x - p)
    v[is.na(x) | is.na(p)] <- max_diff
    v[is.na(x) & is.na(p)] <- 0
    sum((2 / pi) * atan(v))
  }
  
  stopifnot(is.data.frame(x), is.data.frame(xm))
  x <- data.table::setDT(x)
  xm <- data.table::setDT(xm)
  stopifnot(nrow(x) == nrow(xm))
  
  cn <- intersect(names(x), names(xm))
  if (length(cn) == 0) {
    stop("please supply data.frames with overlapping variable names", call. = FALSE)
  }
  
  cl_o <- sapply(cn, function(v) {
    class(x[[v]])
  })
  cl_p <- sapply(cn, function(v) {
    class(xm[[v]])
  })
  
  # classes of common-variables must match
  if(!all.equal(cl_o, cl_p)) {
    stop("classes of common-variables in `x` and `xm` must be identical", call. = FALSE)
  }
  
  inp <- data.frame(v = cn, type = "nominal")
  inp[sapply(cn, function(v) inherits(x[[v]], "numeric")), "type"] <- "numeric"
  inp[sapply(cn, function(v) inherits(x[[v]], "ordered")), "type"] <- "ordered"  
  
  # compute distances  
  inp$dists <- sapply(1:nrow(inp), function(y) {
    v <- inp$v[y]
    vals_o <- x[[v]]
    vals_p <- xm[[v]]
    if (inp$type[y] == "numeric") {
      dist_num(x = vals_o, p = vals_p)
    } else if (inp$type[y] == "ordered") {
      dist_ordered(x = vals_o, p = vals_p)
    } else {
      dist_nom(x = vals_o, p = vals_p)
    }
  })
  
  # aggregate distances to compute final utility measure
  lambda <- sum(inp$dists) / (length(cn) * nrow(x))
  
  # individual contributions
  indiv_contr <- inp$dists / nrow(x)
  names(indiv_contr) <- inp$v
  
  attr(lambda, "indiv_distances") <- indiv_contr
  attr(lambda, "n") <- nrow(x)
  class(lambda) <- "il_variables"
  lambda
}

#' @method print il_variables
#' @rdname il_additional
#' @export
print.il_variables <- function(x, digits = 3, ...) {
  meta <- attributes(x)
  cat("Number of records: ",  meta$n ,"\n")
  cat("Number of variables: ",  length(meta$indiv_distances), "\n")
  cat("Overall information loss: ", round(x, digits = digits), "\n")
  cat("Individual information losses for variables:\n")
  print(data.frame(
    variable = names(meta$indiv_distances),
    loss = round(meta$indiv_distances, digits = digits)),
    row.names = FALSE
  )
  invisible(x)
}
