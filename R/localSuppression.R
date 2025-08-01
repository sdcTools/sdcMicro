#' Local Suppression to obtain k-anonymity
#'
#' Algorithm to achieve *k*-anonymity by performing local suppression.
#'
#' The algorithm provides a *k*-anonymized data set by suppressing values in key
#' variables. The algorithm tries to find an optimal solution to suppress as
#' few values as possible and considers the specified importance vector. If not
#' specified, the importance vector is constructed in a way such that key
#' variables with a high number of characteristics are considered less
#' important than key variables with a low number of characteristics.
#'
#' The implementation provides *k*-anonymity per strata, if slot `strataVar` has
#' been set in `sdcMicroObj-class` or if parameter `strataVar` is
#' used when applying the `data.frame` method. For details, see the examples provided.
#'
#' @rdname localSuppression
#' @param obj a `sdcMicroObj-class` object or a `data.frame`
#' @param k Threshold for *k*-anonymity
#' @param importance Numeric vector of values between 1 and *n* (`n = length(keyVars)`).
#' This vector defines the "importance" of variables for local suppression.
#' Variables with `importance = 1` will, if possible, not be suppressed;
#' variables with `importance = n` will be prioritized for suppression.
#' @param combs Numeric vector. If specified, the algorithm provides *k*-anonymity
#' for each combination of *n* key variables (with *n* being the value of the ith
#' element of this parameter). For example, `combs = c(4,3)` means that *k*-anonymity
#' will be provided for all combinations of 4 and then 3 key variables.
#' It is possible to assign different *k* values for each combination by supplying `k` as a vector.
#' If `k` has only one value, it will be used for all subsets.
#' @param ... see additional arguments below:
#' - **`keyVars`**: Names or indices of categorical key variables (for data.frame method)
#' - **`strataVars`**: Name or index of the variable used for stratification.
#'   *k*-anonymity is ensured within each category of this variable.
#' - **`alpha`**: Numeric value between 0 and 1 specifying how much keys with missing
#'   values (`NA`s) contribute to the calculation of `fk` and `Fk`.
#'   Default is `1`. Used only in the `data.frame` method.
#' - **`nc`**: Maximum number of cores used for stratified computations.
#'   Default is `1`. Parallelization is ignored on Windows.
#'
#' @details
#' For the parameter `alpha`:
#' - `alpha = 1` counts all *wildcard matches* (i.e. `NA`s match everything).
#' - `alpha = 0` assumes missing values form their own categories.
#'
#' These are two extremes. With `alpha = 0`, frequencies are likely underestimated when
#' `NA`s are present. If `combs` is used with `alpha = 0`, the heuristic nature of `kAnon()`
#' may lead to technically correct, but not always intuitively understandable frequency evaluations.
#'
#' @return
#' A modified dataset with suppressions that meets *k*-anonymity based on
#' the specified key variables, or the modified `sdcMicroObj-class` object.
#'
#' @author Bernhard Meindl, Matthias Templ
#'
#' @references
#' Templ, M. *Statistical Disclosure Control for Microdata: Methods and Applications in R.*
#' Springer International Publishing, 287 pages, 2017. ISBN: 978-3-319-50272-4.
#' \doi{10.1007/978-3-319-50272-4}
#'
#' Templ, M., Kowarik, A., Meindl, B.
#' *Statistical Disclosure Control for Micro-Data Using the R Package sdcMicro.*
#' Journal of Statistical Software, **67**(4), 1–36, 2015. \doi{10.18637/jss.v067.i04}
#'
#' @keywords manip
#' @export
#' @md
#' @note
#' Deprecated methods `localSupp2` and `localSupp2Wrapper` are no longer available
#' in `sdcMicro` versions > 4.5.0.
#' `kAnon()` is a more intuitive term for local suppression, since the goal is to achieve *k*-anonymity.
#'
#' @examples
#' \donttest{
#' data(francdat)
#'
#' ## Local Suppression
#' localS <- localSuppression(francdat, keyVar = c(4, 5, 6))
#' localS
#' plot(localS)
#'
#' ## for objects of class sdcMicro, no stratification
#' data(testdata2)
#' kv <- c("urbrur", "roof", "walls", "water", "electcon", "relat", "sex")
#' sdc <- createSdcObj(testdata2, keyVars = kv, w = "sampling_weight")
#' sdc <- localSuppression(sdc)
#'
#' ## for objects of class sdcMicro, with stratification
#' testdata2$ageG <- cut(testdata2$age, 5, labels = paste0("AG", 1:5))
#' sdc <- createSdcObj(
#'   dat = testdata2,
#'   keyVars = kv,
#'   w = "sampling_weight",
#'   strataVar = "ageG"
#' )
#' sdc <- localSuppression(sdc, nc = 1)
#'
#' ## it is also possible to provide k-anonymity for subsets of key-variables
#' ## with different parameter k!
#' ## in this case we want to provide 10-anonymity for all combinations
#' ## of 5 key variables, 20-anonymity for all combinations with 4 key variables
#' ## and 30-anonymity for all combinations of 3 key variables.
#' sdc <- createSdcObj(testdata2, keyVars = kv, w = "sampling_weight")
#' combs <- 5:3
#' k <- c(10, 20, 30)
#' sdc <- localSuppression(sdc, k = k, combs = combs)
#'
#' ## data.frame method (no stratification)
#' inp <- testdata2[, c(kv, "ageG")]
#' ls <- localSuppression(inp, keyVars = 1:7)
#' print(ls)
#' plot(ls)
#'
#' ## data.frame method (with stratification)
#' ls <- kAnon(inp, keyVars = 1:7, strataVars = 8)
#' print(ls)
#' plot(ls)
#' }
localSuppression <- function(obj, k = 2, importance = NULL, combs = NULL, ...) {
  localSuppressionX(
    obj = obj,
    k = k,
    importance = importance,
    combs = combs,
    ...
  )
}

setGeneric("localSuppressionX", function(obj, k = 2, importance = NULL, combs = NULL, ...) {
  standardGeneric("localSuppressionX")
})

setMethod(
  f = "localSuppressionX",
  signature = c("sdcMicroObj"),
  definition = function(obj, k = 2, importance = NULL, combs = NULL, nc = 1) {
    obj <- nextSdcObj(obj)
    ### get data from manipKeyVars
    df <- as.data.frame(get.sdcMicroObj(obj, type = "manipKeyVars"))
    strataVars <- get.sdcMicroObj(obj, "strataVar")
    keyVars <- 1:length(obj@keyVars)
    if (!is.null(strataVars)) {
      df <- cbind(df, get.sdcMicroObj(obj, type = "origData")[, strataVars, drop = FALSE])
      stratV <- length(keyVars) + 1:length(strataVars)
    } else {
      stratV <- NULL
    }

    alpha <- get.sdcMicroObj(obj, type = "options")$alpha
    if (alpha == 0) {
      warnings("alpha is set to 0. \nIn case of missing values in the key variables, \nfrequency counts may likely to be underestimated. \nWe recommend to increase the values of alpha")
    }
    ls <- localSuppressionWORK(
      x = df,
      keyVars = keyVars,
      strataVars = stratV,
      k = k,
      combs = combs,
      importance = importance,
      alpha = alpha,
      nc = nc
    )

    # create final output
    obj <- set.sdcMicroObj(obj, type = "manipKeyVars", input = list(ls$xAnon))
    ls$xAnon <- NULL
    class(ls) <- unclass("list")
    obj <- set.sdcMicroObj(obj, type = "localSuppression", input = list(ls))

    # transfer suppression patterns if ghostVars is specified
    ghostVars <- get.sdcMicroObj(obj, type = "ghostVars")
    if (!is.null(ghostVars)) {
      manipData <- get.sdcMicroObj(obj, type = "manipKeyVars")
      manipGhostVars <- get.sdcMicroObj(obj, type = "manipGhostVars")
      cn <- colnames(get.sdcMicroObj(obj, type = "origData"))
      for (i in seq_along(ghostVars)) {
        # index of keyVar within manipData
        kV <- match(cn[ghostVars[[i]][[1]]], colnames(manipData))
        isna <- is.na(manipData[[kV]])

        # get indices of linked variables within ghostVars and
        # transfer suppression pattern
        vv <- match(cn[ghostVars[[i]][[2]]], colnames(manipGhostVars))
        for (j in 1:length(vv)) {
          manipGhostVars[[vv[j]]][isna] <- NA
        }
      }
      obj <- set.sdcMicroObj(obj, type = "manipGhostVars", input = list(manipGhostVars))
    }
    obj <- calcRisks(obj)
    obj
  }
)

setMethod(
  f = "localSuppressionX",
  signature = c("data.frame"),
  definition = function(obj,
                        k = 2,
                        importance = NULL,
                        combs = NULL,
                        keyVars,
                        strataVars = NULL,
                        alpha = 1,
                        nc = 1) {
    if (alpha == 0) {
      warnings("alpha is set to 0. \nIn case of missing values in the key variables, \nfrequency counts may likely to be underestimated. \nWe recommend to increase the values of alpha")
    }
    localSuppressionWORK(
      x = obj,
      keyVars = keyVars,
      k = k,
      strataVars = strataVars,
      importance = importance,
      combs = combs,
      alpha = alpha,
      nc = nc
    )
  }
)

suppSubset <- function(x, k, importance, alpha) {
  # checks
  if (length(k) != 1 | k < 1) {
    stop("argument 'k' must be of length 1 and > 0.", call. = FALSE)
  }
  if (!is.null(importance)) {
    if (length(importance) != ncol(x)) {
      stop("length of importance-vector does not match number of key variables!", call. = FALSE)
    }
  }

  # k can be at most the number of obs!
  if (nrow(x) < k) {
    stop("k is larger or equal the group size (in at least one stratum).", call. = FALSE)
  }
  k <- min(k, nrow(x))

  keyVars <- names(x)
  x <- data.table(x)

  # calculate number of suppressions for each keyVar
  # before trying to achieve k-anonymity
  NAinKey <- x[, lapply(.SD, function(x) sum(is.na(x)))]

  x[, idvarextraforsls := .I]

  ##############
  # The dataset is reduced to a smaller dataset in the following way
  # 1) all NAs are initialized with the first unique value
  # of the corresponding keyVariable
  xKeys <- x[, c(keyVars, "idvarextraforsls"), with = FALSE]
  for (kV in keyVars) {
    if (NAinKey[[kV]] > 0) {
      e1 <- parse(text = paste0("is.na(", kV, ")"))
      e2 <- parse(text = paste0(kV, ":=unique(", kV, ")[1]"))
      xKeys[eval(e1), eval(e2)]
    }
  }
  setkeyv(xKeys, keyVars)

  # 2) fk is computed
  erg <- xKeys[, list(fk = .N), by = key(xKeys)]
  xKeys <- merge(xKeys, erg)

  # 3) from groups with fk > k, all observations except k observations are removed
  weg <- fkd <- idvarextraforsls <- fk <- NA # for CHECK-NOTES
  erg <- xKeys[fk > k] # more than k
  erg[, fkd := fk - k]
  if (nrow(erg) > 0) {
    erg2 <- erg[, utils::tail(.SD, fkd[1]), by = key(erg)]
    xKeys <- data.table(x)
    setkey(xKeys, "idvarextraforsls")
    erg2 <- erg2[, list(idvarextraforsls)]
    erg2[, weg := 1]
    setkey(erg2, "idvarextraforsls")
    xKeys <- merge(xKeys, erg2, all = TRUE)
    x <- xKeys[is.na(weg)]
  }

  # 4) afterwards the old lS-Algo is applied
  ff <- freqCalc(x, keyVars = keyVars, alpha = alpha)
  rk <- indivRisk(ff)
  runInd <- TRUE

  importanceI <- (length(importance) + 1) - importance

  # prepare data input for cpp_calcSuppInds()
  # factors must be recoded as numeric
  mat <- x[, keyVars, with = FALSE]
  for (kV in names(mat)) {
    if (is.factor(mat[[kV]])) {
      ex <- parse(text = paste0(kV, ":=as.numeric(", kV, ")"))
      mat[, eval(ex)]
    }
  }
  mat <- as.matrix(mat)
  while (runInd) {
    ind.problem <- which(ff$fk < k)
    ind.problem <- ind.problem[order(rk$rk[ind.problem], decreasing = TRUE)]
    for (i in seq_along(ind.problem)) {
      params <- list(alpha = alpha, id = as.integer(ind.problem[i]))
      res <- cpp_calcSuppInds(mat, mat[ind.problem[i], ], params = params)
      ind <- res$ids
      if (length(ind) > 0) {
        colInd <- NULL
        colIndsSorted <- keyVars[order(importanceI)]
        while (is.null(colInd)) {
          for (cc in colIndsSorted) {
            # special case where we have to suppress values in the problematic instance itself
            # because no other candidates are available
            if (length(ind) == 1 && ind == ind.problem[i]) {
              z <- which(!is.na(mat[ind, cc]))
            } else {
              z <- which(mat[ind.problem[i], cc] != mat[ind, cc] & !is.na(mat[ind, cc]))
            }
            if (length(z) > 0) {
              colInd <- cc
              break
            }
          }
        }
        x[[colInd]][ind.problem[i]] <- NA
        mat[ind.problem[i], colInd] <- NA # required for cpp_calcSuppInds()
      } else {
        stop("Error the length of the index vector is 0.", call. = FALSE)
      }
    }
    ff <- freqCalc(x, keyVars = keyVars, alpha = alpha)
    rk <- indivRisk(ff)
    if (all(ff$fk >= k)) {
      runInd <- FALSE
    }
  }
  # 5) the last step is to merge the smaller k-anonymized data set back to the
  # original data set with initial NAs introduced again
  if (nrow(erg) > 0) {
    xrem <- data.table(x[, "idvarextraforsls", with = F], weg = 1)
    x[, weg := NULL]

    setkey(xrem, "idvarextraforsls")
    xKeys[, weg := NULL]
    setkey(xKeys, "idvarextraforsls")
    xKeys <- merge(xKeys, xrem, all = TRUE)
    xKeys <- xKeys[is.na(weg), ]
    xKeys[, weg := NULL]
    x <- rbind(x, xKeys)
    setkey(x, "idvarextraforsls")
    x[, idvarextraforsls := NULL]
  } else {
    setkey(x, "idvarextraforsls")
    x[, idvarextraforsls := NULL]
  }
  out <- list(xAnon = x) # , supps=supps, totalSupps=totalSupps)
  return(out)
}
sum_na <- function(x) {
  sum(is.na(x))
}
localSuppressionWORK <- function(x, keyVars, strataVars, k = 2, combs, importance = NULL, alpha, nc = 1) {
  # find a suppression pattern for a simple subset that is not stratified
  # input: df=data.table with only keyVars
  # k: parameter for k-anonymity (length 1)
  # importance: importance-vector with length equals ncol(df)
  strata <- NULL
  if (!"data.table" %in% class(x)) {
    x <- as.data.table(x)
  }
  if (is.numeric(keyVars)) {
    keyVarsNum <- keyVars
    keyVars <- names(x)[keyVars]
  } else {
    keyVarsNum <- match(keyVars, colnames(x))
  }
  if (is.numeric(strataVars)) {
    strataVarsNum <- strataVars
    strataVars <- names(x)[strataVars]
  } else {
    strataVarsNum <- match(strataVars, colnames(x))
  }

  stopifnot(is.numeric(nc))
  nc <- round(nc)[1]
  stopifnot(nc >= 1)
  # parallel::mclapply does only work reliably under linux/mac
  # and it is not worth bothering dealing with windows here
  if (!tolower(Sys.info()["sysname"]) %in% c("linux", "darwin")) {
    nc <- 1
  } else {
    nc <- max(1, min(nc, parallel::detectCores() - 1))
  }

  # checks and preparations if we apply localSuppression on
  # subsets of key variables
  if (!is.null(combs)) {
    combs <- as.integer(combs)
    if (length(combs) != length(k)) {
      # using the same k!
      k <- rep(k, length(combs))
    }
    if (!all(combs > 0)) {
      stop("each element of 'comb' must be > 0!", call. = FALSE)
    }
    if (any(combs > length(keyVars))) {
      stop("at least one element of 'combs' is to large!", call. = FALSE)
    }

    # all combinations we need to tackle are stored here
    tree <- lapply(combs, function(x) {
      utils::combn(keyVarsNum, x)
    })
  }

  # calculate importance if specified
  if (is.null(importance)) {
    xx <- x[, lapply(.SD, function(y) {
      length(table(y))
    }), .SDcols = keyVars]
    importance <- match(names(xx), names(
      sort(unlist(xx), decreasing = FALSE)
    ))
  } else {
    if (length(setdiff(sort(importance), 1:length(keyVars))) > 0) {
      e <- "The importance vector needs to be discrete numbers between 1 and the number of key variables."
      stop(e, call. = FALSE)
    }
  }

  # calculate number of suppressions for each keyVar
  # before trying to achieve k-anonymity
  NABefore <- is.na(x)
  if (is.null(strataVars)) {
    NAinKey <- x[, lapply(.SD, sum_na), .SDcols = keyVars, by = strataVars]
  } else {
    x[, strata := apply(x[, strataVars, with = F], 1, paste, collapse = "-")]
    NAinKey <- x[, lapply(.SD, sum_na), .SDcols = keyVars, by = "strata"]
    NAinKey_tot <- x[, lapply(.SD, sum_na), .SDcols = keyVars]
    NAinKey_tot[, strata := "Total"]
    NAinKey_tot <- NAinKey_tot[, c("strata", keyVars), with = F]
    NAinKey <- rbind(NAinKey, NAinKey_tot)
    x[, strata := NULL]
  }

  # performing the k-Anon algorithm
  # no stratification required
  if (is.null(strataVars)) {
    if (is.null(combs)) {
      inpDat <- x[, keyVars, with = FALSE]
      res <- suppSubset(
        x = inpDat,
        k = k,
        importance = importance,
        alpha = alpha
      )
      xAnon <- res$xAnon
    } else {
      # no strata but subsets of key variables (combs)
      counter <- 0
      tmpDat <- copy(x)
      for (gr in seq_along(tree)) {
        cur_k <- k[gr]
        # log <- paste0("providing ",cur_k,"-Anonymity for ",ncol(tree[[gr]])," combinations ")
        # log <- paste0(log, "of ",combs[gr]," key variables.\n")
        # message(log)
        for (comb in 1:ncol(tree[[gr]])) {
          # message("combination",comb,"|",ncol(tree[[gr]]),"\n")
          counter <- counter + 1
          kV <- tree[[gr]][, comb]
          cur_importance <- rank(importance[kV], ties.method = "min")
          inpDat <- tmpDat[, kV, with = F]
          res <- suppSubset(
            x = inpDat,
            k = cur_k,
            importance = cur_importance,
            alpha = alpha
          )

          # replace: is there a more elegant way?
          for (z in 1:length(kV)) {
            set(tmpDat, i = NULL, j = kV[z], res$xAnon[[z]])
          }
        }
      }
      # prepare output
      xAnon <- tmpDat
    }
  } else {
    ## we want k-anonymity in each strata!
    inpDat <- x[, keyVars, with = FALSE]
    inpDat[, strata := apply(x[, strataVars, with = FALSE], 1, paste, collapse = "-")]
    inpDat[, sortid := 1:nrow(inpDat)]
    spl <- split(inpDat, inpDat$strata)

    # to be able to sort back later!
    sortid <- as.numeric(unlist(sapply(spl, function(x) {
      x[["sortid"]]
    })))

    supps <- xAnon <- vector("list", length = length(spl))
    totalSupps <- rep(NA, length(spl))
    if (is.null(combs)) {
      if (nc == 1) {
        # message("running serially")
        xAnon <- lapply(seq_len(length(spl)), function(x) {
          suppSubset(
            x = spl[[x]][, keyVars, with = FALSE],
            k = k,
            importance = importance,
            alpha = alpha
          )$xAnon
        })
      } else {
        # message("running in parallel using ", nc, " cores")
        xAnon <- parallel::mclapply(seq_len(length(spl)), function(x) {
          suppSubset(
            x = spl[[x]][, keyVars, with = FALSE],
            k = k,
            importance = importance,
            alpha = alpha
          )$xAnon
        }, mc.cores = nc)
      }
    } else {
      # local Suppression by strata and combination of subsets!
      for (i in seq_along(spl)) {
        counter <- 0
        tmpDat <- copy(spl[[i]])
        for (gr in seq_along(tree)) {
          cur_k <- k[gr]
          # log <- paste0("providing ",cur_k,"-Anonymity for ",ncol(tree[[gr]])," combinations ")
          # log <- paste0(log, "of ",combs[gr]," key variables in strata ", names(spl)[i],"!\n")
          # message(log)
          for (comb in 1:ncol(tree[[gr]])) {
            counter <- counter + 1
            kV <- tree[[gr]][, comb]
            cur_importance <- rank(importance[kV], ties.method = "min")
            inpDat <- tmpDat[, kV, with = FALSE]
            res <- suppSubset(
              x = inpDat,
              k = cur_k,
              importance = cur_importance,
              alpha = alpha
            )

            # replace: is there a more elegant way?
            for (z in 1:length(kV)) {
              set(tmpDat, i = NULL, j = kV[z], res$xAnon[[z]])
            }
          }
        }
        # prepare output
        tmpDat[, sortid := NULL]
        xAnon[[i]] <- tmpDat
      }
    }
    xAnon <- rbindlist(xAnon)
    xAnon[, sortid := sortid]
    setkey(xAnon, sortid)
    xAnon[, sortid := NULL]
  }

  ## compute number of suppressions
  if (is.null(strataVars)) {
    totalSupps <- xAnon[, lapply(.SD, sum_na), .SDcols = keyVars]
    supps <- totalSupps - NAinKey
    totalSupps <- as.data.frame(totalSupps)
    supps <- as.data.frame(supps)
  } else {
    if (is.null(combs)) {
      xAnon[, strata := inpDat[, strata]]
    }
    totalSupps <- xAnon[, lapply(.SD, sum_na), .SDcols = keyVars, by = "strata"]
    totalSupps_tot <- xAnon[, lapply(.SD, sum_na), .SDcols = keyVars]
    totalSupps_tot[, strata := "Total"]
    totalSupps_tot <- totalSupps_tot[, c("strata", keyVars), with = FALSE]
    totalSupps <- rbind(totalSupps, totalSupps_tot)
    xAnon[, strata := NULL]

    supps <- copy(totalSupps)
    supps[, c(keyVars) := totalSupps[, keyVars, with = FALSE] - NAinKey[, keyVars, with = FALSE]]

    supps <- as.data.frame(supps)
    rownames(supps) <- supps$strata
    supps$strata <- NULL
    totalSupps <- as.data.frame(totalSupps)
    rownames(totalSupps) <- totalSupps$strata
    totalSupps$strata <- NULL
  }

  newSupps <- utils::tail(rowSums(supps), 1)
  res <- list(
    xAnon = as.data.frame(xAnon),
    supps = supps,
    totalSupps = totalSupps,
    newSupps = newSupps,
    anonymity = TRUE,
    keyVars = keyVars,
    strataVars = strataVars,
    importance = importance,
    k = k,
    threshold = NA,
    combs = combs
  )
  class(res) <- "localSuppression"
  invisible(res)
}

#' Print method for objects from class localSuppression
#'
#' @param x object from class localSuppression
#' @param \dots Additional arguments passed through.
#' @return Information about the frequency counts for key variables for object
#' of class \sQuote{localSuppression}.
#' @author Matthias Templ
#' @seealso \code{\link{localSuppression}}
#' @keywords print
#' @method print localSuppression
#' @export
#' @examples
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' l1 <- localSuppression(francdat, keyVars = c(2, 4, 5, 6))
#' l1
#'
print.localSuppression <- function(x, ...) {
  byStrata <- ifelse(nrow(x$totalSupps) == 1, FALSE, TRUE)
  totSupps <- utils::tail(rowSums(x$totalSupps), 1)
  addSupps <- utils::tail(rowSums(x$supps), 1)

  pp <- "\n-----------------------\n"
  pp <- paste0(pp, "Total number of suppressions in the key variables: ", totSupps, " (new: ", addSupps, ")\n\n")
  if (!is.na(x$threshold)) {
    pp <- paste0(pp, "Number of suppressions by key variables:\n\n")
    message(pp)
    print(x$supps)

    kv <- x$keyVars[which.max(x$importance)]
    pp <- paste0("\nThe last application of localSupp() resulted")
    pp <- paste0(pp, " in ", x$newSupps, " additional suppressions in variable ", dQuote(kv), ".\n")
    pp <- paste0(pp, "These observations had individual risks >= ", x$threshold, ".\n")
    message(pp)
    return(invisible(NULL))
  }

  if (byStrata) {
    pp <- paste0(pp, "Number of suppressions by key variables and strata:\n(in parenthesis, the total number suppressions is shown)\n\n")
  } else {
    pp <- paste0(pp, "Number of suppressions by key variables:\n(in parenthesis, the total number suppressions is shown)\n\n")
  }

  dt <- x$supps
  for (i in 1:ncol(dt)) {
    dt[[i]] <- paste0(dt[[i]], " (", x$totalSupps[[i]], ")")
  }
  message(pp)
  print(dt)

  if (byStrata == TRUE) {
    if (is.null(x$combs)) {
      if (all(x$anonymity)) {
        pp <- paste0("\n", x$k, "-anonymity == TRUE in all strata!\n")
      } else {
        prob <- rownames(x$supps)[which(!x$anonymity)]
        pp <- paste0("\n", x$k, "-anonymity == FALSE in the following strata:\n")
        pp <- paste0(pp, paste0(rownames(x$supps)[which(!x$anonymity)], collapse = ", "))
      }
    } else {
      pp <- "\nk-anonymity has been achieved within strata and for combinations of key-variables!"
      for (i in 1:length(x$combs)) {
        pp <- paste(pp, paste0("\n", x$k[i], "-anonymity == ", all(x$anonymity), " for all ", x$combs[i], "-dimensional subsets of key variables within stratas"))
      }
      pp <- paste0(pp, "\n")
    }
  } else {
    if (!is.null(x$combs)) {
      pp <- NULL
      for (i in 1:length(x$combs)) {
        pp <- paste(pp, paste0("\n", x$k[i], "-anonymity == ", all(x$anonymity), " for all ", x$combs[i], "-dimensional subsets of key variables"))
      }
      pp <- paste(pp, "\n")
    } else {
      pp <- paste0("\n", x$k, "-anonymity == ", all(x$anonymity), "\n")
    }
  }
  pp <- paste0(pp, "-----------------------\n")
  message(pp)
  invisible(NULL)
}

#' Plots for localSuppression objects
#'
#' This function creates barplots to display the number of suppressed values
#' in categorical key variables to achieve `k`-anonymity.
#'
#' @param x object of derived from [localSuppression()]
#' @param ... Additional arguments, currently available are:
#' - `"showDetails"`: logical, if set, a plot of suppressions by
#' strata is shown (if possible)
#' @author Bernhard Meindl, Matthias Templ
#' @md
#' @seealso [localSuppression()]
#' @keywords plot
#' @method plot localSuppression
#' @export
#' @return a `ggplot` plot object
#' @examples
#' data(francdat)
#' @export plot.localSuppression
plot.localSuppression <- function(x, ...) {
  vals <- NULL
  byStrata <- !is.null(x$strataVars)
  params <- list(...)
  inp <- x$supps

  showDetails <- FALSE
  if (byStrata & !is.null(params$showDetails)) {
    if (params$showDetails) {
      showDetails <- TRUE
    }
  }

  # add overall suppressions if localSuppression was applied per strata
  if (!showDetails) {
    inp <- as.data.frame(t(apply(inp, 2, sum)))
    rownames(inp) <- "Overall"
  }

  keyVar <- rep(x$keyVars, nrow(inp))
  if (any(nchar(keyVar) >= 12)) {
    warnMsg <- "Too long variable names are cutted!\n"
    warning(warnMsg)
    keyVar <- substr(keyVar, 1, 12)
  }

  # create ggplot2-input
  df <- data.frame(
    keyVar = keyVar,
    strata = rep(rownames(inp), each = ncol(inp)),
    vals = as.vector(as.matrix(t(inp)))
  )

  # barplot
  p <- ggplot(df, aes(x = keyVar, y = vals, fill = "darkgrey"))
  p <- p + geom_bar(color = "black", stat = "identity")
  p <- p + geom_text(aes(y = vals * 1.015, label = vals, vjust = 0), size = 3)
  if (nrow(inp) > 1) {
    p <- p + facet_wrap(~strata)
    p <- p + ggtitle("Number of suppressions to achieve k-anonymity by strata")
  } else {
    p <- p + ggtitle("Number of suppressions to achieve k-anonymity.")
  }
  p <- p + labs(x = "Key variables", y = "Number of suppressions") + theme(legend.position = "none")
  p <- p + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  p
}

#' @rdname localSuppression
#' @export
kAnon <- function(obj, k = 2, importance = NULL, combs = NULL, ...) {
  localSuppression(
    obj = obj,
    k = k,
    importance = importance,
    combs = combs,
    ...
  )
}
