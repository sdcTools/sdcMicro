#' Local Suppression to obtain k-anonymity
#'
#' Algorithm to achieve k-anonymity by performing local suppression.
#'
#' The algorithm provides a k-anonymized data set by suppressing values in key
#' variables. The algorithm tries to find an optimal solution to suppress as
#' few values as possible and considers the specified importance vector. If not
#' specified, the importance vector is constructed in a way such that key
#' variables with a high number of characteristics are considered less
#' important than key variables with a low number of characteristics.
#'
#' The implementation provides k-anonymity per strata, if slot 'strataVar' has
#' been set in \code{\link{sdcMicroObj-class}} or if parameter 'strataVar' is
#' used when appying the data.frame- or matrix method. For details, have a look
#' at the examples provided.
#'
#' @name localSuppression
#' @aliases localSuppression-methods localSuppression,data.frame-method
#' localSuppression,matrix-method localSuppression,sdcMicroObj-method
#' localSuppression
#' @docType methods
#' @param obj an object of class sdcMicroObj or a data frame or matrix
#' @param k threshold for k-anonymity
#' @param importance numeric vector of numbers between 1 and n (n=length of
#' vector keyVars).  This vector represents the "importance" of variables that
#' should be used for local suppression in order to obtain k-anonymity.
#' key-variables with importance=1 will - if possible - not suppressed,
#' key-variables with importance=n will be used whenever possible.
#' @param ... see arguments below
#' \itemize{
#' \item{keyVars}{numeric vector specifying indices of (categorical) key-variables}
#' \item{strataVars}{numeric vector specifying indices of variables that should be used
#' for stratification within 'obj'}}
#' @return Manipulated data set with suppressions that has k-anonymity with
#' respect to specified key-variables or the manipulated data stored in the
#' \code{\link{sdcMicroObj-class}}.
#' @section Methods: \describe{
#' \item{list("signature(obj = \"data.frame\")")}{}
#' \item{list("signature(obj = \"matrix\")")}{}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @author Bernhard Meindl, Matthias Templ
#' @keywords manip
#' @export
#' @note Deprecated methods 'localSupp2' and 'localSupp2Wrapper' are no longer available
#' in sdcMicro > 4.5.0
#' @examples
#'
#' data(francdat)
#' ## Local Suppression
#' localS <- localSuppression(francdat, keyVar=c(4,5,6))
#' localS
#' plot(localS)
#'
#' ## for objects of class sdcMicro, no stratification
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- localSuppression(sdc)
#'
#' ## for objects of class sdcMicro, no with stratification
#' testdata2$ageG <- cut(testdata2$age, 5, labels=paste0("AG",1:5))
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight',
#'   strataVar='ageG')
#' sdc <- localSuppression(sdc)
#'
#' ## data.frame method (no stratification)
#' keyVars <- c("urbrur","roof","walls","water","electcon","relat","sex")
#' strataVars <- c("ageG")
#' inp <- testdata2[,c(keyVars, strataVars)]
#' ls <- localSuppression(inp, keyVars=1:7)
#' print(ls)
#' plot(ls)
#'
#' ## data.frame method (with stratification)
#' ls <- localSuppression(inp, keyVars=1:7, strataVars=8)
#' print(ls)
#' plot(ls, showTotalSupps=TRUE)
#'
setGeneric("localSuppression", function(obj, k = 2, importance = NULL, ...) {
  standardGeneric("localSuppression")
})

setMethod(f='localSuppression', signature=c('sdcMicroObj'),
definition=function(obj, k=2, importance=NULL, ...) {
  ### get data from manipKeyVars
  inp <- as.data.frame(get.sdcMicroObj(obj, type="manipKeyVars"))

  strataVars <- get.sdcMicroObj(obj, "strataVar")
  keyVars <- 1:length(obj@keyVars)

  if ( is.null(strataVars) ) {
    ls <- localSuppression(obj=inp, k=k, importance=importance, keyVars=keyVars, strataVars=NULL)
  } else {
    ## we want k-anonymity in each strata!
    strat <- get.sdcMicroObj(obj, type="origData")[,strataVars,drop=F]
    stratV <- (ncol(inp)+1):(ncol(inp)+ncol(strat))
    inp <- cbind(inp, strat)
    ls <- localSuppression(obj=inp, k=k, importance=importance, keyVars=keyVars,
      strataVars=stratV) # data.frame-method calls localSuppressionWORK()!
  }

  # create final output
  obj <- nextSdcObj(obj)
  obj <- set.sdcMicroObj(obj, type="manipKeyVars", input=list(ls$xAnon))

  a <- get.sdcMicroObj(obj, type="origData")[,get.sdcMicroObj(obj, type="keyVars")]
  b <- ls$xAnon
  w = which(is.na(a))
  r = w%%nrow(a)
  cc = ceiling(w/nrow(a))
  wb = which(is.na(b))
  rb = wb%%nrow(b)
  cb = ceiling(wb/nrow(b))
  d = data.frame(id=1:ncol(a), before=NA, after=NA)
  d[,2:3] <- t(sapply(1:ncol(a), function (x) c(sum(cc==x), sum(cb==x))))
  obj <- set.sdcMicroObj(obj, type="localSuppression", input=list(list(d$after-d$before)))
  obj <- calcRisks(obj)
  obj
})

setMethod(f='localSuppression', signature=c("data.frame"),
definition=function(obj, k=2, keyVars, importance=NULL, strataVars=NULL) {
  if ( !is.null(strataVars) ) {
    ## we want k-anonymity in each strata!
    inp <- data.table(obj[,keyVars])
    inp[,strata:=apply(obj[,strataVars,drop=F],1,paste,collapse="-")]
    inp[,sortidforls:=1:nrow(inp)]
    spl <- split(inp, inp$strata)
    ls <- list(); length(ls) <- length(spl)
    supps <- list(); length(supps) <- length(spl)
    xAnon <- list(); length(xAnon) <- length(spl)
    anon <- rep(NA, length(spl))
    # todo: using parallel/mclapply?
    for ( i in seq_along(spl) ) {
      x <- spl[[i]]
      ls[[i]] <- localSuppressionWORK(x, k=k, importance=importance, keyVars=keyVars)
      supps[[i]] <- ls[[i]]$supps
      xAnon[[i]] <- ls[[i]]$xAnon
      anon[i] <- ls[[i]]$anonymity
    }
    supps <- as.data.frame(rbindlist(supps))
    rownames(supps) <- names(spl)
    xAnon <- rbindlist(xAnon)
    setkey(xAnon, sortidforls)
    xAnon[,sortidforls:=NULL]
    xAnon[,strata:=NULL]

    # combine results!
    ls <- ls[[1]]
    ls$xAnon <- as.data.frame(xAnon)
    ls$supps <- supps
    ls$totalSupps <- sum(supps)
    ls$anonymity <- anon
    ls$strataVars <- strataVars
  } else {
    inp <- data.table(obj[,keyVars])
    inp[,strata:=1]
    inp[,sortidforls:=1:nrow(inp)]
    ls <- localSuppressionWORK(x=inp, keyVars=1:length(keyVars), k=k, importance=importance)
    ls$xAnon <- ls$xAnon[,-match(c("strata","sortidforls"), names(ls$xAnon)),drop=F]
    obj[,keyVars] <- ls$xAnon
    ls$xAnon <- obj
  }
  ls
})

setMethod(f='localSuppression', signature=c("matrix"),
definition=function(obj, k=2, importance=NULL, keyVars, strataVars=NULL) {
  localSuppression(as.data.frame(obj), k=k, importance=importance, keyVars=keyVars, strataVars=strataVars)
})

localSuppressionWORK <- function(x, keyVars, k=2, importance=NULL) {
  if ( !"data.table" %in% class(x) ) {
    x <- data.table(x)
  } else {
    x <- copy(x)
  }
  if ( is.numeric(keyVars) ) {
    keyVars <- names(x)[keyVars]
  }

  if ( is.null(importance) ) {
    xx <- x[,lapply(.SD, function(x) length(table(x))), .SDcols=keyVars]
    #importance <- as.numeric(rank(xx, ties.method="first"))
    importance <- match(xx, sort(xx, decreasing = FALSE))
  } else {
    if ( length(setdiff(sort(importance), 1:length(keyVars))) > 0 ) {
      stop("importance vector needs to be discrete numbers between 1 and the number of key-variables!\n")
    }
  }

  # calculate number of suppressions for each keyVar
  # before trying to achieve k-anonymity
  NABefore <- is.na(x)
  NAinKey <- x[,lapply(.SD, function(x) sum(is.na(x))), .SDcols=keyVars]
  totalNABefore <- sum(NAinKey)

  ##############
  # The dataset is reduced to a smaller dataset in the following way
  # 1) all NAs are initialized with the first unique value
  # of the corresponding keyVariable
  x[,idvarextraforsls:=.I]
  xKeys <- x[,c(keyVars,"sortidforls","idvarextraforsls"),with=F]
  for ( kV in keyVars ) {
    if (NAinKey[[kV]] > 0 ) {
      e1 <- parse(text=paste0("is.na(",kV,")"))
      e2 <- parse(text=paste0(kV,":=unique(",kV,")[1]"))
      xKeys[eval(e1), eval(e2)]
    }
  }
  setkeyv(xKeys, keyVars)

  # 2) fk is computed
  erg <- xKeys[,list(fk=.N), by=key(xKeys)]
  xKeys <- merge(xKeys, erg)

  # 3) from groups with fk>k, all observations except k observations are removed
  weg <- fkd <- idvarextraforsls <- fk <- NA #for CHECK-NOTES
  erg <- xKeys[fk>k] # more than k
  erg[,fkd:=fk-k]
  if ( nrow(erg) >0 ) {
    erg2 <- erg[,tail(.SD,fkd[1]),by=key(erg)]
    xKeys <- data.table(x)
    setkey(xKeys,"idvarextraforsls")
    erg2 <- erg2[,list(idvarextraforsls)]
    erg2[,weg:=1]
    setkey(erg2,"idvarextraforsls")
    xKeys <- merge(xKeys,erg2,all=TRUE)
    x <- xKeys[is.na(weg)]
  }

  # 4) afterwards the old lS-Algo is applied
  ff <- freqCalc(x, keyVars=keyVars)
  rk <- indivRisk(ff)
  runInd <- TRUE

  importanceI <- (length(importance)+1)-importance

  # prepare data input for cpp_calcSuppInds()
  # factors must be recoded as numeric
  mat <- x[,keyVars,with=F]
  for ( kV in names(mat) ) {
    if ( is.factor(mat[[kV]]) ) {
      ex <- parse(text=paste0(kV,":=as.numeric(",kV,")"))
      mat[,eval(ex)]
    }
  }
  mat <- as.matrix(mat)
  while ( runInd ) {
    ind.problem <- which(ff$fk < k)
    ind.problem  <- ind.problem[order(rk$rk[ind.problem],decreasing=TRUE)]
    for ( i in 1:length(ind.problem) ) {
      res <- cpp_calcSuppInds(mat, mat[ind.problem[i],])
      if ( res$fk >= k ) {
        break;
      }
      ind <- res$ids
      if ( length(ind) > 0 ) {
        colInd <- NULL
        colIndsSorted <- keyVars[order(importanceI)]
        while ( is.null(colInd) ) {
          for ( cc in colIndsSorted ) {
            z <- which(mat[ind.problem[i],cc]!=mat[ind,cc] & !is.na(mat[ind,cc]))
            if ( length(z) > 0 ) {
              colInd <- cc
              break;
            }
          }
        }
        x[[colInd]][ind.problem[i]] <- NA
        mat[ind.problem[i], colInd] <- NA # required for cpp_calcSuppInds()
      } else {
        stop("Error\n")
      }
    }
    ff <- freqCalc(x, keyVars=keyVars)
    rk <- indivRisk(ff)
    if ( all(ff$fk >= k) ) {
      runInd <- FALSE
    }
  }

  # 5) the last step is to merge the smaller k-anonymized data set back to the
  # original data set with initial NAs introduced again
  if ( nrow(erg)>0 ) {
    xrem <- data.table(x[,"idvarextraforsls",with=F],weg=1)
    x[,weg:=NULL]

    setkey(xrem,"idvarextraforsls")
    xKeys[,weg:=NULL]
    setkey(xKeys,"idvarextraforsls")
    xKeys <- merge(xKeys,xrem,all=TRUE)
    xKeys <- xKeys[is.na(weg),]
    xKeys[,weg:=NULL]
    x <- rbind(x,xKeys)
    setkey(x,"idvarextraforsls")
    x[,idvarextraforsls:=NULL]
    x <- as.data.frame(x)
    if( any(NABefore) ) {
      x[NABefore] <- NA
    }
  } else {
    setkey(x,"idvarextraforsls")
    x[,idvarextraforsls:=NULL]
  }

  ## preparing the output:
  x <- as.data.frame(x)
  totalNA <- length(which(is.na(x[,keyVars])))
  supps <- apply(x[, keyVars], 2, function(x) {
    length(which(is.na(x)))
  })-NAinKey

  res <- list(xAnon=as.data.frame(x), supps=as.data.frame(supps),
    totalSupps=totalNA-totalNABefore, anonymity=TRUE, keyVars=keyVars,
    strataVars=NULL, importance=importance, k=k)
  class(res) <- "localSuppression"
  invisible(res)
}

#' Print method for objects from class localSuppression
#'
#' Print method for objects from class localSuppression.
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
#'
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' l1 <- localSuppression(francdat, keyVars=c(2,4,5,6))
#' l1
#'
print.localSuppression <- function(x, ...) {
  byStrata <- !is.null(x$strataVars)
  pp <- "\n-----------------------\n"
  pp <- paste0(pp, "Total Suppressions in the key variables: ", x$totalSupps,"\n\n")

  if ( byStrata ) {
    pp <- paste0(pp, "Number of suppressions by key-variables and strata:\n\n")
  } else {
    pp <- paste0(pp, "Number of suppressions by key-variables:\n\n")
  }
  cat(pp)

  if ( byStrata ) {
    print(rbind(x$supps, Total=colSums(x$supps)))
  } else {
    print(x$supps)
  }

  if ( byStrata==TRUE ) {
    if ( all(x$anonymity) ) {
      pp <- paste0("\n", x$k, "-anonymity == TRUE in all strata!\n")
    } else {
      prob <- rownames(x$supps)[which(!x$anonymity)]
      pp <- paste0("\n", x$k, "-anonymity == FALSE in the following strata:\n")
      pp <- paste0(pp, paste0(rownames(x$supps)[which(!x$anonymity)], collapse=", "))
    }
  } else {
    pp <- paste0("\n", x$k, "-anonymity == ", all(x$anonymity),"\n")
  }
  pp <- paste0(pp, "-----------------------\n")
  cat(pp)
}

#' plot method for localSuppression objects
#'
#' Barplot for objects from class localSuppression.
#'
#' Just look at the resulting plot.
#'
#' @param x object of class \sQuote{localSuppression}
#' @param \dots Additional arguments, currently available are:
#' \itemize{
#' \item showDetails logical, if setn a plot of suppressions by
#' strata is shown (if possible)
#' }
#' @author Bernhard Meindl, Matthias Templ
#' @seealso \code{\link{localSuppression}}
#' @keywords aplot
#' @method plot localSuppression
#' @export
#' @examples
#'
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' l1 <- localSuppression(francdat, keyVars=c(2,4,5,6))
#' l1
#' plot(l1)
#'
#' ## with details of suppression by strata
#' data(testdata2)
#' testdata2$ageG <- cut(testdata2$age, 5, labels=paste0("AG",1:5))
#' keyVars <- c("urbrur","roof","walls","water","electcon","relat","sex")
#' strataVars <- c("ageG")
#' inp <- testdata2[,c(keyVars, strataVars)]
#' ls <- localSuppression(inp, keyVars=1:7, strataVars=8)
#' print(ls)
#' plot(ls)
#' plot(ls, showDetails=TRUE)
#'
#' @export plot.localSuppression
plot.localSuppression <- function(x, ...) {
  vals <- keyVars <- NULL
  byStrata <- !is.null(x$strataVars)
  params <- list(...)
  inp <- x$supps

  showDetails <- FALSE
  if ( byStrata & !is.null(params$showDetails) ) {
    if ( params$showDetails ) {
      showDetails <- TRUE
    }
  }

  # add overall suppressions if localSuppression was applied per strata
  if ( !showDetails ) {
    inp <- as.data.frame(t(apply(inp, 2, sum)))
    rownames(inp) <- "Overall"
  }

  keyVar <- rep(x$keyVars, nrow(inp))
  if (any(nchar(keyVar) >= 12)) {
    warning("Too long variable names are cutted!\n")
    keyVar <- substr(keyVar, 1, 12)
  }

  # create ggplot2-input
  df <- data.frame(
    keyVar = keyVar,
    strata = rep(rownames(inp), each = ncol(inp)),
    vals = as.vector(as.matrix(t(inp))))

  # barplot
  p <- ggplot(df, aes(x = keyVar, y = vals, fill = "darkgrey"))
  p <- p + geom_bar(color = "black", stat = "identity")
  p <- p + geom_text(aes(y = vals*1.015, label = vals, vjust=0), size = 3)
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

