#' Frequencies calculation for risk estimation
#'
#' Computation and estimation of the sample and population frequency counts.
#'
#' The function considers the case of missing values in the data.  A missing
#' value stands for any of the possible categories of the variable considered.
#' It is possible to apply this function to large data sets with many
#' (catergorical) key variables, since the computation is done in C.
#'
#' \emph{freqCalc()} does not support sdcMicro S4 class objects.
#'
#' @param x data frame or matrix
#' @param keyVars key variables
#' @param w column index of the weight variable. Should be set to NULL if one
#' deal with a population.
#' @param alpha numeric value between 0 and 1 specifying how much keys that
#' contain missing values (\code{NAs}) should contribute to the calculation
#' of \code{fk} and \code{Fk}. For the default value of \code{1}, nothing changes with
#' respect to the implementation in prior versions. Each \emph{wildcard-match} would
#' be counted while for \code{alpha=0} keys with missing values would be basically ignored.
#' @return Object from class freqCalc.
#' \item{freqCalc}{data set}
#' \item{keyVars}{variables used for frequency calculation}
#' \item{w}{index of weight vector. NULL if you do not have a sample.}
#' \item{alpha}{value of parameter \code{alpha}}
#' \item{fk}{the frequency of equal observations in
#' the key variables subset sample given for each observation.}
#' \item{Fk}{estimated frequency in the population}
#' \item{n1}{number of observations with fk=1}
#' \item{n2}{number of observations with fk=2}
#' @author Bernhard Meindl
#' @seealso \code{\link{indivRisk}}, \code{\link{measure_risk}}
#' @references look e.g. in \url{https://research.cbs.nl/casc/deliv/12d1.pdf}
#' Templ, M.  \emph{Statistical Disclosure Control for Microdata Using the
#' R-Package sdcMicro}, Transactions on Data Privacy, vol. 1, number 2, pp.
#' 67-85, 2008.  \url{https://www.tdp.cat/issues/abs.a004a08.php}
#'
#' Templ, M.  \emph{New Developments in Statistical Disclosure Control and
#' Imputation: Robust Statistics Applied to Official Statistics},
#' Suedwestdeutscher Verlag fuer Hochschulschriften, 2009, ISBN: 3838108280,
#' 264 pages.
#' 
#' Templ, M. Statistical Disclosure Control for Microdata: Methods and Applications in R.
#' \emph{Springer International Publishing}, 287 pages, 2017. ISBN 978-3-319-50272-4. \doi{10.1007/978-3-319-50272-4}
#' \doi{10.1007/978-3-319-50272-4}
#'
#' Templ, M. and Meindl, B.: \emph{Practical Applications in Statistical
#' Disclosure Control Using R}, Privacy and Anonymity in Information Management
#' Systems New Techniques for New Practical Problems, Springer, 31-62, 2010,
#' ISBN: 978-1-84996-237-7.
#' @keywords manip
#' @export
#' @examples
#'
#' data(francdat)
#' f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
#' f
#' f$freqCalc
#' f$fk
#' f$Fk
#' ## with missings:
#' x <- francdat
#' x[3,5] <- NA
#' x[4,2] <- x[4,4] <- NA
#' x[5,6]  <- NA
#' x[6,2]  <- NA
#' f2 <- freqCalc(x, keyVars=c(2,4,5,6),w=8)
#' cbind(f2$fk, f2$Fk)
#'
#' ## test parameter 'alpha'
#' f3a <- freqCalc(x, keyVars=c(2,4,5,6), w=8, alpha=1)
#' f3b <- freqCalc(x, keyVars=c(2,4,5,6), w=8, alpha=0.5)
#' f3c <- freqCalc(x, keyVars=c(2,4,5,6), w=8, alpha=0.1)
#' data.frame(fka=f3a$fk, fkb=f3b$fk, fkc=f3c$fk)
#' data.frame(Fka=f3a$Fk, Fkb=f3b$Fk, Fkc=f3c$Fk)
freqCalc <- function(x, keyVars, w=NULL, alpha=1) {
  addAllNAtoKeys <- FALSE

  . <- Fk <- fk <- id <- keyid <- na_ids <- sortidforfreqcalc <- sortvar <- weight <- NULL
  xxxxx_tmpweight_xxxxx <- NULL
  if (alpha <0 | alpha>1) {
    stop("parameter 'alpha' needs to be >0 0 and <= 1!\n")
  }
  if (!is.data.table(x)) {
    xdt <- as.data.table(x)
  } else {
    xdt <- copy(x)
  }
  if (is.numeric(keyVars)) {
    keyVars_n <- names(xdt)[keyVars]
  } else {
    keyVars_n <- keyVars
  }
  if (!is.null(w)) {
    if (is.numeric(w)) {
      w_n <- colnames(x)[w]
    } else {
      w_n <- w
    }
  } else {
    xdt[, xxxxx_tmpweight_xxxxx:=1]
    w_n <- "xxxxx_tmpweight_xxxxx"
  }
  dt <- xdt[,c(keyVars_n,w_n), with=FALSE]
  # factors must be converted to numeric
  dt <- dt[,lapply(.SD, as.numeric)]
  # weight-column is always 'weight'
  cn  <- names(dt)
  cn[length(cn)] <- "weight"
  setnames(dt, cn)

  dt[,sortidforfreqcalc:=.I]
  setkeyv(dt, keyVars_n)

  agg <- dt[,.(fk=.N, Fk=sum(weight)), by=key(dt)]
  TFna <- dt[,any(sapply(.SD,function(x)any(is.na(x)))),.SDcols=keyVars_n]
  if (!TFna) {
    setkeyv(agg, keyVars_n)
    setkeyv(dt, keyVars_n)
    agg <- agg[dt]
    setkey(agg, sortidforfreqcalc)

    z <- list(freqCalc=x, keyVars=keyVars, w=w, fk=agg$fk, Fk=agg$Fk,
      n1=sum(agg$fk==1, na.rm=TRUE), n2=sum(agg$fk==2, na.rm=TRUE), alpha=alpha)
    class(z) <- "freqCalc"
    return(z)
  }

  agg[,fk:=as.numeric(fk)]
  agg[,Fk:=as.numeric(Fk)]

  # split in complete and non-complete
  dat_without_na <- na.omit(agg, cols=keyVars_n)
  dat_with_na <- na.omit(agg, cols=keyVars_n, invert=TRUE)
  if (nrow(dat_without_na) == 0)
    dat_with_na <- copy(agg)

  # special treatment - one key variable containing missings
  if (length(keyVars_n)==1) {
    dat_without_na[,fk:=fk+sum(dat_with_na[,fk])*alpha]
    dat_without_na[,Fk:=Fk+sum(dat_with_na[,Fk])*alpha]
    dat_with_na[,fk:=sum(agg[,fk])]
    dat_with_na[,Fk:=sum(agg[,Fk])]
    new <- rbind(dat_without_na, dat_with_na)
    new <- setkeyv(new, keyVars_n)
    new <- new[dt]
    setkey(new, sortidforfreqcalc)
    z <- list(freqCalc=x, keyVars=keyVars, w=w, fk=new$fk, Fk=new$Fk,
      n1=sum(new$fk==1, na.rm=TRUE), n2=sum(new$fk==2, na.rm=TRUE), alpha=alpha)
    class(z) <- "freqCalc"
    return(z)
  }

  nr_kv <- length(keyVars_n)
  naind <- dat_with_na[,lapply(.SD, function(x) {
    !is.na(x)
  }), .SDcols=keyVars_n]

  # Idea: compute unique combinations of NA-positions in keys and add an id for this
  # we can use this information later when sorting the dataset without NAs
  un <- unique(naind)
  sortKeys <- lapply(1:nrow(un), function(x) {
    keyVars_n[unlist(un[x])]
  })
  ii <- which(rowSums(un)==0)
  if (length(ii)>0) {
    sortKeys[[ii]] <- keyVars_n
  }

  un[,keyid:=.I]
  naind[,sortvar:=.I]
  naind <- merge(naind, un, by=keyVars_n, suffixes = c("", ".y"))

  # we need to resort so that later in the look we can use 'keyid' to set the key
  # for the complete dataset
  dat_with_na <- dat_with_na[naind$sortvar]
  dat_with_na[,tmpid:=.I]
  conditions <- rep("", nrow(dat_with_na))
  for (z in keyVars_n) {
    tmpid <- na.omit(dat_with_na, cols=z)[,tmpid]
    c1 <- paste0("(",z,"==",dat_with_na[[z]][tmpid],"| is.na(",z,"))")
    conditions[tmpid] <- paste0(conditions[tmpid],"&",c1)
  }
  conditions <- paste0(conditions,"&id!=",1:nrow(dat_with_na))
  conditions <- substr(conditions,2,nchar(conditions))

  dat_with_na[,tmpid:=NULL]

  # initialize some values
  dat_without_na[,id:=.I]
  dat_with_na[,id:=.I]

  # initialize fk|Fk and add_fks|add_Fks for na and non-na datasets with original values or 0
  fks_na <- dat_with_na[,fk]
  Fks_na <- dat_with_na[,Fk]
  add_fks_na <- add_Fks_na <- rep(0, nrow(dat_with_na))

  fks_nona <- dat_without_na[,fk]
  Fks_nona <- dat_without_na[,Fk]
  add_fks_nona <- add_Fks_nona <- rep(0, nrow(dat_without_na))

  for (i in 1:nrow(dat_with_na)) {
    cmd1 <- paste0("na_ids <- dat_with_na[", conditions[i],", id]")
    eval(parse(text=cmd1))

    if (i==1 || (naind[i,keyid] != naind[i-1,keyid])) {
      cur_sortVars <- sortKeys[[naind[i,keyid]]]
      setkeyv(dat_without_na, cur_sortVars)
    }

    if (nrow(dat_without_na)==0) {
      ids_complete <- NULL
    } else {
      ids_complete <- dat_without_na[as.list(dat_with_na[i, cur_sortVars, with=F]),id]
      if (is.na(ids_complete)[1]) {
        ids_complete <- NULL
      }
    }

    # update dataset containing NA's
    # na.rm=TRUE fixes case, when we do not have complete keys!
    add_fks_na[i] <- add_fks_na[i] + sum(fks_na[na_ids])*alpha + sum(fks_nona[ids_complete], na.rm=TRUE)
    add_Fks_na[i] <- add_Fks_na[i] + sum(Fks_na[na_ids])*alpha + sum(Fks_nona[ids_complete], na.rm=TRUE)

    # update 'complete' dataset
    add_fks_nona[ids_complete] <- add_fks_nona[ids_complete] + fks_na[i]*alpha
    add_Fks_nona[ids_complete] <- add_Fks_nona[ids_complete] + Fks_na[i]*alpha
  }

  ## TODO: CHECK IF THIS IS NEEDED
  # in case we have NA-only keys, we add their (weighted) fks/Fks to all keys with no missing values
  if (addAllNAtoKeys) {
    ii <- which(rowSums(is.na(dat_with_na[,keyVars_n, with=FALSE]))==nr_kv)
    if (length(ii)==1) {
      add_fks_nona <- add_fks_nona + dat_with_na[ii,fk]*alpha
      add_Fks_nona <- add_Fks_nona + dat_with_na[ii,Fk]*alpha
    }
  }

  setkeyv(dat_without_na, keyVars_n)
  dat_without_na[,fk:=fk+add_fks_nona]
  dat_without_na[,Fk:=Fk+add_Fks_nona]
  dat_with_na[,fk:=fk+add_fks_na]
  dat_with_na[,Fk:=Fk+add_Fks_na]
  agg <- rbind(dat_without_na, dat_with_na)
  setkeyv(agg, keyVars_n)
  setkeyv(dt, keyVars_n)
  agg <- agg[dt]
  setkey(agg, sortidforfreqcalc)

  # allNA-keys
  ii <- which(rowSums(is.na(agg[,keyVars_n, with=FALSE]))==nr_kv)
  if (length(ii)>0) {
    naonly_fk <- nrow(agg)
    naonly_Fk <- agg[,sum(weight)]
    agg[ii, c("fk","Fk"):=list(naonly_fk, naonly_Fk)]
  }

  z <- list(freqCalc=x, keyVars=keyVars, w=w, fk=agg$fk, Fk=agg$Fk,
    n1=sum(agg$fk==1, na.rm=TRUE), n2=sum(agg$fk==2, na.rm=TRUE), alpha=alpha)
  class(z) <- "freqCalc"
  z
}

#' Print method for objects from class freqCalc.
#'
#' @param x object from class \code{\link{freqCalc}}
#' @param \dots Additional arguments passed through.
#' @return information about the frequency counts for key variables for object
#' of class \code{\link{freqCalc}}.
#' @author Matthias Templ
#' @seealso \code{\link{freqCalc}}
#' @keywords print
#' @method print freqCalc
#' @export
#' @examples
#'
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
#' f
#'
print.freqCalc <- function(x, ...) {
  P <- dim(x)[2]
  message("\n --------------------------\n")
  message(paste(x$n1, "obs. violate 2-anonymity \n"))
  message(paste(x$n2 + x$n1, "obs. violate 3-anonymity \n"))
  message(" --------------------------\n")
}

#' Summary method for objects from class freqCalc
#'
#' Summary method for objects of class \sQuote{freqCalc} to provide information
#' about local suppressions.
#'
#' Shows the amount of local suppressions on each variable in which local
#' suppression was applied.
#'
#' @param object object from class freqCalc
#' @param \dots Additional arguments passed through.
#' @return Information about local suppression in each variable (only if a
#' local suppression is already done).
#' @author Matthias Templ
#' @seealso \code{\link{freqCalc}}
#' @keywords print
#' @method summary freqCalc
#' @export
#' @examples
#'
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
#' f
#' f$fk
#' f$Fk
#' ## individual risk calculation:
#' indivf <- indivRisk(f)
#' indivf$rk
#' ## Local Suppression
#' localS <- localSupp(f, keyVar=2, threshold=0.25)
#' f2 <- freqCalc(localS$freqCalc, keyVars=c(4,5,6), w=8)
#' summary(f2)
#'
summary.freqCalc <- function(object, ...) {
  a1 <- c(apply(object$freqCalc[, object$keyVars, drop = FALSE], 2, function(x) {
    sum(is.na(x))
  }))
  P <- dim(object$freqCalc)[1]
  message("\n Suppressions: \n")
  for (i in 1:length(a1)) {
    if (a1[i] != 0)
      message(paste("\nLocal suppression in", names(a1)[i], ":", a1[i], "/", P, "\n"))
  }
}
