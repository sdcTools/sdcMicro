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
#' @author Bernhard Meindl and Matthias Templ
#' @seealso \code{\link{indivRisk}}, \code{\link{measure_risk}}
#' @references look e.g. in \url{http://neon.vb.cbs.nl/casc/Deliv/12d1.pdf}
#' Templ, M.  \emph{Statistical Disclosure Control for Microdata Using the
#' R-Package sdcMicro}, Transactions on Data Privacy, vol. 1, number 2, pp.
#' 67-85, 2008.  \url{http://www.tdp.cat/issues/abs.a004a08.php}
#'
#' Templ, M.  \emph{New Developments in Statistical Disclosure Control and
#' Imputation: Robust Statistics Applied to Official Statistics},
#' Suedwestdeutscher Verlag fuer Hochschulschriften, 2009, ISBN: 3838108280,
#' 264 pages.
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
  # weight-column is always 'weight'
  cn  <- names(dt)
  cn[length(cn)] <- "weight"
  setnames(dt, cn)

  TFna <- dt[,any(sapply(.SD,function(x)any(is.na(x)))),.SDcols=keyVars_n]
  if (TFna) {
    z <- sffcNA(dt, keyVars_n, alpha)
  } else {
    z <- sffc(dt, keyVars_n)
  }
  z$freqCalc <- x
  z$w <- w
  z$keyVars <- keyVars
  z$alpha <- alpha
  class(z) <- "freqCalc"
  invisible(z)
}

## data.table based frequency calculation without any NA in keyVariables A
## Author: Alexander Kowarik and Bernhard Meindl
sffc <- function(dt, keyVars) {
  .I <- idvarextraforsffc <- NULL
  dt[, `:=`(idvarextraforsffc, .I)]
  setkeyv(dt, keyVars)
  erg <- vector()
  cmd <- paste("erg <- dt[,list(Fk=sum(weight),fk=.N),by=key(dt)]")
  eval(parse(text=cmd))
  erg <- merge(erg, dt)
  setkey(erg, idvarextraforsffc)

  res <- list(freqCalc=dt, keyVars=keyVars, w="weight", fk=as.integer(erg$fk),
    Fk=as.numeric(erg$Fk), n1=sum(erg$fk==1, na.rm=TRUE), n2=sum(erg$fk==2, na.rm=TRUE))
  invisible(res)
}

## data.table based frequency calculation with NA in keyVariables
## Author: Alexander Kowarik and Bernhard Meindl
sffcNA <- function(dt, keyVars, alpha) {
  ergna4 <- idvarextraforsffc <- .I <- datwona <- fkneu <- fk <- ergna <- plusNA <- jjjj <- NULL
  datwonlyna <- Fk <- Fkneu <- sFk <- plusSUMNA <- sfk <- matchedObsW <- matchedObs <- ind <- J <- NULL
  for (k in keyVars) {
    # all keyVars should be numeric, (no factors)
    if (!is.numeric(dt[[k]]))
      dt[[k]] <- as.numeric(dt[[k]])
  }

  # Compute fk for observations without any NA
  dt[, idvarextraforsffc:=.I]  #unique id for easy outputting
  # Split data set in data set with NAs and without
  cmd <- paste0("datwona <- dt[", paste0("!is.na(", keyVars, ")", collapse="&"),"]")
  eval(parse(text=cmd))
  cmd <- paste0("datwonlyna <- dt[", paste0("is.na(", keyVars, ")", collapse="&"),"]")
  eval(parse(text=cmd))
  cmd <- paste0("datwna <- dt[(", paste("is.na(", keyVars, ")", collapse="|"),
    ")&(", paste0("!is.na(", keyVars, ")", collapse="|"), ")]")
  eval(parse(text=cmd))

  setkeyv(datwona, keyVars)
  setkeyv(datwna, keyVars)

  erg <- vector()
  # erg contains the 'first' fk for all observations without any NAs
  cmd <- paste0("erg <- datwona[,list(fk=as.numeric(.N),Fk=sum(weight)),by=key(datwona)]")
  eval(parse(text=cmd))
  allCombKeysVars <- as.list(set_power(keyVars))  ## build the power set of all keyVars
  allCombKeysVars <- allCombKeysVars[-c(1, length(allCombKeysVars))]  ##delete the empty set and the full set

  ##TODO: What to do with observations where all key variables are missing?!
  allNAexist <- ifelse(nrow(datwonlyna) > 0, TRUE, FALSE)

  # First forloop updates fk for observations without any NA
  # matched$ind: relative to allCombKeyVars
  matched <- data.table(ind=1, indM=1, matchedObs=1, matchedObsW=1)[-1]
  erg[,fkneu:=fk]
  erg[,Fkneu:=Fk]
  cmd <- paste("ergna2 <- datwna[", paste0("is.na(", keyVars, ")", collapse="&"),
    ",list(plusNA=.N,plusSUMNA=sum(weight)),by=key(datwna)]")
  eval(parse(text=cmd))
  for (i in seq_along(allCombKeysVars)) {
    nakeyVars <- unlist(as.list(allCombKeysVars[[i]]))
    notnakeyVars <- keyVars[!keyVars %in% nakeyVars]
    cmd <- paste("ergna <- datwna[", paste0("is.na(", nakeyVars, ")", collapse="&"),
      "&", paste0("!is.na(", notnakeyVars, ")", collapse="&"),
      ",list(plusNA=.N,plusSUMNA=sum(weight)),by=key(datwna)]")
    eval(parse(text=cmd))
    if (nrow(ergna) > 0) {
      indM <- nrow(ergna2) + 1
      ergna2 <- rbind(ergna2, ergna)
      indM <- indM:nrow(ergna2)
      setkeyv(erg, notnakeyVars)
      setkeyv(ergna, notnakeyVars)
      cmd <- paste("erg <- merge(erg,ergna[,list(", paste(c(notnakeyVars, "plusNA","plusSUMNA"), collapse=","), ")],all.x=TRUE)")
      eval(parse(text=cmd))
      ergD <- erg[!is.na(plusNA), ]
      cmd <- paste("ergD <- ergD[,list(sum(fk,na.rm=TRUE),sum(Fk,na.rm=TRUE)),by=list(",paste0(notnakeyVars, collapse=","), ")]")
      eval(parse(text=cmd))
      setnames(ergD, ncol(ergD), "sFk")
      setnames(ergD, ncol(ergD)-1, "sfk")
      ergna2[, `:=`(jjjj, .I)]
      ergD <- merge(ergna2[indM, ], ergD, all.x=TRUE, by=notnakeyVars)
      ergD[is.na(sfk), sfk:=0L]
      ergD[is.na(sFk), sFk:=0L]
      setkey(ergD, "jjjj")
      ergna2[, jjjj:=NULL]
      tmpX <- data.table(ind=i, indM=indM, matchedObs=ergD$sfk, matchedObsW=ergD$sFk)
      matched <- rbind(matched, tmpX)
      erg[!is.na(plusNA), `:=`(c("fkneu", "Fkneu"), list(fkneu+(plusNA*alpha), Fkneu+(plusSUMNA*alpha)))]
      # 1 and #2 should be the same, but maybe #2 is better for checkign?!
      erg[, `:=`(c("plusNA", "plusSUMNA"), NULL)]
    }
  }

  # Second part computes fk for observations with NA (only based on non-NA-obs)
  ergna2[, indM:=.I]
  setkey(ergna2, "indM")
  setkey(matched, "indM")
  ergna2 <- merge(ergna2, matched, all.x=TRUE)
  ergna2[, `:=`(c("fk", "Fk"), list(plusNA + matchedObs, plusSUMNA + matchedObsW))]
  ergna2[, `:=`(c("matchedObs", "indM", "ind", "matchedObsW"), NULL)]
  setkey(matched, "ind")
  indtmp <- NULL

  ## End Sec Part Third Part computes fk for NA observations
  for (i in matched[J(unique(ind)), "ind", with=FALSE, mult="first"]$ind) {
    nakeyVars <- unlist(as.list(allCombKeysVars[[i]]))
    notnakeyVars <- keyVars[!keyVars %in% nakeyVars]
    indtmp <- matched[J(i), "indM", with=FALSE]$indM
    ergna3 <- ergna2[-indtmp]
    if (nrow(ergna3) > 0) {
      for (j in indtmp) {
        cmd <- paste0("ergna4 <- ergna3[", paste0("(", notnakeyVars, "==ergna2[j,",
        notnakeyVars, "]|is.na(", notnakeyVars, "))", collapse="&"), ",list(sum(plusNA),sum(plusSUMNA))]")
        eval(parse(text=cmd))
        if (nrow(ergna4) > 0) {
          # scaling fk and Fk by parameter 'alpha'
          ergna2[j, `:=`(c("fk", "Fk"), list(fk + ergna4$V1*alpha, Fk + ergna4$V2*alpha))]
        }
      }
    }
  }
  ergna2[, `:=`(c("plusNA", "plusSUMNA"), NULL)]
  setkeyv(erg, keyVars)
  erg[, `:=`(c("fk", "Fk"), list(fkneu, Fkneu))]
  erg[, `:=`(c("fkneu", "Fkneu"), NULL)]
  erg <- merge(erg, datwona)
  setkeyv(ergna2, keyVars)
  for (k in keyVars) {
    cmd <- paste0("ergna2[is.na(", k, "),", k, ":=999777666L]")
    eval(parse(text=cmd))
    cmd <- paste0("datwna[is.na(", k, "),", k, ":=999777666L]")
    eval(parse(text=cmd))
  }
  setkeyv(datwna, keyVars)
  setkeyv(ergna2, keyVars)
  datwna <- merge(datwna, ergna2)

  for (k in keyVars) {
    cmd <- paste0("datwna[", k, "==999777666,", k, ":=NA]")
    eval(parse(text=cmd))
  }
  if (allNAexist) {
    # if all keyVariables are missing
    fknaonly <- nrow(datwonlyna)
    Fknaonly <- sum(datwonlyna$weight)
    datwonlyna[, `:=`(c("fk", "Fk"), list(nrow(dt), sum(dt$weight)))]
    setkeyv(datwonlyna, keyVars)
    datwna[, `:=`(c("fk", "Fk"), list(fk + fknaonly*alpha, Fk + Fknaonly*alpha))]
    erg[, `:=`(c("fk", "Fk"), list(fk + fknaonly*alpha, Fk + Fknaonly*alpha))]
    erg <- rbind(datwonlyna[, j=colnames(erg), with=FALSE], datwna[, j=colnames(erg), with=FALSE], erg)
  } else {
    erg <- rbind(datwna[, j=colnames(erg), with=FALSE], erg)
  }
  setkey(erg, "idvarextraforsffc")
  res <- list(freqCalc=erg, keyVars=keyVars, w="weight", fk=erg$fk,
    Fk=as.numeric(erg$Fk), n1=sum(erg$fk==1, na.rm=TRUE), n2=sum(erg$fk==2, na.rm=TRUE))
  invisible(res)
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
  cat("\n --------------------------\n")
  cat(paste(x$n1, "obs. violate 2-anonymity \n"))
  cat(paste(x$n2 + x$n1, "obs. violate 3-anonymity \n"))
  cat(" --------------------------\n")
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
  cat("\n Suppressions: \n")
  for (i in 1:length(a1)) {
    if (a1[i] != 0)
      cat(paste("\nLocal suppression in", names(a1)[i], ":", a1[i], "/", P, "\n"))
  }
}
