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
#' @param fast beta version of faster algorithm should not change the results
#' in any way
#' @return Object from class freqCalc.
#' \item{freqCalc}{data set}
#' \item{keyVars}{variables used for frequency calculation}
#' \item{w}{index of weight vector. NULL if you do not have a sample.}
#' \item{indexG}{}
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
#' f2 <- freqCalc(x,  keyVars=c(2,4,5,6),w=8)
#' f2$Fk
#' # time comparison freqCalc old version  vs. new version
#' data(testdata)
#' system.time( f3 <-  freqCalc(testdata,keyVars=c(1:4,7),w=14,fast=FALSE) )
#' system.time( f3f <- freqCalc(testdata,keyVars=c(1:4,7),w=14,fast=TRUE) )
#'
freqCalc <- function(x, keyVars, w = NULL, fast = TRUE) {
  # some parts do not work yet with data.tables
  # this should be fixed
  if ( is.data.table(x) ) {
    class(x) <- "data.frame"
  }
  if (is.numeric(keyVars)) {
    keyVars <- colnames(x)[keyVars]
  }
  # classInfo <- character() xKeys <- x[,keyVars,drop=FALSE] for(i in 1:ncol(xKeys)){
  # classInfo[i] <- class(xKeys[,i]) } dfInfo <- is.data.frame(x) ## internally code as
  # numbers: for(i in 1:ncol(xKeys)){ xKeys[,i] <- as.numeric(as.factor(xKeys[,i])) } TODO:
  # directly work with xKeys in ffc and freqCalc x[,keyVars] <- xKeys
  if (fast) {
    TFna <- any(is.na(x[, keyVars]))
    if (TFna) {
      if (nrow(x) > 5000) {
        z <- sffcNA(x, keyVars, w)
      } else {
        z <- ffc(x, keyVars, w)
      }
      # }
    } else {
      z <- sffc(x, keyVars, w)
    }
  } else {
    y <- x

    x <- x[, keyVars]
    x <- apply(x, 2, function(x) {
      as.integer(as.factor(x))
    })
    x <- apply(x, 1, rbind)

    N <- dim(y)[1]
    S <- dim(y[, keyVars, drop = FALSE])[2]
    res <- .C("f2", as.integer(c(N, S)), as.integer(ifelse(is.na(x), -999999, x)), as.integer(rep(0,
      N)), as.numeric(rep(0, N)), as.numeric(if (length(w) == 0) rep(1, N) else y[, w]),
      PACKAGE = "sdcMicro", NUOK = TRUE)
    z <- list(freqCalc = y, keyVars = keyVars, w = w, indexG = NULL, fk = res[[3]], Fk = res[[4]],
      n1 = length(which(res[[3]] == 1)), n2 = length(which(res[[3]] == 2)))
    class(z) <- "freqCalc"
  }
  invisible(z)
}

ffc <- function(x, keyVars, w = NULL) {
  treatmissing <- -999
  dataX <- x[, keyVars, drop = FALSE]
  weighted <- 0
  if (!is.null(w)) {
    weighted <- 1
    dataX <- cbind(dataX, x[, w])
  }
  for (i in 1:ncol(dataX)) {
    if (!is.numeric(dataX[, i]))
      dataX[, i] <- as.numeric(dataX[, i])
  }
  dataX <- as.matrix(dataX)
  while (any(dataX == treatmissing, na.rm = TRUE)) {
    treatmissing <- -sample(999:999999, 1)
  }
  dataX[is.na(dataX)] <- treatmissing
  ind <- do.call(order, data.frame(dataX))
  dataX <- dataX[ind, , drop = FALSE]
  ind <- order(c(1:nrow(dataX))[ind])
  if (weighted == 1) {
    Res <- .Call("ffc", dataX, 1, length(keyVars), treatmissing)$Res[ind, ]
    Fk <- Res[, 2]
    fk <- Res[, 1]
  } else {
    Fk <- fk <- .Call("ffc", dataX, 0, length(keyVars), treatmissing)$Res[ind, 1]
  }
  res <- list(freqCalc = x, keyVars = keyVars, w = w, indexG = NULL, fk = as.integer(fk),
    Fk = Fk, n1 = sum(fk == 1, na.rm = TRUE), n2 = sum(fk == 2, na.rm = TRUE))
  class(res) <- "freqCalc"
  invisible(res)
}

## data.table based frequency calculation without any NA in keyVariables Author: Alexander
## Kowarik
sffc <- function(x, keyVars, w = NULL) {
  xorig <- x
  .I <- idvarextraforsffc <- NULL
  if (is.numeric(keyVars))
    keyVars <- colnames(x)[keyVars]
  if (is.null(w)) {
    dat <- data.table(x[, keyVars, drop = FALSE])
    setnames(dat, keyVars)
    dat[, `:=`(idvarextraforsffc, .I)]
    setkeyv(dat, keyVars)
    erg <- vector()
    cmd <- paste("erg <- dat[,list(fk=.N),by=list(", paste(keyVars, collapse = ","), ")]", sep = "")
    eval(parse(text = cmd))
    erg <- merge(erg, dat)
    setkey(erg, "idvarextraforsffc")
    res <- list(freqCalc = xorig, keyVars = keyVars, w = w, indexG = NULL, fk = as.integer(erg$fk),
      Fk = as.numeric(erg$fk), n1 = sum(erg$fk == 1, na.rm = TRUE), n2 = sum(erg$fk == 2, na.rm = TRUE))
  } else {
    dat <- data.table(x[, keyVars], weight = x[, w])
    setnames(dat, c(keyVars, "weight"))
    dat[, `:=`(idvarextraforsffc, .I)]
    setkeyv(dat, keyVars)
    erg <- vector()
    cmd <- paste("erg <- dat[,list(Fk=sum(weight),fk=.N),by=list(", paste(keyVars, collapse = ","),")]", sep = "")
    eval(parse(text = cmd))
    erg <- merge(erg, dat)
    setkey(erg, "idvarextraforsffc")
    res <- list(freqCalc = xorig, keyVars = keyVars, w = w, indexG = NULL, fk = as.integer(erg$fk),
      Fk = as.numeric(erg$Fk), n1 = sum(erg$fk == 1, na.rm = TRUE), n2 = sum(erg$fk == 2, na.rm = TRUE))
  }
  class(res) <- "freqCalc"
  invisible(res)
}

## data.table based frequency calculation with NA in keyVariables Author: Alexander Kowarik
sffcNA <- function(x, keyVars, w = NULL) {
  ergna4 <- idvarextraforsffc <- .I <- datwona <- fkneu <- fk <- ergna <- plusNA <- jjjj <- NULL
  datwonlyna <- Fk <- Fkneu <- sFk <- plusSUMNA <- sfk <- matchedObsW <- matchedObs <- ind <- J <- NULL
  xorig <- x  # for returning
  x <- x[, keyVars, drop = FALSE]  #reduce data set to small necessary variables
  for (k in keyVars) {
    # all keyVars should be numeric, (no factors)
    if (!is.numeric(x[, k]))
      x[, k] <- as.numeric(x[, k])
  }
  if (is.numeric(keyVars))
    keyVars <- colnames(x)[keyVars]
  if (is.null(w)) {
    # Compute fk for observations without any NA
    dat <- data.table(x)  # create data.table
    dat[, `:=`(idvarextraforsffc, .I)]  #unique id for easy outputting
    # Split data set in data set with NAs and without
    cmd <- paste("datwona <- dat[", paste("!is.na(", keyVars, ")", collapse = "&", sep = ""),
      "]", sep = "")
    eval(parse(text = cmd))
    cmd <- paste("datwonlyna <- dat[", paste("is.na(", keyVars, ")", collapse = "&", sep = ""),"]", sep = "")
    eval(parse(text = cmd))
    cmd <- paste("datwna <- dat[(", paste("is.na(", keyVars, ")", collapse = "|", sep = ""),
      ")&(", paste("!is.na(", keyVars, ")", collapse = "|", sep = ""), ")]", sep = "")
    eval(parse(text = cmd))
    setkeyv(datwona, keyVars)
    erg <- vector()
    # erg contains the 'first' fk for all observations without any NAs
    cmd <- paste("erg <- datwona[,list(fk=.N),by=list(", paste(keyVars, collapse = ","),
      ")]", sep = "")
    eval(parse(text = cmd))
    allCombKeysVars <- as.list(set_power(keyVars))  ## build the power set of all keyVars
    allCombKeysVars <- allCombKeysVars[-c(1, length(allCombKeysVars))]  ##delete the empty set and the full set
    if (nrow(datwonlyna) > 0) {
      allNAexist <- TRUE  ##TODO: What to do with observations where all key variables are missing?!
    } else {
      allNAexist <- FALSE
    }
    # First forloop updates fk for observations without any NA
    matched <- data.table(ind = 1, indM = 1, matchedObs = 1)[-1]
    erg[, `:=`(fkneu, fk)]
    cmd <- paste("ergna2 <- datwna[", paste("is.na(", keyVars, ")", collapse = "&", sep = ""),
      ",list(plusNA=.N),by=list(", paste(keyVars, collapse = ","), ")]", sep = "")
    eval(parse(text = cmd))
    for (i in seq_along(allCombKeysVars)) {
      nakeyVars <- unlist(as.list(allCombKeysVars[[i]]))
      notnakeyVars <- keyVars[!keyVars %in% nakeyVars]
      cmd <- paste("ergna <- datwna[", paste("is.na(", nakeyVars, ")", collapse = "&",
        sep = ""), "&", paste("!is.na(", notnakeyVars, ")", collapse = "&", sep = ""),
        ",list(plusNA=.N),by=list(", paste(keyVars, collapse = ","), ")]", sep = "")
      eval(parse(text = cmd))
      if (nrow(ergna) > 0) {
        indM <- nrow(ergna2) + 1
        ergna2 <- rbind(ergna2, ergna)
        indM <- indM:nrow(ergna2)
        setkeyv(erg, notnakeyVars)
        setkeyv(ergna, notnakeyVars)
        cmd <- paste("erg <- merge(erg,ergna[,list(", paste(c(notnakeyVars, "plusNA"),
          collapse = ","), ")],all.x=TRUE)")
        eval(parse(text = cmd))
        ergD <- erg[!is.na(plusNA), ]
        cmd <- paste("ergD <- ergD[,sum(fk,na.rm=TRUE),by=list(", paste(notnakeyVars,
          collapse = ",", sep = ""), ")]")
        eval(parse(text = cmd))
        setnames(ergD, ncol(ergD), "sfk")
        ergna2[, `:=`(jjjj, .I)]
        ergD <- merge(ergna2[indM, ], ergD, all.x = TRUE, by = notnakeyVars)
        ergD[is.na(sfk), `:=`(sfk, 0L)]
        setkey(ergD, "jjjj")
        ergna2[, `:=`(jjjj, NULL)]
        tmpX <- data.table(ind = i, indM = indM, matchedObs = ergD$sfk)
        matched <- rbind(matched, tmpX)
        erg[!is.na(plusNA), `:=`(fkneu, fkneu + plusNA)]
        erg[, `:=`(plusNA, NULL)]
      }
    }
    # Second forloop computes fk for observations with NA (only based on non-NA-obs)
    ergna2[, `:=`(indM, .I)]
    setkey(ergna2, "indM")
    setkey(matched, "indM")
    ergna2 <- merge(ergna2, matched, all.x = TRUE)
    ergna2[, `:=`(fk, plusNA + matchedObs)]
    ergna2[, `:=`(c("matchedObs", "indM", "ind"), NULL)]
    setkey(matched, "ind")
    indtmp <- NULL
    for (i in matched[J(unique(ind)), "ind", with = FALSE, mult = "first"]$ind) {
      nakeyVars <- unlist(as.list(allCombKeysVars[[i]]))
      notnakeyVars <- keyVars[!keyVars %in% nakeyVars]
      indtmp <- matched[J(i), "indM", with = FALSE]$indM
      ergna3 <- ergna2[-indtmp, ]
      if (nrow(ergna3) > 0) {
        for (j in indtmp) {
          cmd <- paste("ergna4 <- ergna3[", paste("(", notnakeyVars, "==ergna2[j,",
          notnakeyVars, "]|is.na(", notnakeyVars, "))", sep = "", collapse = "&"),
          ",sum(plusNA)]", sep = "")
          eval(parse(text = cmd))
          if ( nrow(ergna4) > 0 ) {
            ergna2[j, `:=`(fk, fk + ergna4)]
          }
        }
      }
    }
    ergna2[, `:=`(plusNA, NULL)]
    setkeyv(erg, keyVars)
    erg[, `:=`(fk, fkneu)]
    erg[, `:=`(fkneu, NULL)]
    erg <- merge(erg, datwona)
    setkeyv(ergna2, keyVars)
    for (k in keyVars) {
      cmd <- paste("ergna2[is.na(", k, "),", k, ":=999777666L]", sep = "")
      eval(parse(text = cmd))
      cmd <- paste("datwna[is.na(", k, "),", k, ":=999777666L]", sep = "")
      eval(parse(text = cmd))
    }
    setkeyv(datwna, keyVars)
    setkeyv(ergna2, keyVars)
    datwna <- merge(datwna, ergna2)

    for (k in keyVars) {
      cmd <- paste("datwna[", k, "==999777666,", k, ":=NA]", sep = "")
      eval(parse(text = cmd))
    }
    if (allNAexist) {
      # if all keyVariables are missing
      fknaonly <- nrow(datwonlyna)
      datwonlyna[, `:=`(fk, nrow(dat))]
      setkeyv(datwonlyna, keyVars)
      datwna[, `:=`(fk, fk + fknaonly)]
      erg[, `:=`(fk, fk + fknaonly)]
      erg <- rbind(datwonlyna[, j = colnames(erg), with = FALSE], datwna[, j = colnames(erg),
        with = FALSE], erg)
    } else {
      erg <- rbind(datwna[, j = colnames(erg), with = FALSE], erg)
    }
    setkey(erg, "idvarextraforsffc")
    res <- list(freqCalc = xorig, keyVars = keyVars, w = w, indexG = NULL, fk = as.integer(erg$fk),
      Fk = as.numeric(erg$fk), n1 = sum(erg$fk == 1, na.rm = TRUE), n2 = sum(erg$fk ==
        2, na.rm = TRUE))
  } else {
    # Compute fk for observations without any NA
    dat <- data.table(x, gew = xorig[, w])  # create data.table
    dat[, `:=`(idvarextraforsffc, .I)]  #unique id for easy outputting
    # Split data set in data set with NAs and without
    cmd <- paste("datwona <- dat[", paste("!is.na(", keyVars, ")", collapse = "&", sep = ""),
      "]", sep = "")
    eval(parse(text = cmd))
    cmd <- paste("datwonlyna <- dat[", paste("is.na(", keyVars, ")", collapse = "&", sep = ""),
      "]", sep = "")
    eval(parse(text = cmd))
    cmd <- paste("datwna <- dat[(", paste("is.na(", keyVars, ")", collapse = "|", sep = ""),
      ")&(", paste("!is.na(", keyVars, ")", collapse = "|", sep = ""), ")]", sep = "")
    eval(parse(text = cmd))
    setkeyv(datwona, keyVars)
    erg <- vector()
    # erg contains the 'first' fk for all observations without any NAs
    cmd <- paste("erg <- datwona[,list(fk=.N,Fk=sum(gew)),by=list(", paste(keyVars, collapse = ","),
      ")]", sep = "")
    eval(parse(text = cmd))
    allCombKeysVars <- as.list(set_power(keyVars))  ## build the power set of all keyVars
    allCombKeysVars <- allCombKeysVars[-c(1, length(allCombKeysVars))]  ##delete the empty set and the full set
    if (nrow(datwonlyna) > 0) {
      allNAexist <- TRUE  ##TODO: What to do with observations where all key variables are missing?!
    } else {
      allNAexist <- FALSE
    }
    # First forloop updates fk for observations without any NA
    matched <- data.table(ind = 1, indM = 1, matchedObs = 1, matchedObsW = 1)[-1]
    erg[, `:=`(fkneu, fk)]
    erg[, `:=`(Fkneu, Fk)]
    cmd <- paste("ergna2 <- datwna[", paste("is.na(", keyVars, ")", collapse = "&", sep = ""),
      ",list(plusNA=.N,plusSUMNA=sum(gew)),by=list(", paste(keyVars, collapse = ","),
      ")]", sep = "")
    eval(parse(text = cmd))
    for (i in seq_along(allCombKeysVars)) {
      nakeyVars <- unlist(as.list(allCombKeysVars[[i]]))
      notnakeyVars <- keyVars[!keyVars %in% nakeyVars]
      cmd <- paste("ergna <- datwna[", paste("is.na(", nakeyVars, ")", collapse = "&",
        sep = ""), "&", paste("!is.na(", notnakeyVars, ")", collapse = "&", sep = ""),
        ",list(plusNA=.N,plusSUMNA=sum(gew)),by=list(", paste(keyVars, collapse = ","),
        ")]", sep = "")
      eval(parse(text = cmd))
      if (nrow(ergna) > 0) {
        indM <- nrow(ergna2) + 1
        ergna2 <- rbind(ergna2, ergna)
        indM <- indM:nrow(ergna2)
        setkeyv(erg, notnakeyVars)
        setkeyv(ergna, notnakeyVars)
        cmd <- paste("erg <- merge(erg,ergna[,list(", paste(c(notnakeyVars, "plusNA","plusSUMNA"), collapse = ","), ")],all.x=TRUE)")
        eval(parse(text = cmd))
        ergD <- erg[!is.na(plusNA), ]
        cmd <- paste("ergD <- ergD[,list(sum(fk,na.rm=TRUE),sum(Fk,na.rm=TRUE)),by=list(",
          paste(notnakeyVars, collapse = ",", sep = ""), ")]")
        eval(parse(text = cmd))
        setnames(ergD, ncol(ergD), "sFk")
        setnames(ergD, ncol(ergD) - 1, "sfk")
        ergna2[, `:=`(jjjj, .I)]
        ergD <- merge(ergna2[indM, ], ergD, all.x = TRUE, by = notnakeyVars)
        ergD[is.na(sfk), `:=`(sfk, 0L)]
        ergD[is.na(sFk), `:=`(sFk, 0L)]
        setkey(ergD, "jjjj")
        ergna2[, `:=`(jjjj, NULL)]
        tmpX <- data.table(ind = i, indM = indM, matchedObs = ergD$sfk, matchedObsW = ergD$sFk)
        matched <- rbind(matched, tmpX)

        erg[!is.na(plusNA), `:=`(c("fkneu", "Fkneu"), list(fkneu+plusNA, Fkneu+plusSUMNA))]
        # 1 and #2 should be the same, but maybe #2 is better for checkign?!
        erg[, `:=`(c("plusNA", "plusSUMNA"), NULL)]
      }
    }
    # Second part computes fk for observations with NA (only based on non-NA-obs)
    ergna2[, `:=`(indM, .I)]
    setkey(ergna2, "indM")
    setkey(matched, "indM")
    ergna2 <- merge(ergna2, matched, all.x = TRUE)
    ergna2[, `:=`(c("fk", "Fk"), list(plusNA + matchedObs, plusSUMNA + matchedObsW))]
    ergna2[, `:=`(c("matchedObs", "indM", "ind", "matchedObsW"), NULL)]
    setkey(matched, "ind")
    indtmp <- NULL
    ## End Sec Part Third Part computes fk for NA observations
    for (i in matched[J(unique(ind)), "ind", with = FALSE, mult = "first"]$ind) {
      nakeyVars <- unlist(as.list(allCombKeysVars[[i]]))
      notnakeyVars <- keyVars[!keyVars %in% nakeyVars]
      indtmp <- matched[J(i), "indM", with = FALSE]$indM
      ergna3 <- ergna2[-indtmp, ]
      if (nrow(ergna3) > 0) {
        for (j in indtmp) {
          cmd <- paste("ergna4 <- ergna3[", paste("(", notnakeyVars, "==ergna2[j,",
          notnakeyVars, "]|is.na(", notnakeyVars, "))", sep = "", collapse = "&"),
          ",list(sum(plusNA),sum(plusSUMNA))]", sep = "")
          eval(parse(text = cmd))
          if ( nrow(ergna4) > 0 ) {
            ergna2[j, `:=`(c("fk", "Fk"), list(fk + ergna4$V1, Fk + ergna4$V2))]
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
      cmd <- paste("ergna2[is.na(", k, "),", k, ":=999777666L]", sep = "")
      eval(parse(text = cmd))
      cmd <- paste("datwna[is.na(", k, "),", k, ":=999777666L]", sep = "")
      eval(parse(text = cmd))
    }
    setkeyv(datwna, keyVars)
    setkeyv(ergna2, keyVars)
    datwna <- merge(datwna, ergna2)

    for (k in keyVars) {
      cmd <- paste("datwna[", k, "==999777666,", k, ":=NA]", sep = "")
      eval(parse(text = cmd))
    }
    if (allNAexist) {
      # if all keyVariables are missing
      fknaonly <- nrow(datwonlyna)
      Fknaonly <- sum(datwonlyna$gew)
      datwonlyna[, `:=`(c("fk", "Fk"), list(nrow(dat), sum(dat$gew)))]
      setkeyv(datwonlyna, keyVars)
      datwna[, `:=`(c("fk", "Fk"), list(fk + fknaonly, Fk + Fknaonly))]
      erg[, `:=`(c("fk", "Fk"), list(fk + fknaonly, Fk + Fknaonly))]
      erg <- rbind(datwonlyna[, j = colnames(erg), with = FALSE], datwna[, j = colnames(erg), with = FALSE], erg)
    } else {
      erg <- rbind(datwna[, j = colnames(erg), with = FALSE], erg)
    }
    setkey(erg, "idvarextraforsffc")
    res <- list(freqCalc = xorig, keyVars = keyVars, w = w, indexG = NULL, fk = as.integer(erg$fk),
      Fk = as.numeric(erg$Fk), n1 = sum(erg$fk == 1, na.rm = TRUE), n2 = sum(erg$fk == 2, na.rm = TRUE))
  }
  class(res) <- "freqCalc"
  invisible(res)
}

#' Print method for objects from class freqCalc
#'
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
#' localS <- localSupp(f, keyVar=2, indivRisk=indivf$rk, threshold=0.25)
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


