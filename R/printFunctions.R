# format as string with given number of digits
prettyF <- function(inp, digits=3) {
  formatC(as.numeric(inp), format="f", digits=digits)
}

#' @rdname print.sdcMicroObj
#' @export
setGeneric("freq", function(obj, type = "fk") {
  standardGeneric("freq")
})

setMethod(f = "freq", signature = c("sdcMicroObj"),
definition = function(obj, type = "fk") {
  if (type == "fk")
    ret <- obj@risk$individual[, 2] else if (type == "Fk")
    ret <- obj@risk$individual[, 3] else stop(paste("type=", type, "is unknown."))
  return(ret)
})

#' Print and Extractor Functions for objects of class \code{\link{sdcMicroObj-class}}
#'
#' Descriptive print function for Frequencies, local Supression, Recoding,
#' categorical risk and numerical risk.
#'
#' Possible values for the type argument of the print function are: "freq": for
#' Frequencies, "ls": for Local Supression output, "pram": for results of
#' post-randomization "recode":for Recodes, "risk": forCategorical risk and
#' "numrisk": for Numerical risk.
#'
#' Possible values for the type argument of the freq function are: "fk": Sample
#' frequencies and "Fk": weighted frequencies.
#'
#' @name print.sdcMicroObj
#' @aliases print-methods freq-methods print,sdcMicroObj-method
#' freq,sdcMicroObj-method freq print
#' @docType methods
#' @param x An object of class \code{\link{sdcMicroObj-class}}
#' @param obj An object of class \code{\link{sdcMicroObj-class}}
#' @param type Selection of the content to be returned or printed-
#' @param ... the type argument for the print method, currently supported are:
#' \itemize{
#' \item general: basic information on the input obj such as the number of observations
#' and variables.
#' \item kAnon: displays information about 2- and 3-anonymity
#' \item ls: displays various information if local suppression has been applied.
#' \item pram: displays various information if post-randomization has been applied.
#' \item recode: shows information about categorical key variables before and after recoding
#' \item risk: displays information on re-identification risks
#' \item numrisk: displays risk- and utility measures for numerical key variables
#' }
#' @author Alexander Kowarik, Matthias Templ
#' @keywords classes
#' @export
#' @examples
#'
#' data(testdata)
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('urbrur','roof','walls','relat','sex'),
#'   pramVars=c('water','electcon'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' fk=freq(sdc)
#' Fk=freq(sdc,type="Fk")
#' print(sdc)
#' print(sdc,type="general")
#' print(sdc,type="ls")
#' print(sdc,type="recode")
#' print(sdc,type="risk")
#' print(sdc,type="numrisk")
#' print(sdc,type="pram")
#' print(sdc,type="kAnon")
#'
setMethod(f = "print", signature = c("sdcMicroObj"),
definition = function(x, type = "kAnon", ...) {
  if ( !type %in% c("kAnon","ls","pram","recode","risk","numrisk","general") ) {
    stop(paste("type=", type, "is unknown."))
  }

  # type=kAnon,ls,recode,risk,numrisk,general
  obj <- x
  hr <- paste0(rep("-", 75), collapse="")
  n <- nrow(get.sdcMicroObj(x, "origData"))
  cn <- colnames(get.sdcMicroObj(x, "origData"))

  TFchange <- TRUE
  if ( identical(obj@risk$individual, obj@originalRisk$individual) ) {
    TFchange <- FALSE
  }

  if ( type == "kAnon" ) {
    txt_f <- paste0("Infos on 2/3-Anonymity:\n\n")
    txt_f <- paste0(txt_f, "Number of observations violating\n")
    txt_f <- paste0(txt_f, "\t- 2-anonymity: ")
    f_2anon <- sum(obj@risk$individual[,2]<2)
    f_2anon_o <- sum(obj@originalRisk$individual[,2]<2)
    f_3anon <- sum(obj@risk$individual[,2]<3)
    f_3anon_o <- sum(obj@originalRisk$individual[,2]<3)
    if ( TFchange ) {
      txt_f <- paste0(txt_f, f_2anon, " (original data: ",f_2anon_o,")\n")
    } else {
      txt_f <- paste0(txt_f, f_2anon_o,"\n")
    }
    txt_f <- paste0(txt_f, "\t- 3-anonymity: ")
    if ( TFchange ) {
      txt_f <- paste0(txt_f, f_3anon, " (original data: ",f_3anon_o,")\n")
    } else {
      txt_f <- paste0(txt_f, f_3anon_o,"\n")
    }
    txt_f <- paste0(txt_f, "\n")

    txt_f <- paste0(txt_f, "Percentage of observations violating\n")
    txt_f <- paste0(txt_f, "\t- 2-anonymity: ")
    if ( TFchange ) {
      f_2anon_o <- sum(obj@originalRisk$individual[,2]<2)
      txt_f <- paste0(txt_f, prettyF(100*f_2anon/n), " % (original data: ",prettyF(100*f_2anon_o/n)," %)\n")
    } else {
      txt_f <- paste0(txt_f, prettyF(100*f_2anon_o/n)," %\n")
    }
    txt_f <- paste0(txt_f, "\t- 3-anonymity: ")
    if ( TFchange ) {
      f_3anon_o <- sum(obj@originalRisk$individual[,2]<3)
      txt_f <- paste0(txt_f, prettyF(100*f_3anon/n), " % (original data: ",prettyF(100*f_3anon_o/n)," %)\n")
    } else {
      txt_f <- paste0(txt_f, prettyF(100*f_3anon_o/n)," %\n")
    }
    txt_f <- paste0(txt_f, hr,"\n\n")
    cat(txt_f)
    invisible(c(f_2anon_o, f_3anon_o))
  }

  if (type == "risk") {
    risk <- obj@risk
    originalRisk <- obj@originalRisk

    # individual risk in original/modified data
    iR_mod <- risk$individual[,1]
    iR_orig <- originalRisk$individual[,1]

    # risky obs
    s_mod <-sum((iR_mod > median(iR_mod) + 2*mad(iR_mod)) & (iR_mod>0.1))
    s_orig <- sum((iR_orig > median(iR_orig) + 2*mad(iR_orig)) & (iR_orig>0.1))

    # expected re-identifications for original/modified data
    exp_ident <- prettyF(round(risk$global$risk_ER, 2), digits=2)
    exp_ident_p <- prettyF(round(risk$global$risk_pct, 2), digits=2)
    exp_ident_o <- prettyF(round(originalRisk$global$risk_ER, 2), digits=2)
    exp_ident_op <- prettyF(round(originalRisk$global$risk_pct, 2), digits=2)

    txt_r <- paste0("Risk measures:\n\n")
    txt_r <- paste0(txt_r, "Number of observations with higher risk than the main part of the data: ")
    if ( TFchange ) {
      txt_r <- paste0(txt_r,"\n\tin modified data: ",s_mod,"\n")
      txt_r <- paste0(txt_r,"\tin original data: ",s_orig,"\n")
    } else {
      txt_r <- paste0(txt_r, s_mod,"\n")
    }
    txt_r <- paste0(txt_r, "Expected number of re-identifications: ")
    if ( TFchange ) {
      txt_r <- paste0(txt_r, "\n\tin modified data: ",exp_ident," (",exp_ident_p," %)\n")
      txt_r <- paste0(txt_r, "\tin original data: ",exp_ident_o," (",exp_ident_op," %)\n")
    } else {
      txt_r <- paste0(txt_r, exp_ident," (",exp_ident_p," %)\n")
    }

    if ( "hier_risk_ER" %in% names(risk$global) ) {
      if ( !is.na(risk$global$hier_risk_ER) ) {
        txt_r <- paste0(txt_r, "\nInformation on hierarchical risk:\n")

        hR <- prettyF(round(risk$global$hier_risk_ER, 2), digits=2)
        hR_p <- prettyF(round(risk$global$hier_risk_pct, 2), digits=2)
        hR_o <- prettyF(round(originalRisk$global$hier_risk_ER,2), digits=2)
        hR_op <- prettyF(round(originalRisk$global$hier_risk_pct, 2), digits=2)

        txt_r <- paste0(txt_r, "Expected number of re-identifications: ")
        if (TFchange) {
          txt_r <- paste0(txt_r, "\n\tin modified data: ",hR," (",hR_p," %)\n")
          txt_r <- paste0(txt_r, "\tin original data: ",hR_o," (",hR_op," %)\n")
        } else {
          txt_r <- paste0(txt_r, hR," (",hR_p," %)\n")
        }
      } else {
        txt_r <- paste0(txt_r, "\tHierarchical risk is not available!\n")
      }
      txt_r <- paste0(txt_r, hr, "\n\n")
    }
    cat(txt_r)
  }

  if (type == "pram") {
    pp <- get.sdcMicroObj(obj, type="pram")
    if ( is.null(pp) ) {
      cat(paste0("PRAM has not been applied!\n"))
      return(invisible(NULL))
    }
    txt_pram <- paste0("Post-Randomization (PRAM):\n")
    txt_pram <- paste0(txt_pram,"\tParameters used: 'pd'=", pp$pd," | ")
    txt_pram <- paste0(txt_pram, "'alpha'=", pp$alpha,"\n\n")
    txt_pram <- paste0(txt_pram, "Number of changed observations:\n")
    cat(txt_pram)
    print(pp$summary, row.names=FALSE)
    cat(hr,"\n\n")
  }

  if (type == "ls") {
    ls <- get.sdcMicroObj(obj, "localSuppression")
    if ( is.null(ls) ) {
      cat(paste0("Local Suppression has not been applied!\n"))
      return(invisible(NULL))
    }
    keyVars <- colnames(obj@manipKeyVars)
    txt_ls <- paste0("Local Suppression")
    supps <- as.data.table(ls$supps)
    if ( nrow(supps) > 1 ) {
      txt_ls <- paste0(txt_ls, " (applied per strata given by variables ", paste0(ls$strataVars, collapse=", "),")\n")
    } else {
      txt_ls <- paste0(txt_ls, ":\n")
    }
    supps_perc <- prettyF(as.numeric(100*(supps[nrow(supps)]/nrow(x@origData))), digits=3)
    out <- data.table(keyVars, xx="|", supps=as.numeric(supps[nrow(supps)]), y="|",supps_perc)
    setnames(out, c("KeyVar", "|","Suppressions (#)","|","Suppressions (%)"))
    cat(txt_ls)
    print(out, row.names=F)
    cat(hr,"\n\n")
  }

  if (type == "recode") {
    df.o <- as.data.table(get.sdcMicroObj(obj, "origData"))
    df.o <- df.o[,get.sdcMicroObj(obj, "keyVars"),with=F]
    df.rec <- as.data.table(get.sdcMicroObj(obj, "manipKeyVars"))

    # number of categories (inkl. NA)
    stats_o <- t(df.o[,lapply(.SD, function(x) {
      c(length(unique(x)), mean(table(x)), sort(table(x))[1])})])
    stats_rec <- t(df.rec[,lapply(.SD, function(x) {
      c(length(unique(x)), mean(table(x)), sort(table(x))[1])})])

    stats_o <- cbind(names(df.o), stats_o)
    stats_rec <- cbind(names(df.rec), stats_rec)

    stats_rec <- cbind(stats_rec[,1], stats_rec[,2], paste0("(",stats_o[,2],")"),
      prettyF(stats_rec[,3]), paste0("(",prettyF(stats_o[,3]),")"),
      stats_rec[,4], paste0("(",stats_o[,4],")"))
    stats_rec <- as.data.table(stats_rec)
    setnames(stats_rec, c("Key Variable","Number of categories","","Mean size","","Size of smallest",""))

    txt_rec <- paste0("Information on categorical Key-Variables:\n")
    txt_rec <- paste0(txt_rec,"\nReported is the number, mean size and size of the smallest category for recoded variables.\n")
    txt_rec <- paste0(txt_rec, "In parenthesis, the same statistics are shown for the unmodified data.\n")
    txt_rec <- paste0(txt_rec, "Note: NA (missings) are counted as seperate categories!\n\n")
    cat(txt_rec)
    print(stats_rec, nrows=nrow(stats_rec), row.names=FALSE)
    cat(hr,"\n\n")
  }

  if (type == "numrisk") {
    risk <- obj@risk
    utility <- obj@utility

    txt_r <- paste0("Numerical key variables: ", paste(cn[x@numVars], collapse = ", "), "\n\n")
    risk_up <- prettyF(round(100 * risk$numeric, 2),2)
    il1 <- prettyF(round(utility$il1, 2),2)
    diff_eigen <- prettyF(round(utility$eigen * 100, 2))

    if (TFchange) {
      txt_r <- paste0(txt_r, "Disclosure risk (~100.00% in original data):\n")
      txt_r <- paste0(txt_r, "\tmodified data: [0.00%; ",risk_up,"%]\n\n")

      txt_r <- paste0(txt_r, "Current Information Loss in modified data (0.00% in original data):\n")
      txt_r <- paste0(txt_r, "\tIL1: ",il1,"\n")
      txt_r <- paste0(txt_r, "\tDifference of Eigenvalues: ",diff_eigen,"%\n")
    } else {
      txt_r <- paste0(txt_r, "Disclosure risk is currently between [0.00%; ",risk_up,"]\n\n")
      txt_r <- paste0(txt_r, "Current Information Loss:\n")
      txt_r <- paste0(txt_r, "\tIL1: ",il1,"\n")
      txt_r <- paste0(txt_r, "\tDifference of Eigenvalues: ",diff_eigen,"%\n")
    }
    txt_r <- paste0(txt_r, hr, "\n\n")
    cat(txt_r)
  }

  if ( type == "general") {
    dims <- dim(get.sdcMicroObj(x, "origData"))
    txt <- paste("Data set with", dims[1], "rows and", dims[2], "columns.\n")

    kV <- get.sdcMicroObj(x, "keyVars")
    if ( length(kV) > 0 ) {
      txt <- paste0(txt, "\t--> Categorical key variables: ", paste(cn[kV], collapse = ", "), "\n")
    }
    nV <- get.sdcMicroObj(x, "numVars")
    if ( length(nV) > 0 ) {
      txt <- paste0(txt, "\t--> Numerical key variables: ", paste(cn[nV], collapse = ", "), "\n")
    }
    wV <- get.sdcMicroObj(x, "weightVar")
    if (length(wV) > 0) {
      txt <- paste0(txt, "\t--> Weight variable: ", cn[wV], "\n")
    }
    sV <- get.sdcMicroObj(x, "strataVar")
    if (length(sV) > 0) {
      txt <- paste0(txt, "\t--> Strata variable(s): ", paste(cn[sV], collapse = ", "), "\n")
    }
    txt <- paste0(txt, hr,"\n\n")
    cat(txt)
  }
})
