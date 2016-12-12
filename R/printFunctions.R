# format as string with given number of digits
prettyF <- function(inp, digits=3) {
  formatC(as.numeric(inp), format="f", digits=digits)
}

#' Freq
#'
#' Extract sample frequency counts (fk) or estimated population frequency counts (Fk)
#'
#' @param obj an \code{\link{sdcMicroObj-class}}-object
#' @param type either \code{'fk'} or \code{'FK'}
#'
#' @return a vector containing sample frequencies or weighted frequencies
#' @export
#' @examples
#' data(testdata)
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('urbrur','roof','walls','relat','sex'),
#'   pramVars=c('water','electcon'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' head(freq(sdc, type="fk"))
#' head(freq(sdc, type="Fk"))
freq <- function(obj, type="fk") {
  freqX(obj, type=type)
}

setGeneric("freqX", function(obj, type="fk") {
  standardGeneric("freqX")
})

setMethod(f="freqX", signature=c("sdcMicroObj"),
definition = function(obj, type="fk") {
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
#' @aliases print,sdcMicroObj-method
#' @docType methods
#' @param x An object of class \code{\link{sdcMicroObj-class}}
#' @param type Selection of the content to be returned or printed
#' @param docat logical, if TRUE (default) the results will be actually printed
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
#' @author Alexander Kowarik, Matthias Templ, Bernhard Meindl
#' @keywords classes
#' @export
#' @examples
#' data(testdata)
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('urbrur','roof','walls','relat','sex'),
#'   pramVars=c('water','electcon'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- microaggregation(sdc, method="mdav", aggr=3)
#' print(sdc)
#' print(sdc, type="general")
#' print(sdc, type="ls")
#' print(sdc, type="recode")
#' print(sdc, type="risk")
#' print(sdc, type="numrisk")
#' print(sdc, type="pram")
#' print(sdc, type="kAnon")
#' print(sdc, type="comp_numvars")
setMethod(f = "print", signature = c("sdcMicroObj"),
definition = function(x, type = "kAnon", docat=TRUE, ...) {
  if (!type %in% c("kAnon","ls","pram","recode","risk","numrisk","general", "comp_numvars")) {
    stop(paste("type=", type, "is unknown."))
  }

  # type=kAnon,ls,recode,risk,numrisk,general
  obj <- x
  hr <- paste0(rep("-", 70), collapse="")
  tabsp <- "  "
  n <- nrow(get.sdcMicroObj(x, "origData"))
  cn <- colnames(get.sdcMicroObj(x, "origData"))

  TFchange <- TRUE
  if ( identical(obj@risk$individual, obj@originalRisk$individual) ) {
    TFchange <- FALSE
  }

  if (type=="kAnon") {
    txt_f <- paste0("Infos on 2/3-Anonymity:\n\n")
    txt_f <- paste0(txt_f, "Number of observations violating\n")
    txt_f <- paste0(txt_f, tabsp,"- 2-anonymity: ")
    f_2anon <- sum(obj@risk$individual[,2]<2)
    f_2anon_o <- sum(obj@originalRisk$individual[,2]<2)

    f_3anon <- sum(obj@risk$individual[,2]<3)
    f_3anon_o <- sum(obj@originalRisk$individual[,2]<3)

    f_5anon <- sum(obj@risk$individual[,2]<5)
    f_5anon_o <- sum(obj@originalRisk$individual[,2]<5)

    f_2anon_p <- prettyF(100*f_2anon/n); f_2anon_op <- prettyF(100*f_2anon_o/n)
    f_3anon_p <- prettyF(100*f_3anon/n); f_3anon_op <- prettyF(100*f_3anon_o/n)
    f_5anon_p <- prettyF(100*f_5anon/n); f_5anon_op <- prettyF(100*f_5anon_o/n)

    if (TFchange) {
      txt_f <- paste0(txt_f, f_2anon, " (",f_2anon_p,"%) | in original data: ",f_2anon_o," (",f_2anon_op,"%)\n")
    } else {
      txt_f <- paste0(txt_f, f_2anon_o," (",f_2anon_op,"%)\n")
    }
    txt_f <- paste0(txt_f, tabsp,"- 3-anonymity: ")
    if (TFchange) {
      txt_f <- paste0(txt_f, f_3anon, " (",f_3anon_p,"%) | in original data: ",f_3anon_o," (",f_3anon_op,"%)\n")
    } else {
      txt_f <- paste0(txt_f, f_3anon_o," (",f_3anon_op,"%)\n")
    }
    txt_f <- paste0(txt_f, tabsp,"- 5-anonymity: ")
    if (TFchange) {
      txt_f <- paste0(txt_f, f_5anon, " (",f_5anon_p,"%) | in original data: ",f_5anon_o," (",f_5anon_op,"%)\n")
    } else {
      txt_f <- paste0(txt_f, f_5anon_o," (",f_5anon_op,"%)\n")
    }
    txt_f <- paste0(txt_f, "\n",hr,"\n\n")
    if (docat) {
      cat(txt_f)
    }
    return(invisible(list(
      "2anon"=list(orig=f_2anon_o, orig_p=f_2anon_op, mod=f_2anon, mod_p=f_2anon_p),
      "3anon"=list(orig=f_3anon_o, orig_p=f_3anon_op, mod=f_3anon, mod_p=f_3anon_p),
      "5anon"=list(orig=f_5anon_o, orig_p=f_5anon_op, mod=f_5anon, mod_p=f_5anon_p))))
  }

  if (type=="risk") {
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
      txt_r <- paste0(txt_r,"\n",tabsp,"in modified data: ",s_mod,"\n")
      txt_r <- paste0(txt_r,tabsp,"in original data: ",s_orig,"\n")
    } else {
      txt_r <- paste0(txt_r, s_mod,"\n")
    }
    txt_r <- paste0(txt_r, "Expected number of re-identifications: ")
    if (TFchange) {
      txt_r <- paste0(txt_r, "\n",tabsp,"in modified data: ",exp_ident," (",exp_ident_p," %)\n")
      txt_r <- paste0(txt_r, tabsp,"in original data: ",exp_ident_o," (",exp_ident_op," %)\n")
    } else {
      txt_r <- paste0(txt_r, exp_ident," (",exp_ident_p," %)\n")
    }

    if ("hier_risk_ER" %in% names(risk$global)) {
      if (!is.na(risk$global$hier_risk_ER)) {
        txt_r <- paste0(txt_r, "\nInformation on hierarchical risk:\n")

        hR <- prettyF(round(risk$global$hier_risk_ER, 2), digits=2)
        hR_p <- prettyF(round(risk$global$hier_risk_pct, 2), digits=2)
        hR_o <- prettyF(round(originalRisk$global$hier_risk_ER,2), digits=2)
        hR_op <- prettyF(round(originalRisk$global$hier_risk_pct, 2), digits=2)

        txt_r <- paste0(txt_r, "Expected number of re-identifications: ")
        if (TFchange) {
          txt_r <- paste0(txt_r, "\n",tabsp,"in modified data: ",hR," (",hR_p," %)\n")
          txt_r <- paste0(txt_r, tabsp,"in original data: ",hR_o," (",hR_op," %)\n")
        } else {
          txt_r <- paste0(txt_r, hR," (",hR_p," %)\n")
        }
      } else {
        txt_r <- paste0(txt_r, tabsp, "Hierarchical risk is not available!\n")
      }
      txt_r <- paste0(txt_r, hr, "\n\n")
    }
    if (docat) {
      cat(txt_r)
    }
    out <- list(
      "riskyObs"=list(orig=s_orig, mod=s_mod),
      "reident"=list(orig=exp_ident_o, orig_p=exp_ident_op, mod=exp_ident, mod_p=exp_ident_p))
    if ("hier_risk_ER" %in% names(risk$global)) {
      out <- list(out,
        "hierRisk"=list(orig=hR_o, orig_p=hR_op, mod=hR, mod_p=hR_p)
      )
    } else {
      out <- list(out, "hierRisk"=NA)
    }
    return(invisible(out))
  }

  if (type=="pram") {
    pp <- get.sdcMicroObj(obj, type="pram")
    if (is.null(pp)) {
      if (docat) {
        cat(paste0("PRAM has not been applied!\n"))
      }
      return(invisible(NULL))
    }
    ss <- pp$summary
    params <- pp$params
    if (docat) {
      cat("Post-Randomization (PRAM):\n")
      for (i in 1:nrow(ss)) {
        cat("Variable:",ss$variable[i],"\n")
        cat("--> final Transition-Matrix:\n")
        print(params[[i]]$Rs)
      }
      cat("\nChanged observations:\n")
      print(ss)
      cat(hr,"\n\n")
    }
    return(invisible(list(
      "pram_summary"=ss,
      "params"=params
    )))
  }

  if (type=="ls") {
    ls <- get.sdcMicroObj(obj, "localSuppression")
    if (is.null(ls)) {
      if (docat) {
        cat(paste0("Local suppression has not been applied!\n"))
      }
      return(invisible(NULL))
    }
    keyVars <- colnames(obj@manipKeyVars)
    txt_ls <- paste0("Local suppression")
    supps <- as.data.table(ls$supps)
    suppsT <- as.data.table(ls$totalSupps)
    if (nrow(supps) > 1) {
      txt_ls <- paste0(txt_ls, " (applied per strata given by variable(s) ", paste0(ls$strataVars, collapse=", "),")\n")
    } else {
      txt_ls <- paste0(txt_ls, ":\n")
    }
    supps_perc <- prettyF(as.numeric(100*(supps[nrow(supps)]/nrow(obj@origData))), digits=3)
    supps_percT <- prettyF(as.numeric(100*(suppsT[nrow(suppsT)]/nrow(obj@origData))), digits=3)

    out <- data.table(keyVars, xx="|", supps=as.integer(supps[nrow(supps)]), y="|",supps_perc)
    setnames(out, c("KeyVar", "|","Suppressions (#)","|","Suppressions (%)"))
    outT <- data.table(keyVars, supps=as.integer(suppsT[nrow(suppsT)]), supps_percT)
    setnames(outT, c("KeyVar", "Suppressions (#)", "Suppressions (%)"))

    if (docat) {
      cat(txt_ls)
      print(out, row.names=F)
      cat(hr,"\n\n")
    }
    return(invisible(list(
      "supps"=out[,c(1,3,5), with=FALSE],
      "suppsT"=outT,
      "threshold"=ls$threshold,
      "strataVars"=ls$strataVars
    )))
  }

  if (type=="recode") {
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

    txt_rec <- paste0("Information on categorical key variables:\n")
    txt_rec <- paste0(txt_rec,"\nReported is the number, mean size and size of the smallest category for recoded variables.\n")
    txt_rec <- paste0(txt_rec, "In parenthesis, the same statistics are shown for the unmodified data.\n")
    txt_rec <- paste0(txt_rec, "Note: NA (missings) are counted as seperate categories!\n\n")

    if (docat) {
      cat(txt_rec)
      print(stats_rec, nrows=nrow(stats_rec), row.names=FALSE)
      cat(hr,"\n\n")
    }
    return(invisible(list(
      "keyVars"=stats_rec[[1]],
      "categories"=list(orig=stats_rec[[2]], mod=stats_rec[[3]]),
      "meansize"=list(orig=stats_rec[[4]], mod=stats_rec[[5]]),
      "minsize"=list(orig=stats_rec[[6]], mod=stats_rec[[7]])
    )))
  }

  if (type=="numrisk") {
    if (length(obj@numVars)==0) {
      return(NULL)
    }
    risk <- obj@risk
    utility <- obj@utility

    txt_r <- paste0("Numerical key variables: ", paste(cn[x@numVars], collapse = ", "), "\n\n")
    risk_up <- prettyF(round(100 * risk$numeric, 2),2)

    if (!is.null(utility)) {
      il1 <- prettyF(round(utility$il1, 2),2)
      diff_eigen <- prettyF(round(utility$eigen * 100, 2))
    } else {
      il1 <- diff_eigen <- NA
    }

    if (TFchange) {
      txt_r <- paste0(txt_r, "Disclosure risk (~100.00% in original data):\n")
      txt_r <- paste0(txt_r, tabsp,"modified data: [0.00%; ",risk_up,"%]\n\n")
      if (!is.null(utility)) {
        txt_r <- paste0(txt_r, "Current Information Loss in modified data (0.00% in original data):\n")
        txt_r <- paste0(txt_r, tabsp, "IL1: ",il1,"\n")
        txt_r <- paste0(txt_r, tabsp, "Difference of Eigenvalues: ",diff_eigen,"%\n")
      }
    } else {
      txt_r <- paste0(txt_r, "Disclosure risk is currently between [0.00%; ",risk_up,"%]\n\n")
      if (!is.null(utility)) {
        txt_r <- paste0(txt_r, "Current Information Loss:\n")
        txt_r <- paste0(txt_r, tabsp, "- IL1: ",il1,"\n")
        txt_r <- paste0(txt_r, tabsp, "- Difference of Eigenvalues: ",diff_eigen,"%\n")
      }
    }
    txt_r <- paste0(txt_r, hr, "\n\n")
    if (docat) {
      cat(txt_r)
    }
    return(invisible(list(
      "numVars"=cn[x@numVars],
      "riskOrig"=100,
      "risk_up"=risk_up,
      "il1"=il1,
      "diff_eigen"=diff_eigen
    )))
  }

  if (type=="general") {
    dims <- dim(get.sdcMicroObj(x, "origData"))
    txt <- paste("The input dataset consists of", dims[1], "rows and", dims[2], "variables.\n")

    delVars <- x@deletedVars
    if (!is.null(delVars)) {
      txt <- paste0(txt, "\nThe following variables have been deleted are not available in the output dataset:\n")
      for (i in 1:length(delVars)) {
        txt <- paste0(txt, tabsp, "--> ", delVars[i],"\n")
      }
      txt <- paste0(txt, "\n\n")
    }

    kV <- get.sdcMicroObj(x, "keyVars")
    if (length(kV) > 0) {
      txt <- paste0(txt, tabsp, "--> Categorical key variables: ", paste(cn[kV], collapse = ", "), "\n")
    }
    nV <- get.sdcMicroObj(x, "numVars")
    if (length(nV) > 0) {
      txt <- paste0(txt, tabsp, "--> Numerical key variables: ", paste(cn[nV], collapse = ", "), "\n")
    }
    wV <- get.sdcMicroObj(x, "weightVar")
    if (length(wV) > 0) {
      txt <- paste0(txt, tabsp, "--> Weight variable: ", cn[wV], "\n")
    }
    sV <- get.sdcMicroObj(x, "strataVar")
    if (length(sV) > 0) {
      txt <- paste0(txt, tabsp, "--> Strata variable(s): ", paste(cn[sV], collapse = ", "), "\n")
    }
    hhV <- get.sdcMicroObj(x, "hhId")
    if (length(hhV) > 0) {
      txt <- paste0(txt, tabsp, "--> Cluster/Household-Id variable: ", cn[hhV], "\n")
    }
    gV <- get.sdcMicroObj(x, "ghostVars")
    if (length(gV) > 0) {
      txt <- paste0(txt, tabsp, "--> Ghost variable(s) exist\n")
      for (i in 1:length(gV)) {
        cur_kv <- cn[gV[[i]][[1]]]
        cur_gv <- cn[gV[[i]][[2]]]
        txt <- paste0(txt, tabsp,tabsp,"Variable(s) ",paste(cur_gv, collapse = ", ")," are linked to key variable ",cur_kv,"\n")
        gV[[i]][[1]] <- cur_kv
        gV[[i]][[2]] <- cur_gv
      }
    }
    txt <- paste0(txt, hr,"\n\n")
    if (docat) {
      cat(txt)
    }
    return(invisible(list(
      "dims"=dims,
      "delVars"=delVars,
      "keyVars"=cn[kV],
      "numVars"=cn[nV],
      "weightVar"=cn[wV],
      "strataVar"=cn[sV],
      "householdId"=cn[hhV],
      "ghostVars"=gV
    )))
  }

  if (type=="comp_numvars") {
    orig <- get.sdcMicroObj(x, type="origData")
    numvars <- colnames(orig)[get.sdcMicroObj(x, type="numVars")]
    if (length(numvars) == 0) {
      return(invisible(NULL))
    }
    nv_o <- orig[, numvars, drop=F]
    nv_m <- get.sdcMicroObj(x, "manipNumVars")
    if (docat) {
      cat("Compare original and modified numeric key variables\n\n")
    }
    out <- list(
      "numVars"=numvars,
      "results"=list()
    )
    for (i in 1:ncol(nv_o)) {
      if (identical(nv_o[,i], nv_m[,i])) {
        if (docat) {
          cat(paste0(tabsp,"Variable ", shQuote(numvars[i])," has not been modified.\n\n"))
        }
      } else {
        summary_o <- summary(nv_o[,i])
        summary_m <- summary(as.numeric(nv_m[,i]))
        val_cor <- cor(nv_o[,i], nv_m[,i], use="pairwise.complete.obs")
        dt <- as.data.table(rbind(c("orig",as.numeric(summary_o)), c("modified",as.numeric(summary_m))))
        setnames(dt, c("Type",names(summary_o)))
        if (docat) {
          cat(paste0(tabsp,"Variable ", shQuote(numvars[i])," has been modified. The correlation is ",prettyF(val_cor),"\n\n"))
          print(dt)
          cat("\n")
        }
        out$results[[length(out$results)+1]] <- dt
      }
    }
    if (docat) {
      cat(hr,"\n\n")
    }
    return(invisible(out))
  }
})
