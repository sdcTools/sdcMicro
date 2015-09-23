setClass(Class = "reportObj",
  representation = representation(
    title = "characterOrNULL",
    origData = "dataframeOrNULL",
    impFile = "characterOrNULL",
    nrObs = "numericOrNULL",
    importantVariables = "listOrNULL",
    modifications = "listOrNULL",
    pram = "listOrNULL",
    kAnon = "listOrNULL",
    indivRisk = "listOrNULL",
    hierRisk = "listOrNULL",
    riskNumKeyVars = "listOrNULL",
    dataUtility = "listOrNULL",
    localSupps = "listOrNULL",
    dataUtilityCont = "listOrNULL",
    code = "listOrNULL",
    sessionInfo = "listOrNULL"),
  prototype = prototype(
    title = "SDC-Report",
    origData = NULL,
    impFile = NULL,
    nrObs = NULL,
    importantVariables = NULL,
    modifications = NULL,
    pram = NULL,
    kAnon = NULL,
    indivRisk = NULL,
    hierRisk = NULL,
    riskNumKeyVars = NULL,
    dataUtility = NULL,
    localSupps = NULL,
    dataUtilityCont = NULL,
    code = NULL,
    sessionInfo = NULL),
  validity = function(object) {
    return(TRUE)
})

setGeneric("get.reportObj", function(object, type) {
  standardGeneric("get.reportObj")
})

setMethod(f = "get.reportObj", signature = c("reportObj", "character"),
definition = function(object, type) {
  ind <- match(type, slotNames(object))
  if (length(ind) == 1) {
    return(slot(object, type))
  } else {
    stop("get.reportObj:: argument 'type' is not valid!\n")
  }
})

setGeneric("set.reportObj", function(object, type, input) {
  standardGeneric("set.reportObj")
})

setMethod(f = "set.reportObj", signature = c("reportObj", "character", "listOrNULL"),
definition = function(object, type, input) {
  ind <- match(type, slotNames(object))
  if (length(ind) == 1) {
    slot(object, type) <- input[[1]]
  } else {
    stop("set.reportObj:: check argument 'type'!\n")
  }
  validObject(object)
  return(object)
})

setGeneric("calcReportData", function(obj, internal = FALSE, title, outdir) {
  standardGeneric("calcReportData")
})

setMethod(f = "calcReportData", signature = c("sdcMicroObj"),
definition = function(obj, internal, title, outdir) {
  niceF <- function(x) {
    sprintf("%.3f", x)
  }

  repObj <- new("reportObj")
  if (internal) {
    repObj <- set.reportObj(repObj, "pram", list(list()))
    repObj <- set.reportObj(repObj, "kAnon", list(list()))
    repObj <- set.reportObj(repObj, "indivRisk", list(list()))
    repObj <- set.reportObj(repObj, "hierRisk", list(list()))
    repObj <- set.reportObj(repObj, "riskNumKeyVars", list(list()))
    repObj <- set.reportObj(repObj, "dataUtility", list(list()))
    repObj <- set.reportObj(repObj, "localSupps", list(list()))
    repObj <- set.reportObj(repObj, "dataUtilityCont", list(list()))
    repObj <- set.reportObj(repObj, "code", list(list()))
    repObj <- set.reportObj(repObj, "sessionInfo", list(list()))
  }
  repObj <- set.reportObj(repObj, "title", list(title))

  x <- get.sdcMicroObj(obj, type = "origData")
  repObj <- set.reportObj(repObj, "origData", list(x))
  y1 <- get.sdcMicroObj(obj, type = "manipKeyVars")
  y2 <- get.sdcMicroObj(obj, type = "manipNumVars")
  # optionss <- get.sdcMicroObj(obj, type='options')
  optionss <- obj@options

  ##################### imported File ###
  if ("filename" %in% names(optionss)) {
    repObj <- set.reportObj(repObj, "impFile", list(optionss$filename))
  }

  cn <- colnames(x)
  y1cn <- get.sdcMicroObj(obj, type = "keyVars"); y1cn <- cn[y1cn]
  y2cn <- get.sdcMicroObj(obj, type = "numVars"); y2cn <- cn[y2cn]

  hhid <- get.sdcMicroObj(obj, type = "hhId")
  hhIdcn <- ifelse(length(hhid)>0, cn[hhid], "not defined")

  strataV <- get.sdcMicroObj(obj, type = "strataVar")
  stratacn <- ifelse(length(strataV)>0, cn[strataV], "not defined")

  n <- nrow(x)
  pCat <- length(get.sdcMicroObj(obj, "keyVars"))

  wind <- get.sdcMicroObj(obj, type = "weightVar")
  weightcn <- ifelse(length(wind)>0, cn[wind], "not defined")

  senscn <- get.sdcMicroObj(obj, "sensibleVar")
  if ( !is.null(senscn)) {
    senscn <- cn[senscn]
  }

  ## information on categorical key-variables
  repObj <- set.reportObj(repObj, "nrObs", list(nrow(x)))

  importantVariables <- list()
  importantVariables$catVars <- y1cn
  importantVariables$numVars <- y2cn
  importantVariables$weightVar <- weightcn
  importantVariables$hhId <- hhIdcn
  importantVariables$strataVars <- stratacn
  importantVariables$sensibleVar <- senscn
  repObj <- set.reportObj(repObj, "importantVariables", list(importantVariables))

  ## information about anonymisation methods
  delDirect <- get.sdcMicroObj(obj, "deletedVars")

  modCat <- sum(!(x[, get.sdcMicroObj(obj, "keyVars")] == get.sdcMicroObj(obj, "manipKeyVars")),na.rm = TRUE) > 0
  if ( is.null(get.sdcMicroObj(obj, "manipNumVars"))) {
    modNum <- NA
  } else {
    modNum <- sum(!(x[, get.sdcMicroObj(obj, "numVars")] == get.sdcMicroObj(obj, "manipNumVars")),na.rm = TRUE) > 0
  }
  modPram <- !is.null(get.sdcMicroObj(obj, "pram"))
  modLocSupp <- !is.null(get.sdcMicroObj(obj, "localSuppression"))

  modifications <- list(delDirect=delDirect,modCat=modCat,modNum=modNum,modPram=modPram,modLocSupp=modLocSupp)
  repObj <- set.reportObj(repObj, "modifications", list(modifications))

  ## information about disclosure risk and k-anonymity
  pram <- get.sdcMicroObj(obj, "pram")
  if ( is.null(pram) ) {
    repObj <- set.reportObj(repObj, "pram", list(NULL))
  }
  if ( is.null(get.sdcMicroObj(obj, "pramVars")) ) {
    repObj <- set.reportObj(repObj, "pram", list(NULL))
  }
  if ( is.list(get.reportObj(repObj, "pram")) ) {
    pramOut <- list()
    changedVars <- list()
    vNames.pram <- colnames(x)[obj@pramVars]
    vNames.key <- colnames(x)[obj@keyVars]
    for (i in 1:length(vNames.pram)) {
      changedVars[[i]] <- list()
      ind <- which(vNames.pram[i] == vNames.key)
      if (length(ind) == 1) {
        s <- sum(as.character(x[, vNames.pram[i]]) != as.character(get.sdcMicroObj(obj,
          "manipKeyVars")[, vNames.key[ind]]), na.rm = TRUE)
        p <- round(s/nrow(x) * 100, 2)
      } else {
        s <- sum(as.character(x[, vNames.pram[i]]) != as.character(get.sdcMicroObj(obj,
          "manipPramVars")[, vNames.pram[i]]), na.rm = TRUE)
        p <- round(s/nrow(x) * 100, 2)
      }
      changedVars[[i]]$oName <- vNames.pram[i]
      changedVars[[i]]$nr <- s
      changedVars[[i]]$perc <- p
    }
    totChanges <- sum(sapply(changedVars, function(x) x$nr))
    percChanges <- 100 * totChanges/(n * length(vNames.pram))

    pramOut$changedVars <- changedVars
    pramOut$totChanges <- totChanges
    pramOut$percChanges <- niceF(percChanges)
    repObj <- set.reportObj(repObj, "pram", list(pramOut))
  }

  ## k-anonymity if we pram, kAnon is useless
  if ( is.list(get.reportObj(repObj, "pram")) ) {
    repObj <- set.reportObj(repObj, "kAnon", list(NULL))
  }

  if ( is.list(get.reportObj(repObj, "kAnon")) ) {
    kAnon <- list()
    n <- nrow(x)
    kAnon$anon2 <- sum(obj@risk$individual[, 2] < 2)
    kAnon$anon2p <- niceF((kAnon$anon2 / n) * 100)
    kAnon$anon2o <- sum(obj@originalRisk$individual[,2] < 2)
    kAnon$anon2op <- niceF((kAnon$anon2o / n) * 100)
    kAnon$anon3 <- sum(obj@risk$individual[, 2] < 3)
    kAnon$anon3p <- niceF((kAnon$anon3 / n) * 100)
    kAnon$anon3o <- sum(obj@originalRisk$individual[,2] < 3)
    kAnon$anon3op <- niceF((kAnon$anon3o / n) * 100)
    repObj <- set.reportObj(repObj, "kAnon", list(kAnon))
  }

  ## information about individual risk
  if ( is.list(get.reportObj(repObj, "indivRisk")) ) {
    indivRisk <- list()
    risk <- get.sdcMicroObj(obj, type="risk")

    indivRisk$expRI <- risk$global$risk * nrow(x)
    indivRisk$expRIp <- niceF(risk$global$risk_pct)
    indivRisk$expRI <- niceF(indivRisk$expRI)

    ## TODO: save risk of original data and not renewly calculate it:
    if ( weightcn == "not defined" ) {
      weightcn <- NULL
    }
    ro <- measure_risk(x, keyVars = y1cn, w = weightcn)
    indivRisk$expRIop <- ro$global_risk_pct
    indivRisk$expRIo <- indivRisk$expRIop/100 * nrow(x)
    indivRisk$expRIop <- niceF(indivRisk$expRIop)
    indivRisk$expRIo <- niceF(indivRisk$expRIo)

    # 10 combinations with highest risks
    or <- cbind(y1, risk$individual)
    index <- apply(y1, 1, paste, collapse = "")
    or <- or[!duplicated(index), ]
    or <- or[order(or$risk, decreasing = TRUE), ]
    indivRisk$highest <- or[1:10, ]
    indivRisk$highest$risk <- niceF(indivRisk$highest$risk)
    repObj <- set.reportObj(repObj, "indivRisk", list(indivRisk))
  }

  ## information about hierarchical risk
  if (!"hier_risk_ER" %in% names(obj@risk$global)) {
    repObj <- set.reportObj(repObj, "hierRisk", list(NULL))
  }

  if ( is.list(get.reportObj(repObj, "hierRisk")) ) {
    if (is.na(obj@risk$global$hier_risk_ER)) {
      repObj <- set.reportObj(repObj, "hierRisk", list(NULL))
    }
  }

  if ( is.list(get.reportObj(repObj, "hierRisk")) ) {
    hierRisk <- list()
    hierRisk$expReident <- niceF(obj@risk$global$hier_risk_ER)
    hierRisk$expReidentp <- niceF(obj@risk$global$hier_risk_pct)
    hierRisk$expReidento <- niceF(obj@originalRisk$global$hier_risk_ER)
    hierRisk$expReidentop <- niceF(obj@originalRisk$global$hier_risk_pct)
    repObj <- set.reportObj(repObj, "hierRisk", list(hierRisk))
  }

  ## information about disclosure risk on continuous key variables
  risknum <- get.sdcMicroObj(obj, "risk")$numeric
  riskNumKeyVars <- list()
  if ( is.null(get.sdcMicroObj(obj, "manipNumVars")) ) {
    repObj <- set.reportObj(repObj, "riskNumKeyVars", list(NULL))
  } else {
    riskNumKeyVars <- list()
    riskNumKeyVars$risk <- niceF(100 * obj@risk$numeric)
    repObj <- set.reportObj(repObj, "riskNumKeyVars", list(riskNumKeyVars))
  }

  ## information about data-utility
  if ( is.list(get.reportObj(repObj, "dataUtility")) ) {
    dU <- list()
    for (i in y1cn) {
      dU[[i]] <- list()
      dat_o <- as.data.frame.table(table(x[[i]], useNA="ifany"))
      dat_m <- as.data.frame.table(table(factor(y1[[i]]), useNA="ifany"))

      df <- merge(dat_o, dat_m, by="Var1", all.x=TRUE, all.y=TRUE)
      names(df) <- c("Categories","Original data", "Modified data")
      df$Categories <- as.character(df$Categories)
      df$Categories[is.na(df$Categories)] <- "NA"
      dU[[i]]$title <- i
      dU[[i]]$tab <- df
    }
    repObj <- set.reportObj(repObj, "dataUtility", list(dU))
  }

  ## Local Suppressions
  if ( is.list(get.reportObj(repObj, "localSupps")) ) {
    df <- as.data.frame(get.sdcMicroObj(obj, "localSuppression")$supps)
    if ( nrow(df) == 0 ) {
      repObj <- set.reportObj(repObj, "localSupps", list(NULL))
    } else {
      df <- rbind(df, 100*(df[1,]/nrow(x)))
      df[1,] <- sprintf("%1.0f", df[1,])
      df[2,] <- sprintf("%.3f", df[2,])
      rownames(df) <- c("Number of Suppression", "Percentage")
      localSupps <- list()
      localSupps$tab <- df
      repObj <- set.reportObj(repObj, "localSupps", list(localSupps))
    }
  }

  ## dataUtility for continuous variables
  dataUtilityCont_show <- !(is.null(y2) || ncol(y2) == 0) & is.list(get.reportObj(repObj, "dataUtilityCont"))
  if ( dataUtilityCont_show ) {
    dataUtilityCont <- list()
    s <- apply(na.omit(x[, y2cn, drop = FALSE]), 2, summary)
    ss <- apply(na.omit(y2), 2, function(x) round(summary(x), 1))

    tabSummary <- lapply(1:ncol(ss), function(x) {
      xx <- data.frame(s[,x,drop=F], ss[,x,drop=F])
      xx$diff <- xx[,1]-xx[,2]
      colnames(xx) <- c("Original", "Modified","Difference")
      xx
    })
    names(tabSummary) <- colnames(ss)
    dataUtilityCont$tabSummary <- tabSummary

    dataUtilityCont$IL1 <- niceF(obj@utility$il1)
    dataUtilityCont$diffEigen <- niceF(obj@utility$eigen*100)
    dataUtilityCont$boxplotData <- list(orig=x[, y2cn, drop = FALSE], modified=y2)
    repObj <- set.reportObj(repObj, "dataUtilityCont", list(dataUtilityCont))
  }

  ## R-code
  if ( "cmd" %in% names(optionss) & is.list(get.reportObj(repObj, "code")) ) {
    repObj <- set.reportObj(repObj, "code", list(as.list(obj@options$cmd)))
  }

  ## Information about current R-session
  if ( !is.null(get.reportObj(repObj, "sessionInfo")) ) {
    sessionInfo <- list()
    sI <- sessionInfo()
    # 1) R-Version
    sessionInfo$version <- sI$R.version$version.string

    # 2) Platform
    sessionInfo$platform <- sI$R.version$platform

    # 3) base-Packages
    sessionInfo$basePgks <- sI$basePkgs

    # 4) other loaded packages
    sessionInfo$otherPkgs <- as.character(sapply(sI$otherPkgs, function(x) {
      paste0(x$Package," (",x$Version,")")
    }))

    # 5) packages that are only attached
    sessionInfo$loaded <- as.character(sapply(sI$loadedOnly, function(x) {
      paste0(x$Package," (",x$Version,")")
    }))

    # 6) Localization
    sessionInfo$loc <- unlist(strsplit(sI$locale, ";"))

    repObj <- set.reportObj(repObj, "sessionInfo", list(sessionInfo))
  }
  return(repObj)
})

#' Generate a HTML/LATEX output from an sdcMicroObj
#'
#' Summary statistics of the original and the perturbed data set
#'
#' The application of this function provides you with a html, text or
#' pdf-report for your sdcMicro object that contains useful summaries about the
#' anonymization process.
#'
#' @name report
#' @aliases report-methods report report,sdcMicroObj-method
#' @docType methods
#' @param obj an object of class \code{\link{sdcMicroObj-class}} or 'reportObj'
#' @param outdir output folder
#' @param filename output filename
#' @param format HTML, TEXT or LATEX
#' @param title Title for the report
#' @param internal TRUE/FALSE, if TRUE a detailled internal report is produced,
#' else a non-disclosive overview
#' @author Matthias Templ, Bernhard Meindl
#' @keywords methods
#' @export
#' @examples
#'
#' \dontrun{
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' report(sdc)
#' }
#'
setGeneric("report", function(obj, outdir = getwd(), filename = "SDC-Report",
  format = "HTML", title = "SDC-Report", internal = FALSE) {
  standardGeneric("report")
})

setMethod(f = "report", signature = c("sdcMicroObj"),
definition = function(obj, outdir = getwd(), filename = "SDC-Report",
  format = "HTML", title = "SDC-Report", internal = FALSE) {

  if (!format %in% c("HTML", "LATEX","TEXT")) {
    stop("possible values for 'type' are 'HTML','LATEX' and 'TEXT'!\n")
  }

  if ( format=="TEXT" ) {
    msg <- "Please note for sdcMicro > 4.5.0 this option has changed.\n"
    msg <- paste0(msg, "You will get receive an html-output!\n")
    message(msg)
  }

  if ( format %in% c("HTML","TEXT") ) {
    filename <- paste(filename, ".html", sep = "")
  }
  if ( format == "LATEX" ) {
    filename <- paste(filename, ".pdf", sep = "")
  }

  repObj <- calcReportData(obj, internal = internal, title = title, outdir = outdir)
  fTemplate <- system.file("templates", "report-template.rmd", package="sdcMicro")
  if ( format == "HTML" ) {
    render(fTemplate, quiet=TRUE,output_format="html_document", output_dir=outdir, output_file=filename)
  }
  if ( format == "LATEX" ) {
    tryCatch(render(fTemplate, quiet=TRUE,output_format="pdf_document", output_dir=outdir, output_file=filename), error = function(e) {
      cat("It was not possible to produce a pdf-output. Please generate the report using format='HTML'\n")
    }, finally = TRUE)
  }

  if ( internal ) {
    txt <- paste0("An internal (extensive) report was successfully generated.\n")
  } else {
    txt <- paste0("An short report was successfully generated.\n")
  }
  txt <- paste0(txt, "It was saved in '", outdir,"/",filename,"'.\n")
  cat(txt)
})
