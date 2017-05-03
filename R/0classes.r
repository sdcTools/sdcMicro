### class sdcMicroObj ###
#all <- setdiff(getNamespaceExports("shiny"), c("renderDataTable", "dataTableOutput"))
#cat(paste("#' @importFrom shiny", all), sep="\n")

#' @useDynLib sdcMicro, .registration=TRUE
#' @import methods
#' @import Rcpp
#' @import robustbase
#' @import MASS
#' @import car
#' @import cluster
#' @import e1071
#' @import tools
#' @import knitr
#' @import xtable
#' @import data.table
#' @import ggplot2
#' @import shinyBS
#' @importFrom shiny passwordInput
#' @importFrom shiny includeHTML
#' @importFrom shiny knit_print.shiny.tag
#' @importFrom shiny splitLayout
#' @importFrom shiny fixedPage
#' @importFrom shiny bookmarkButton
#' @importFrom shiny setBookmarkExclude
#' @importFrom shiny tabsetPanel
#' @importFrom shiny animationOptions
#' @importFrom shiny icon
#' @importFrom shiny shinyUI
#' @importFrom shiny code
#' @importFrom shiny outputOptions
#' @importFrom shiny actionButton
#' @importFrom shiny freezeReactiveValue
#' @importFrom shiny updateDateInput
#' @importFrom shiny verticalLayout
#' @importFrom shiny registerInputHandler
#' @importFrom shiny insertUI
#' @importFrom shiny updateCheckboxInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny plotPNG
#' @importFrom shiny bootstrapPage
#' @importFrom shiny reactivePoll
#' @importFrom shiny imageOutput
#' @importFrom shiny includeScript
#' @importFrom shiny fileInput
#' @importFrom shiny reactiveTable
#' @importFrom shiny knit_print.reactive
#' @importFrom shiny installExprFunction
#' @importFrom shiny updateTextAreaInput
#' @importFrom shiny renderPlot
#' @importFrom shiny knit_print.shiny.render.function
#' @importFrom shiny textAreaInput
#' @importFrom shiny selectInput
#' @importFrom shiny shinyServer
#' @importFrom shiny setProgress
#' @importFrom shiny removeUI
#' @importFrom shiny removeModal
#' @importFrom shiny sidebarPanel
#' @importFrom shiny ..stacktraceon..
#' @importFrom shiny exprToFunction
#' @importFrom shiny fluidPage
#' @importFrom shiny runGist
#' @importFrom shiny updateSelectizeInput
#' @importFrom shiny updateQueryString
#' @importFrom shiny runUrl
#' @importFrom shiny submitButton
#' @importFrom shiny fillCol
#' @importFrom shiny as.shiny.appobj
#' @importFrom shiny includeMarkdown
#' @importFrom shiny safeError
#' @importFrom shiny reactiveText
#' @importFrom shiny withReactiveDomain
#' @importFrom shiny runGadget
#' @importFrom shiny addResourcePath
#' @importFrom shiny fillRow
#' @importFrom shiny textInput
#' @importFrom shiny pageWithSidebar
#' @importFrom shiny fixedRow
#' @importFrom shiny conditionStackTrace
#' @importFrom shiny downloadLink
#' @importFrom shiny invalidateLater
#' @importFrom shiny stopApp
#' @importFrom shiny dateInput
#' @importFrom shiny tags
#' @importFrom shiny onBookmark
#' @importFrom shiny ..stacktraceoff..
#' @importFrom shiny brushedPoints
#' @importFrom shiny basicPage
#' @importFrom shiny markRenderFunction
#' @importFrom shiny runApp
#' @importFrom shiny serverInfo
#' @importFrom shiny renderPrint
#' @importFrom shiny navbarPage
#' @importFrom shiny dblclickOpts
#' @importFrom shiny onReactiveDomainEnded
#' @importFrom shiny enableBookmarking
#' @importFrom shiny printStackTrace
#' @importFrom shiny is.singleton
#' @importFrom shiny observeEvent
#' @importFrom shiny showReactLog
#' @importFrom shiny modalButton
#' @importFrom shiny parseQueryString
#' @importFrom shiny isTruthy
#' @importFrom shiny req
#' @importFrom shiny withTags
#' @importFrom shiny createWebDependency
#' @importFrom shiny img
#' @importFrom shiny extractStackTrace
#' @importFrom shiny selectizeInput
#' @importFrom shiny removeNotification
#' @importFrom shiny getShinyOption
#' @importFrom shiny reactivePlot
#' @importFrom shiny tagAppendAttributes
#' @importFrom shiny repeatable
#' @importFrom shiny navbarMenu
#' @importFrom shiny knit_print.shiny.tag.list
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shiny formatStackTrace
#' @importFrom shiny shinyAppFile
#' @importFrom shiny updateDateRangeInput
#' @importFrom shiny reactive
#' @importFrom shiny knit_print.html
#' @importFrom shiny paneViewer
#' @importFrom shiny tagAppendChild
#' @importFrom shiny is.reactive
#' @importFrom shiny column
#' @importFrom shiny withMathJax
#' @importFrom shiny reactiveValues
#' @importFrom shiny runGitHub
#' @importFrom shiny is.reactivevalues
#' @importFrom shiny includeCSS
#' @importFrom shiny renderImage
#' @importFrom shiny flowLayout
#' @importFrom shiny br
#' @importFrom shiny showModal
#' @importFrom shiny singleton
#' @importFrom shiny span
#' @importFrom shiny sliderInput
#' @importFrom shiny plotOutput
#' @importFrom shiny reactivePrint
#' @importFrom shiny a
#' @importFrom shiny downloadButton
#' @importFrom shiny tagList
#' @importFrom shiny updateTextInput
#' @importFrom shiny strong
#' @importFrom shiny textOutput
#' @importFrom shiny tableOutput
#' @importFrom shiny helpText
#' @importFrom shiny browserViewer
#' @importFrom shiny mainPanel
#' @importFrom shiny tabPanel
#' @importFrom shiny reactiveFileReader
#' @importFrom shiny tag
#' @importFrom shiny observe
#' @importFrom shiny knit_print.shiny.appobj
#' @importFrom shiny updateTabsetPanel
#' @importFrom shiny p
#' @importFrom shiny Progress
#' @importFrom shiny bootstrapLib
#' @importFrom shiny validateCssUnit
#' @importFrom shiny HTML
#' @importFrom shiny h1
#' @importFrom shiny titlePanel
#' @importFrom shiny withLogErrors
#' @importFrom shiny showNotification
#' @importFrom shiny h2
#' @importFrom shiny dialogViewer
#' @importFrom shiny reactiveValuesToList
#' @importFrom shiny h3
#' @importFrom shiny h4
#' @importFrom shiny reactiveTimer
#' @importFrom shiny h5
#' @importFrom shiny h6
#' @importFrom shiny removeInputHandler
#' @importFrom shiny conditionalPanel
#' @importFrom shiny is.shiny.appobj
#' @importFrom shiny tagAppendChildren
#' @importFrom shiny exportTestValues
#' @importFrom shiny updateSelectInput
#' @importFrom shiny runExample
#' @importFrom shiny onRestored
#' @importFrom shiny updateCheckboxGroupInput
#' @importFrom shiny em
#' @importFrom shiny htmlOutput
#' @importFrom shiny need
#' @importFrom shiny urlModal
#' @importFrom shiny div
#' @importFrom shiny withProgress
#' @importFrom shiny NS
#' @importFrom shiny numericInput
#' @importFrom shiny fluidRow
#' @importFrom shiny renderUI
#' @importFrom shiny updateNavbarPage
#' @importFrom shiny maskReactiveContext
#' @importFrom shiny printError
#' @importFrom shiny reactiveUI
#' @importFrom shiny isolate
#' @importFrom shiny makeReactiveBinding
#' @importFrom shiny pre
#' @importFrom shiny inputPanel
#' @importFrom shiny onFlush
#' @importFrom shiny updateSliderInput
#' @importFrom shiny hoverOpts
#' @importFrom shiny conditionStackTrace<-
#' @importFrom shiny sidebarLayout
#' @importFrom shiny incProgress
#' @importFrom shiny captureStackTraces
#' @importFrom shiny restoreInput
#' @importFrom shiny htmlTemplate
#' @importFrom shiny fillPage
#' @importFrom shiny updateNumericInput
#' @importFrom shiny wellPanel
#' @importFrom shiny verbatimTextOutput
#' @importFrom shiny nearPoints
#' @importFrom shiny showBookmarkUrlModal
#' @importFrom shiny onFlushed
#' @importFrom shiny headerPanel
#' @importFrom shiny onSessionEnded
#' @importFrom shiny suppressDependencies
#' @importFrom shiny fixedPanel
#' @importFrom shiny radioButtons
#' @importFrom shiny tagSetChildren
#' @importFrom shiny eventReactive
#' @importFrom shiny includeText
#' @importFrom shiny ns.sep
#' @importFrom shiny renderTable
#' @importFrom shiny actionLink
#' @importFrom shiny brushOpts
#' @importFrom shiny updateActionButton
#' @importFrom shiny shinyApp
#' @importFrom shiny navlistPanel
#' @importFrom shiny callModule
#' @importFrom shiny dateRangeInput
#' @importFrom shiny uiOutput
#' @importFrom shiny validate
#' @importFrom shiny shinyOptions
#' @importFrom shiny hr
#' @importFrom shiny modalDialog
#' @importFrom shiny absolutePanel
#' @importFrom shiny updateRadioButtons
#' @importFrom shiny updateNavlistPanel
#' @importFrom shiny onBookmarked
#' @importFrom shiny renderText
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny downloadHandler
#' @importFrom shiny shinyAppDir
#' @importFrom shiny onRestore
#' @importFrom shiny clickOpts
#' @importFrom shiny ..stacktraceoff..
#' @importFrom shiny ..stacktraceon..
#' @importFrom shiny HTML
#' @importFrom shiny NS
#' @importFrom shiny Progress
#' @importFrom shiny a
#' @importFrom shiny absolutePanel
#' @importFrom shiny actionButton
#' @importFrom shiny actionLink
#' @importFrom shiny addResourcePath
#' @importFrom shiny animationOptions
#' @importFrom shiny as.shiny.appobj
#' @importFrom shiny basicPage
#' @importFrom shiny bookmarkButton
#' @importFrom shiny bootstrapLib
#' @importFrom shiny bootstrapPage
#' @importFrom shiny br
#' @importFrom shiny browserViewer
#' @importFrom shiny brushOpts
#' @importFrom shiny brushedPoints
#' @importFrom shiny callModule
#' @importFrom shiny captureStackTraces
#' @importFrom shiny checkboxGroupInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny clickOpts
#' @importFrom shiny code
#' @importFrom shiny column
#' @importFrom shiny conditionStackTrace
#' @importFrom shiny conditionStackTrace<-
#' @importFrom shiny conditionalPanel
#' @importFrom shiny createWebDependency
#' @importFrom shiny dateInput
#' @importFrom shiny dateRangeInput
#' @importFrom shiny dblclickOpts
#' @importFrom shiny dialogViewer
#' @importFrom shiny div
#' @importFrom shiny downloadButton
#' @importFrom shiny downloadHandler
#' @importFrom shiny downloadLink
#' @importFrom shiny em
#' @importFrom shiny enableBookmarking
#' @importFrom shiny eventReactive
#' @importFrom shiny exportTestValues
#' @importFrom shiny exprToFunction
#' @importFrom shiny extractStackTrace
#' @importFrom shiny fileInput
#' @importFrom shiny fillCol
#' @importFrom shiny fillPage
#' @importFrom shiny fillRow
#' @importFrom shiny fixedPage
#' @importFrom shiny fixedPanel
#' @importFrom shiny fixedRow
#' @importFrom shiny flowLayout
#' @importFrom shiny fluidPage
#' @importFrom shiny fluidRow
#' @importFrom shiny formatStackTrace
#' @importFrom shiny freezeReactiveValue
#' @importFrom shiny getDefaultReactiveDomain
#' @importFrom shiny getShinyOption
#' @importFrom shiny h1
#' @importFrom shiny h2
#' @importFrom shiny h3
#' @importFrom shiny h4
#' @importFrom shiny h5
#' @importFrom shiny h6
#' @importFrom shiny headerPanel
#' @importFrom shiny helpText
#' @importFrom shiny hoverOpts
#' @importFrom shiny hr
#' @importFrom shiny htmlOutput
#' @importFrom shiny htmlTemplate
#' @importFrom shiny icon
#' @importFrom shiny imageOutput
#' @importFrom shiny img
#' @importFrom shiny incProgress
#' @importFrom shiny includeCSS
#' @importFrom shiny includeHTML
#' @importFrom shiny includeMarkdown
#' @importFrom shiny includeScript
#' @importFrom shiny includeText
#' @importFrom shiny inputPanel
#' @importFrom shiny insertUI
#' @importFrom shiny installExprFunction
#' @importFrom shiny invalidateLater
#' @importFrom shiny is.reactive
#' @importFrom shiny is.reactivevalues
#' @importFrom shiny is.shiny.appobj
#' @importFrom shiny is.singleton
#' @importFrom shiny isTruthy
#' @importFrom shiny isolate
#' @importFrom shiny knit_print.html
#' @importFrom shiny knit_print.reactive
#' @importFrom shiny knit_print.shiny.appobj
#' @importFrom shiny knit_print.shiny.render.function
#' @importFrom shiny knit_print.shiny.tag
#' @importFrom shiny knit_print.shiny.tag.list
#' @importFrom shiny mainPanel
#' @importFrom shiny makeReactiveBinding
#' @importFrom shiny markRenderFunction
#' @importFrom shiny maskReactiveContext
#' @importFrom shiny modalButton
#' @importFrom shiny modalDialog
#' @importFrom shiny navbarMenu
#' @importFrom shiny navbarPage
#' @importFrom shiny navlistPanel
#' @importFrom shiny nearPoints
#' @importFrom shiny need
#' @importFrom shiny ns.sep
#' @importFrom shiny numericInput
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny onBookmark
#' @importFrom shiny onBookmarked
#' @importFrom shiny onFlush
#' @importFrom shiny onFlushed
#' @importFrom shiny onReactiveDomainEnded
#' @importFrom shiny onRestore
#' @importFrom shiny onRestored
#' @importFrom shiny onSessionEnded
#' @importFrom shiny outputOptions
#' @importFrom shiny p
#' @importFrom shiny pageWithSidebar
#' @importFrom shiny paneViewer
#' @importFrom shiny parseQueryString
#' @importFrom shiny passwordInput
#' @importFrom shiny plotOutput
#' @importFrom shiny plotPNG
#' @importFrom shiny pre
#' @importFrom shiny printError
#' @importFrom shiny printStackTrace
#' @importFrom shiny radioButtons
#' @importFrom shiny reactive
#' @importFrom shiny reactiveFileReader
#' @importFrom shiny reactivePlot
#' @importFrom shiny reactivePoll
#' @importFrom shiny reactivePrint
#' @importFrom shiny reactiveTable
#' @importFrom shiny reactiveText
#' @importFrom shiny reactiveTimer
#' @importFrom shiny reactiveUI
#' @importFrom shiny reactiveValues
#' @importFrom shiny reactiveValuesToList
#' @importFrom shiny registerInputHandler
#' @importFrom shiny removeInputHandler
#' @importFrom shiny removeModal
#' @importFrom shiny removeNotification
#' @importFrom shiny removeUI
#' @importFrom shiny renderImage
#' @importFrom shiny renderPlot
#' @importFrom shiny renderPrint
#' @importFrom shiny renderTable
#' @importFrom shiny renderText
#' @importFrom shiny renderUI
#' @importFrom shiny repeatable
#' @importFrom shiny req
#' @importFrom shiny restoreInput
#' @importFrom shiny runApp
#' @importFrom shiny runExample
#' @importFrom shiny runGadget
#' @importFrom shiny runGist
#' @importFrom shiny runGitHub
#' @importFrom shiny runUrl
#' @importFrom shiny safeError
#' @importFrom shiny selectInput
#' @importFrom shiny selectizeInput
#' @importFrom shiny serverInfo
#' @importFrom shiny setBookmarkExclude
#' @importFrom shiny setProgress
#' @importFrom shiny shinyApp
#' @importFrom shiny shinyAppDir
#' @importFrom shiny shinyAppFile
#' @importFrom shiny shinyOptions
#' @importFrom shiny shinyServer
#' @importFrom shiny shinyUI
#' @importFrom shiny showBookmarkUrlModal
#' @importFrom shiny showModal
#' @importFrom shiny showNotification
#' @importFrom shiny showReactLog
#' @importFrom shiny sidebarLayout
#' @importFrom shiny sidebarPanel
#' @importFrom shiny singleton
#' @importFrom shiny sliderInput
#' @importFrom shiny span
#' @importFrom shiny splitLayout
#' @importFrom shiny stopApp
#' @importFrom shiny strong
#' @importFrom shiny submitButton
#' @importFrom shiny suppressDependencies
#' @importFrom shiny tabPanel
#' @importFrom shiny tableOutput
#' @importFrom shiny tabsetPanel
#' @importFrom shiny tag
#' @importFrom shiny tagAppendAttributes
#' @importFrom shiny tagAppendChild
#' @importFrom shiny tagAppendChildren
#' @importFrom shiny tagList
#' @importFrom shiny tagSetChildren
#' @importFrom shiny tags
#' @importFrom shiny textAreaInput
#' @importFrom shiny textInput
#' @importFrom shiny textOutput
#' @importFrom shiny titlePanel
#' @importFrom shiny uiOutput
#' @importFrom shiny updateActionButton
#' @importFrom shiny updateCheckboxGroupInput
#' @importFrom shiny updateCheckboxInput
#' @importFrom shiny updateDateInput
#' @importFrom shiny updateDateRangeInput
#' @importFrom shiny updateNavbarPage
#' @importFrom shiny updateNavlistPanel
#' @importFrom shiny updateNumericInput
#' @importFrom shiny updateQueryString
#' @importFrom shiny updateRadioButtons
#' @importFrom shiny updateSelectInput
#' @importFrom shiny updateSelectizeInput
#' @importFrom shiny updateSliderInput
#' @importFrom shiny updateTabsetPanel
#' @importFrom shiny updateTextAreaInput
#' @importFrom shiny updateTextInput
#' @importFrom shiny urlModal
#' @importFrom shiny validate
#' @importFrom shiny validateCssUnit
#' @importFrom shiny verbatimTextOutput
#' @importFrom shiny verticalLayout
#' @importFrom shiny wellPanel
#' @importFrom shiny withLogErrors
#' @importFrom shiny withMathJax
#' @importFrom shiny withProgress
#' @importFrom shiny withReactiveDomain
#' @importFrom shiny withTags
#' @import rhandsontable
#' @importFrom DT renderDataTable
#' @importFrom DT dataTableOutput
#' @importFrom sets set_power
#' @importFrom rmarkdown pandoc_available
#' @importFrom rmarkdown render
#' @importFrom graphics axis
#' @importFrom graphics box
#' @importFrom graphics boxplot
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics mtext
#' @importFrom graphics par
#' @importFrom graphics plot.new
#' @importFrom graphics plot.window
#' @importFrom graphics rect
#' @importFrom graphics segments
#' @importFrom graphics strwidth
#' @importFrom graphics text
#' @importFrom stats complete.cases
#' @importFrom stats as.formula
#' @importFrom stats biplot
#' @importFrom stats coef
#' @importFrom stats cor
#' @importFrom stats cov
#' @importFrom stats density
#' @importFrom stats dist
#' @importFrom stats fitted
#' @importFrom stats formula
#' @importFrom stats glm
#' @importFrom stats kmeans
#' @importFrom stats lm
#' @importFrom stats mad
#' @importFrom stats mahalanobis
#' @importFrom stats median
#' @importFrom stats model.matrix
#' @importFrom stats na.omit
#' @importFrom stats poisson
#' @importFrom stats predict
#' @importFrom stats princomp
#' @importFrom stats qchisq
#' @importFrom stats qnorm
#' @importFrom stats quantile
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @importFrom stats sd
#' @importFrom stats terms
#' @importFrom stats var
#' @importFrom stats weighted.mean
#' @importFrom stats xtabs
#' @importFrom utils combn
#' @importFrom utils data
#' @importFrom utils flush.console
#' @importFrom utils tail
#' @importFrom utils read.table
#' @importFrom utils write.table
#' @importFrom haven read_spss
#' @importFrom haven read_dta
#' @importFrom haven read_sas
#' @importFrom haven write_sav
#' @importFrom haven write_dta
#' @importFrom haven as_factor
#' @importFrom prettydoc html_pretty
setClassUnion("dataframeOrNULL", c("data.frame", "NULL"))
setClassUnion("numericOrNULL", c("numeric", "NULL"))
setClassUnion("characterOrNULL", c("character", "NULL"))
setClassUnion("logicalOrNULL", c("logical", "NULL"))
setClassUnion("matrixOrNULL", c("matrix", "NULL"))
setClassUnion("listOrNULL", c("list", "NULL"))
setClassUnion("factorOrNULL", c("factor", "NULL"))
setClassUnion("sdcmicroOrNULL", c("NULL"))

#' Class \code{"sdcMicroObj"}
#'
#' Class to save all information about the SDC process
#'
#' @name sdcMicroObj-class
#' @aliases sdcMicroObj-class
#' createSdcObj
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("sdcMicroObj", ...)}.
#' @author Bernhard Meindl, Alexander Kowarik, Matthias Templ, Elias Rut
#' @keywords classes
#' @export
#' @examples
#'
#' showClass("sdcMicroObj")
#' \dontrun{
#' data(testdata)
#' sdc <- createSdcObj(testdata,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' head(sdc@@manipNumVars)
#' ### Display Risks
#' sdc@@risk$global
#' sdc <- dRisk(sdc)
#' sdc@@risk$numeric
#' ### use addNoise without Parameters
#' sdc <- addNoise(sdc,variables=c("expend","income"))
#' head(sdc@@manipNumVars)
#' sdc@@risk$numeric
#' ### undolast
#' sdc <- undolast(sdc)
#' head(sdc@@manipNumVars)
#' sdc@@risk$numeric
#' ### redo addNoise with Parameter
#' sdc <- addNoise(sdc, noise=0.2)
#' head(sdc@@manipNumVars)
#' sdc@@risk$numeric
#' ### dataGen
#' #sdc <- undolast(sdc)
#' #head(sdc@@risk$individual)
#' #sdc@@risk$global
#' #sdc <- dataGen(sdc)
#' #head(sdc@@risk$individual)
#' #sdc@@risk$global
#' ### LocalSuppression
#' sdc <- undolast(sdc)
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' sdc <- localSuppression(sdc)
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' ### microaggregation
#' sdc <- undolast(sdc)
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' sdc <- microaggregation(sdc)
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' ### pram
#' sdc <- undolast(sdc)
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' sdc <- pram(sdc,keyVar="water")
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' ### rankSwap
#' sdc <- undolast(sdc)
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' sdc <- rankSwap(sdc)
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' head(sdc@@risk$individual)
#' sdc@@risk$global
#' ### suda2
#' sdc <- suda2(sdc)
#' sdc@@risk$suda2
#' ### topBotCoding
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' sdc@@risk$numeric
#' sdc <- topBotCoding(sdc, value=60000000, replacement=62000000, column="income")
#' head(get.sdcMicroObj(sdc, type="manipNumVars"))
#' sdc@@risk$numeric
#' ### LocalRecProg
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c("urbrur", "roof", "walls", "water", "sex", "relat"))
#' sdc@@risk$global
#' sdc <- LocalRecProg(sdc)
#' sdc@@risk$global
#' ### LLmodGlobalRisk
#' sdc <- undolast(sdc)
#' sdc <- LLmodGlobalRisk(sdc, inclProb=0.001)
#' sdc@@risk$model
#' }
#'
setClass(Class = "sdcMicroObj",
  representation = representation(
    origData = "dataframeOrNULL",
    keyVars = "numericOrNULL",
    pramVars = "numericOrNULL",
    numVars = "numericOrNULL",
    ghostVars = "listOrNULL",
    weightVar = "numericOrNULL",
    hhId = "numericOrNULL",
    strataVar = "numericOrNULL",
    sensibleVar = "numericOrNULL",
    manipKeyVars = "dataframeOrNULL",
    manipPramVars = "dataframeOrNULL",
    manipNumVars = "dataframeOrNULL",
    manipGhostVars = "dataframeOrNULL",
    manipStrataVar = "factorOrNULL",
    originalRisk = "listOrNULL",
    risk = "listOrNULL",
    utility = "listOrNULL",
    pram = "listOrNULL",
    localSuppression = "listOrNULL",
    options = "listOrNULL",
    additionalResults = "listOrNULL",
    set = "listOrNULL",
    prev = "sdcmicroOrNULL",
    deletedVars = "characterOrNULL"),
  prototype = prototype(
    origData = NULL,
    keyVars = NULL,
    pramVars = NULL,
    numVars = NULL,
    ghostVars = NULL,
    weightVar = NULL,
    hhId = NULL,
    strataVar = NULL,
    sensibleVar = NULL,
    manipKeyVars = NULL,
    manipPramVars = NULL,
    manipNumVars = NULL,
    manipGhostVars = NULL,
    manipStrataVar = NULL,
    originalRisk = NULL,
    risk = NULL,
    utility = NULL,
    pram = NULL,
    localSuppression = NULL,
    options = NULL,
    additionalResults = NULL,
    set = NULL,
    prev = NULL,
    deletedVars = NULL),
  validity = function(object) {
    if (!is.null(object@manipKeyVars) && ncol(object@manipKeyVars) != length(object@keyVars)) {
      stop("wrong dimension of slot 'manipKeyVars'!\n")
    }
    if (!is.null(object@manipNumVars) && ncol(object@manipNumVars) != length(object@numVars)) {
      stop("wrong dimension of slot 'manipNumVars'!\n")
    }
    if (!is.null(object@strataVar) && object@strataVar %in% object@keyVars) {
      stop("stratification variable cant be a categorical key variable!\n")
    }
    return(TRUE)
  })

setIs("sdcMicroObj", "sdcmicroOrNULL")
