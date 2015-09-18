###################################################
# LLmodRisk Function
# Calculates two disclosure risk measures with
# five possible log-linear models (standard, CE, PSE, weightedLLM, IPF)
# 1. estimates the number of sample uniques that are population unique
# 2. estimates the number of correct matches of sample uniques
###################################################

#' Global risk using log-linear models.
#'
#' The sample frequencies are assumed to be independent and following a Poisson
#' distribution. The parameters of the corresponding parameters are estimated
#' by a log-linear model including the main effects and possible interactions.
#'
#' This measure aims to (1) calculate the number of sample uniques that are
#' population uniques with a probabilistic Poisson model and (2) to estimate
#' the expected number of correct matches for sample uniques.
#'
#' ad 1) this risk measure is defined over all sample uniques as \deqn{ \tau_1
#' = \sum\limits_{j:f_j=1} P(F_j=1 | f_j=1) \quad , } i.e. the expected number
#' of sample uniques that are population uniques.
#'
#' ad 2) this risk measure is defined over all sample uniques as \deqn{ \tau_2
#' = \sum\limits_{j:f_j=1} P(1 / F_j | f_j=1) \quad . }
#'
#' Since population frequencies \eqn{F_k} are unknown, they need to be
#' estimated.
#'
#' The iterative proportional fitting method is used to fit the parameters of
#' the Poisson distributed frequency counts related to the model specified to
#' fit the frequency counts. The obtained parameters are used to estimate a
#' global risk, defined in Skinner and Holmes (1998).
#'
#' @name modRisk
#' @aliases modRisk modRisk-methods
#' modRisk,data.frame-method modRisk,matrix-method
#' modRisk,sdcMicroObj-method modRisk
#' @docType methods
#' @param obj An \code{\link{sdcMicroObj-class}}-object or a numeric matrix
#' or data.frame containing all variables required in the specified model.
#' @param method chose method for model-based risk-estimation. Currently, the
#' following methods can be selected:
#' \itemize{
#' \item "default": the standard log-linear model.
#' \item "CE": the Clogg Eliason method, additionally,  considers survey weights by using an offset term.
#' \item "PML": the pseudo maximum likelihood method.
#' \item "weightedLLM": the weighted maximum likelihood method, considers survey weights by including them as one of the predictors.
#' \item "IPF": iterative proportional fitting as used in deprecated method 'LLmodGlobalRisk'.
#' }
#' @param weights a variable name specifying sampling weights
#' @param formulaM A formula specifying the model.
#' @param bound a number specifying a threshold for 'risky' observations in the sample.
#' @param ... additional parameters passed through, currently ignored.
#' @return Two global risk measures and some model output given the specified model. If this method
#' is applied to an \code{\link{sdcMicroObj-class}}-object, the slot 'risk' in the object ist updated
#' with the result of the model-based risk-calculation.
#' @author Matthias Templ, Marius Totter, Bernhard Meindl
#' @seealso \code{\link{loglm}}, \code{\link{measure_risk}}
#' @references Skinner, C.J. and Holmes, D.J. (1998) \emph{Estimating the
#' re-identification risk per record in microdata}. Journal of Official
#' Statistics, 14:361-372, 1998.
#'
#' Rinott, Y. and Shlomo, N. (1998). \emph{A Generalized Negative Binomial
#' Smoothing Model for Sample Disclosure Risk Estimation}. Privacy in
#' Statistical Databases. Lecture Notes in Computer Science.  Springer-Verlag,
#' 82--93.
#' 
#' Clogg, C.C. and Eliasson, S.R. (1987). \emph{Some Common Problems in Log-Linear Analysis}. Sociological Methods and Research, 8-44.
#' 
#' @keywords manip
#' @export
#' @examples
#'
#' ## data.frame method
#' data(testdata2)
#' form <- ~sex+water+roof
#' w <- "sampling_weight"
#' (modRisk(testdata2, method="default", formulaM=form, weights=w))
#' (modRisk(testdata2, method="CE", formulaM=form, weights=w))
#' (modRisk(testdata2, method="PML", formulaM=form, weights=w))
#' (modRisk(testdata2, method="weightedLLM", formulaM=form, weights=w))
#' (modRisk(testdata2, method="IPF", formulaM=form, weights=w))
#'
#' ## application to a sdcMicroObj
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'  keyVars=c('urbrur','roof','walls','electcon','relat','sex'),
#'  numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- modRisk(sdc,form=~sex+water+roof)
#' slot(sdc, "risk")$model
#'
setGeneric("modRisk", function(obj, method = "default", weights, formulaM, bound=Inf, ...) {
  standardGeneric("modRisk")
})

setMethod(f = "modRisk", signature = c("sdcMicroObj"),
          definition = function(obj, method = "default", weights, formulaM, bound=Inf) {
            vars <- labels(terms(formulaM))
            mk <- get.sdcMicroObj(obj, type = "manipKeyVars")
            mn <- get.sdcMicroObj(obj, type = "manipNumVars")
            orig <- get.sdcMicroObj(obj, type = "origData")
            cn <- colnames(orig)
            ok <- orig[, !colnames(orig) %in% c(colnames(mk), colnames(mn)), drop = FALSE]
            if ( any(colnames(mk) %in% vars) ) {
              x <- mk[, colnames(mk) %in% vars, drop = FALSE]
            } else {
              x <- NULL
            }
            if ( any(colnames(mn) %in% vars) ) {
              if (is.null(x)) {
                x <- mn[, colnames(mn) %in% vars, drop = FALSE]
              } else {
                x <- data.frame(x, mn[, colnames(mn) %in% vars, drop = FALSE])
              }
            }
            if (any(colnames(ok) %in% vars)) {
              if (is.null(x)) {
                x <- ok[, colnames(ok) %in% vars, drop = FALSE]
              } else {
                x <- data.frame(x, ok[, colnames(ok) %in% vars, drop = FALSE])
              }
            }
            
            wV <- get.sdcMicroObj(obj, type="weightVar")
            weightsVar <- cn[wV]
            if ( is.null(wV) ) {
              stop("It is not possible to calculate model-based risks for data without sampling weights (slot 'weightVar')!\n")
            }
            x[[weightsVar]] <- orig[[wV]]
            
            risk <- get.sdcMicroObj(obj, type="risk")
            risk$model <- modRisk(x, method=method, weights=weightsVar, formulaM=formulaM, bound=bound)
            obj <- set.sdcMicroObj(obj, type="risk", input=list(risk))
            obj
          })

setMethod(f = "modRisk", signature = c("matrix"),
          definition = function(obj, method = "default", weights, formulaM, bound=Inf) {
            modRisk(obj=as.data.frame(obj), method = method, weights = weights, formulaM = formulaM, bound=bound)
          })

# todo: modRisk-data.frame() should be the workhorse
setMethod(f = "modRisk", signature = c("data.frame"),
          definition = function(obj, method = "default", weights, formulaM, bound=Inf) {
            risk1 <- function(l, p) {
              v = (1 - p) * l
              exp(-v)
            }
            risk2 <- function(l, p) {
              v = (1 - p) * l
              (1 - exp(-v))/v
            }
            file_risk <- function(freq, risk) {
              sum(as.numeric(freq == 1) * risk)
            }
            
            . <- inclProb <- counts <- id <- Fk <- NULL
            x <- obj
            if ( !is.data.frame(x) ) {
              stop("input 'x' must be a data.frame!\n")
            }
            if ( !method %in% c("default","CE","PML","weightedLLM","IPF") ) {
              warning("Unknown method was selected. Falling back to default estimation method!\n")
              method <- "default"
            }
            if ( !weights %in% colnames(x) ) {
              stop("Please provide a valid variable name that contains sampling weights!\n")
            }
            if ( length(bound) != 1 & bound[1] <= 0) {
              stop("Argument 'bound' must be numeric > 0!\n")
            }
            
            form_info <- terms(formulaM)
            orders <- attributes(form_info)$order
            vars <- labels(form_info)[orders==1]
            
            if ( method=="IPF" && any(orders>1) ) {
              stop("Sorry, but method 'IPF' cannot be used for models with interactions!\n")
            }
            
            if ( !all(vars %in% colnames(x)) ) {
              stop("all variables specified in the formula must exist in the input dataset!\n")
            }
            
            x <- x[,c(vars, weights)]
            colnames(x) <- c(vars, "weights")
            y <- data.table(x, key=vars)
            y[,inclProb:=1/weights]
            y <- y[,.(counts=.N, weights=sum(weights), inclProb=sum(inclProb)), by=key(y)]
            
            grid <- data.table(expand.grid(lapply(1:length(vars), function(t) unique((x[[vars[t]]])))))
            setnames(grid, vars)
            setkeyv(grid, vars)
            
            x <- merge(grid, y, all.x=TRUE)
            x[is.na(counts), counts:=0]
            x[is.na(weights ), weights :=0]
            x[is.na(inclProb), inclProb:=0]
            
            # model selection
            if ( method == "default" ) {
              form <- as.formula(paste(c("counts", as.character(formulaM)), collapse = ""))
            }
            if ( method == "CE" ) {
              EC <- x$counts/x$weights  #offset term
              EC[EC == "NaN"] <- 0
              EC <- log(EC + 0.1)
              form <- as.formula(paste(c("counts", c(as.character(formulaM)), "+ offset(EC)"),
                                       collapse = ""))
            }
            if ( method == "PML" ) {
              f <- sum(x$counts)/sum(x$weights)
              weights_t <- round(x$weights * f)  #round
              x <- data.frame(x, weights_t)
              form <- as.formula(paste(c("weights_t", as.character(formulaM)), collapse = ""))
            }
            if ( method == "weightedLLM" ) {
              form_zw <- as.formula(paste(c(formulaM, "weights"), collapse = "+"))
              form <- as.formula(paste(c("counts", as.character(form_zw)), collapse = ""))
            }
            if ( method == "IPF" ) {
              form <- as.formula(paste(c("counts", as.character(formulaM)), collapse = ""))
              form2 <- as.formula(paste(c("inclProb", as.character(formulaM)), collapse = ""))
              tab <- xtabs(form, x)
              tabP <- xtabs(form2, x)
            }
            
            # running the model with the chosen formula
            if ( method == "IPF" ) {
              mod <- loglm(form, data = tab, fitted = TRUE)
            } else {
              mod <- glm(form, data = x, family = poisson())
            }
            lambda <- fitted(mod)
            
            # calculate risk and estimate
            # 1. the number of sample uniques that are population unique
            # 2. the number of correct matches of sample uniques
            if ( method != "IPF" ) {
              x <- data.table(x)
              x[,id:=1:nrow(x)]
              setkey(x, id)
              lambda <- data.table(Fk=as.numeric(lambda), id=as.numeric(attributes(lambda)$names))
              setkey(lambda, id)
            } else {
              x <- data.table(x, key=vars)
              lambda <- as.data.frame.table(lambda, stringsAsFactors=FALSE)
              colnames(lambda)[ncol(lambda)] <- "Fk"
              lambda <- data.table(lambda, key=vars)
              lambda <- lambda[,lapply(.SD, as.numeric)]
            }
            x <- merge(x, lambda, all.x=TRUE)
            x[is.na(Fk), Fk:=0]
            x <- x[0 < x$counts & x$counts <= bound]
            x <- x[!(x$Fk==0 & x$counts >0)]
            r1 <- risk1(x$Fk, x$inclProb) / nrow(x)
            r2 <- risk2(x$Fk, x$inclProb) / nrow(x)
            gr1 <- file_risk(x$counts, r1)
            gr2 <- file_risk(x$counts, r2)
            
            res <- list(gr1=gr1, gr2=gr2, gr1perc=gr1*100, gr2perc=gr2*100,
                        method=method, model=formulaM, fitted=fitted(mod), inclProb=x$inclProb)
            class(res) <- "modrisk"
            res
          })


#' Print method for objects from class modrisk
#'
#' Print method for objects from class modrisk
#' @param x an object of class \code{\link{modrisk}}
#' @param \dots Additional arguments passed through.
#' @return Output of model-based risk estimation
#' @author Bernhard Meindl
#' @seealso \code{\link{modRisk}}
#' @aliases modrisk
#' @keywords print
#' @method print modrisk
#' @export
print.modrisk <- function(x, ...) {
  cat(paste0("The estimated model (using method '",x$method,"') was:\n"))
  cat(paste0("\t",paste(as.character(x$model), collapse=" "),"\n"))
  cat("global risk-measures:\n")
  cat(paste0("\tRisk-Measure 1: ", prettyF(x$gr1, digits=3)," (",prettyF(x$gr1perc, digits=3)," %)\n"))
  cat(paste0("\tRisk-Measure 2: ", prettyF(x$gr2, digits=3)," (",prettyF(x$gr2perc, digits=3)," %)\n"))
}