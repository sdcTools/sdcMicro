#' Shuffling and EGADP
#'
#' Data shuffling and General Additive Data Perturbation.
#'
#' Perturbed values for the sensitive variables are generated.  The sensitive
#' variables have to be stored as responses in the argument \sQuote{form},
#' which is the usual formula interface for regression models in R.
#'
#' For method \dQuote{ds} the EGADP method is applied on the norm inverse
#' percentiles.  Shuffling then ranks the original values according to the GADP
#' output. For further details, please see the references.
#'
#' Method \dQuote{mvn} uses a simplification and draws from the normal Copulas
#' directly before these draws are shuffled.
#'
#' Method \dQuote{mlm} is also a simplification. A linear model is applied the
#' expected values are used as the perturbed values before shuffling is
#' applied.
#'
#' @name shuffle
#' @aliases shuffle shuffle-methods shuffle,data.frame-method
#' shuffle,matrix-method shuffle,sdcMicroObj-method
#' @docType methods
#' @param obj An object of class sdcMicroObj or a data.frame including the
#' data.
#' @param form An object of class \dQuote{formula} (or one that can be coerced
#' to that class): a symbolic description of the model to be fitted.  The
#' responses have to consists of at least two variables of any class and the
#' response variables have to be of class numeric.  The response variables
#' belongs to numeric key variables (quasi-identifiers of numeric scale). The
#' predictors are can be distributed in any way (numeric, factor, ordered
#' factor).
#' @param method currently either the original form of data shuffling
#' (\dQuote{ds} - default), \dQuote{mvn} or \dQuote{mlm}, see the details
#' section. The last method is in experimental mode and almost untested.
#' @param weights Survey sampling weights. Automatically chosen when obj is of
#' class \code{\link{sdcMicroObj-class}}.
#' @param covmethod Method for covariance estimation. \dQuote{spearman},
#' \dQuote{pearson} and \ dQuotemcd are possible. For the latter one, the
#' implementation in package robustbase is used.
#' @param regmethod Method for multivariate regression. \dQuote{lm} and
#' \dQuote{MM} are possible.  For method \dQuote{MM}, the function \dQuote{rlm}
#' from package MASS is applied.
#' @param gadp TRUE, if the egadp results from a fit on the origianl data is
#' returned.
#' @return If \sQuote{obj} is of class \code{\link{sdcMicroObj-class}} the corresponding
#' slots are filled, like manipNumVars, risk and utility.  If \sQuote{obj} is
#' of class \dQuote{data.frame} an object of class \dQuote{micro} with
#' following entities is returned: \item{shConf }{the shuffled numeric key
#' variables} \item{egadp }{the perturbed (using gadp method) numeric key
#' variables}
#' @note In this version, the covariance method chosen is used for any
#' covariance and correlation estimations in the whole gadp and shuffling
#' function.
#' @section Methods: \describe{
#' \item{list("signature(obj = \"data.frame\")")}{}
#' \item{list("signature(obj = \"matrix\")")}{}
#' \item{list("signature(obj = \"sdcMicroObj\")")}{}}
#' @author Matthias Templ, Alexander Kowarik
#' @seealso \code{\link{rankSwap}, \link{lm}}
#' @references K. Muralidhar, R. Parsa, R. Saranthy (1999). A general additive
#' data perturbation method for database security. \emph{Management Science},
#' 45, 1399-1415.
#'
#' K. Muralidhar, R. Sarathy (2006). Data shuffling - a new masking approach
#' for numerical data. \emph{Management Science}, 52(5), 658-670, 2006.
#'
#' M. Templ, B. Meindl. (2008).  Robustification of Microdata Masking Methods
#' and the Comparison with Existing Methods, in: \emph{Lecture Notes on
#' Computer Science}, J. Domingo-Ferrer, Y. Saygin (editors.); Springer,
#' Berlin/Heidelberg, 2008, ISBN: 978-3-540-87470-6, pp. 14-25.
#' @keywords manip
#' @export
#' @examples
#'
#' data(Prestige,package="car")
#' form <- formula(income + education ~ women + prestige + type, data=Prestige)
#' sh <- shuffle(obj=Prestige,form)
#' plot(Prestige[,c("income", "education")])
#' plot(sh$sh)
#' colMeans(Prestige[,c("income", "education")])
#' colMeans(sh$sh)
#' cor(Prestige[,c("income", "education")], method="spearman")
#' cor(sh$sh, method="spearman")
#'
#' ## for objects of class sdcMicro:
#' data(testdata2)
#' sdc <- createSdcObj(testdata2,
#'   keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
#'   numVars=c('expend','income','savings'), w='sampling_weight')
#' sdc <- shuffle(sdc, method=c('ds'),regmethod= c('lm'), covmethod=c('spearman'),
#' 		form=savings+expend ~ urbrur+walls)
#'
setGeneric("shuffle", function(obj, form, method = "ds", weights = NULL,
  covmethod = "spearman", regmethod = "lm", gadp = TRUE) {
  standardGeneric("shuffle")
})

setMethod(f = "shuffle", signature = c("sdcMicroObj"),
  definition = function(obj, form, method = "ds", weights = NULL,
    covmethod = "spearman", regmethod = "lm", gadp = TRUE) {

  xn <- get.sdcMicroObj(obj, type = "manipNumVars")
  xp <- get.sdcMicroObj(obj, type = "manipPramVars")
  xk <- get.sdcMicroObj(obj, type = "manipKeyVars")
  xs <- get.sdcMicroObj(obj, type = "manipStrataVar")
  vars <- gsub(" ", "", unlist(strsplit(as.character(form)[[2]], "\\+")))
  pred <- gsub(" ", "", unlist(strsplit(as.character(form)[[3]], "\\+")))

  x <- cbind(xn, xk)
  if (!is.null(xs)) {
    x <- cbind(x, xs)
  }
  if (!is.null(xp)) {
    x <- cbind(x, xp)
  }
  x <- x[, colnames(x) %in% c(pred, vars)]
  if (any(!(pred %in% colnames(x)))) {
    xo <- get.sdcMicroObj(obj, type = "origData")
    xo <- xo[, colnames(xo) %in% pred[!(pred %in% colnames(x))], drop = FALSE]
    x <- cbind(x, xo)
  }
  if (any(!vars %in% colnames(x))) {
    stop(paste("Variables:", paste(vars[!vars %in% colnames(x)], collapse = ","), "not found"))
  }

  res <- shuffleWORK(data = x, form = form, method = method, weights = weights, covmethod = covmethod,
    regmethod = regmethod, gadp = gadp)
  if (any(!vars %in% colnames(xn)))
    stop("All response variable have to be numeric!")
  if (any(vars %in% colnames(xn))) {
    xn[, vars[vars %in% colnames(xn)]] <- res$shuffled[, vars[vars %in% colnames(xn)]]
  }
  obj <- nextSdcObj(obj)

  obj <- set.sdcMicroObj(obj, type = "manipNumVars", input = list(as.data.frame(xn)))
  obj <- dRisk(obj)
  obj <- dUtility(obj)
  obj
})

setMethod(f = "shuffle", signature = c("data.frame"),
definition = function(obj, form, method = "ds", weights = NULL,
  covmethod = "spearman", regmethod = "lm", gadp = TRUE) {

  shuffleWORK(data = obj, form = form, method = method, weights = weights,
    covmethod = covmethod, regmethod = regmethod, gadp = gadp)
})

setMethod(f = "shuffle", signature = c("matrix"),
definition = function(obj, form, method = "ds", weights = NULL,
  covmethod = "spearman", regmethod = "lm", gadp = TRUE) {

  shuffleWORK(data = obj, form = form, method = method, weights = weights,
    covmethod = covmethod, regmethod = regmethod, gadp = gadp)
})

shuffleWORK <- function(data, form, method = "ds", weights = NULL, covmethod = "spearman",
  regmethod = "lm", gadp = TRUE) {
  ## S ... nonconfidential variables (numerical and/or categorical) (predictors) X ...
  ## confidential variables (numerical) (responses) multivariate regression of confidential
  ## variables against non-confidential currentlty no support for missings covmethod:
  cv <- function(x, type = "spearman") {
    switch(type, spearman = cov(x, method = "spearman"), pearson = cov(x, method = "pearson"),
      mcd = covMcd(x, cor = TRUE)$cov)
  }
  cr <- function(x, type = "spearman") {
    switch(type, spearman = cor(x, method = "spearman"), pearson = cor(x, method = "pearson"),
      mcd = covMcd(x, cor = TRUE)$cor)
  }

  reverseMap <- function(x, y, ties = "average") {
    x[order(rank(x, ties.method = ties))] <- x[order(rank(y, ties.method = ties))]
    y[order(rank(y, ties.method = ties))] <- x[order(rank(x, ties.method = ties))]
    y
  }
  missingid <- list()
  if (any(is.na(data))) {
    warning("rows with missing values have been imputed!")
    missingid <- list()
    for (i in 1:ncol(data)) {
      indna <- which(is.na(data[, i]))
      indnotna <- c(1:nrow(data))[-indna]
      missingid[[i]] <- indna
      ## Imputation with any valid value from the data set
      if (length(indna) > 0)
        data[indna, i] <- data[sample(indnotna, length(indna), replace = TRUE), i]
    }
  }
  # data <- na.omit(data)
  predictors <- model.matrix(form, data = as.data.frame(data))
  predictors <- predictors[, 2:ncol(predictors), drop = FALSE]
  formR <- formula(paste(" ~ ", strsplit(as.character(form), "~")[[2]]))
  responses <- model.matrix(formR, data = as.data.frame(data))
  responses <- responses[, 2:ncol(responses)]
  if (dim(data.frame(responses))[2] < 2)
    stop("The method needs at least 2 confidential numeric variables")
  egadp <- function(responses1, predictors1, covmethod = "spearman", regmethod = "lm") {
    if (regmethod == "lm") {
      responses1[responses1 == Inf] <- 1
      predictors1[predictors1 == Inf] <- 1
      result <- lm(responses1 ~ predictors1, weights = weights)
      reg <- result$resid
      fitted <- result$fitted
    } else if (regmethod == "MM") {
      reg <- fitted <- matrix(, ncol = ncol(responses1), nrow = nrow(responses1))
      for (i in 1:ncol(responses1)) {
        result <- rlm(responses1[, i] ~ predictors1, method = "MM", weights = weights)
        reg[, i] <- result$resid
        fitted[, i] <- result$fitted
      }
    } else {
      stop("regmethod must be lm or MM")
    }
    ## covariance of the residuals
    cvs <- cv(reg, covmethod)
    ## generate independent random variates V
    V <- mvrnorm(n = nrow(data), mu = colMeans(responses1), Sigma = cv(responses1, covmethod))
    ## regress V on S and X
    Vres <- lm(V ~ cbind(predictors1, responses1))
    ## compute covariance of residuals again
    cvV <- cv(Vres$resid, covmethod)
    ## normalize cvV based on cv:
    E <- eigen(cvs)
    V <- E$values
    Q <- E$vectors
    Y <- Q %*% diag(sqrt(V)) %*% t(Q)
    Yr <- solve(Y %*% Y)
    E <- eigen(cvV)
    V <- E$values
    Q <- E$vectors
    Y <- Q %*% diag(sqrt(V)) %*% t(Q)
    Yb <- solve(Y %*% Y)
    e <- Vres$resid %*% Yb %*% Yr
    ## calculate new perturbed matrix Y:
    res <- fitted + e
    # fits <-fitted
    return(res)
  }
  if (method == "ds_cov") {
    mat <- cbind(responses, predictors)
    sig <- cr(mat, covmethod)
    ranks <- apply(mat, 2, rank, ties.method = "average")
    perc <- apply(ranks, 2, function(x) (x - 0.5)/length(x))
    indResp <- 1:ncol(responses)
    indPred <- (ncol(responses) + 1):(ncol(responses) + ncol(predictors))
    normInvers <- apply(perc, c(2), function(x) qnorm(x))
    responses1 <- normInvers[, indResp, drop = FALSE]
    predictors1 <- normInvers[, indPred, drop = FALSE]
    Ystar <- predictors %*% t(cov(responses1, predictors1) %*% solve(cov(predictors1)))
    Sigma <- cov(responses1) - cov(responses1, predictors1) %*% solve(cov(predictors1)) %*%
      cov(predictors1, responses1)
    e <- mvrnorm(nrow(responses), Sigma = Sigma, mu = rep(0, ncol(responses)))
    Ystar <- Ystar + e
  }
  if (method == "ds") {
    mat <- cbind(responses, predictors)
    # sig <- cr(mat, covmethod)
    ranks <- apply(mat, 2, rank, ties.method = "average")
    perc <- apply(ranks, 2, function(x) (x - 0.5)/length(x))
    normInvers <- apply(perc, c(2), function(x) qnorm(x))
    indResp <- 1:ncol(responses)
    indPred <- (ncol(responses) + 1):(ncol(responses) + ncol(predictors))
    pmc <- 2 * sin((pi * cr(mat, type = covmethod))/6)  #(where pi = 22/7).
    pxs <- pmc[indResp, indPred]
    pxx <- pmc[indResp, indResp]
    psx <- pmc[indPred, indResp]
    pssinv <- solve(pmc[indPred, indPred])
    normInvers <- apply(perc, c(2), function(x) qnorm(x))
    responses1 <- normInvers[, indResp]
    predictors1 <- normInvers[, indPred]
    Ystar1 <- predictors1 %*% t(pxs %*% pssinv)
    Sigma <- pxx - pxs %*% pssinv %*% psx
    e1 <- mvrnorm(nrow(responses), Sigma = Sigma, mu = rep(0, ncol(responses)))
    Ystar <- Ystar1 + e1
  }
  if (method == "ds2") {
    ### Shuffling: compute rank order correlation matrix of (X, S):
    mat <- cbind(responses, predictors)
    sig <- cr(mat, covmethod)
    ## convert X, S to X*, S*: compute ranks ranks <- apply(mat, 2, function(x) sort(x,
    ## index.return=TRUE)$ix)
    ranks <- apply(mat, 2, rank)
    # ranks[,4:10] <- predictors##ALEX ranks[,4:10] <- ranks[,4:10]*(nrow(ranks)-1)
    # ranks[,4:10] <- ranks[,4:10]+1 compute percentiles p
    perc <- apply(ranks, 2, function(x) (x - 0.5)/length(x))
    ## compute x*,s*
    normInvers <- apply(perc, c(2), qnorm)
    ## compute pmc:
    pmc <- 2 * sin((pi * cor(mat, method = covmethod))/6)  #(where pi = 22/7).

    Ystar <- egadp(normInvers[, 1:ncol(responses)], normInvers[, (ncol(responses) + 1):(ncol(responses) +
      ncol(predictors))], covmethod, regmethod)
  }
  if (method == "mvn") {
    ### Shuffling: compute rank correlations of the entire data set
    sig <- cr(cbind(responses, predictors), covmethod)
    ## using a multivariate normal copula
    newxs <- mvrnorm(nrow(data), mu = c(colMeans(responses), colMeans(predictors)), Sigma = cv(cbind(responses,
      predictors), covmethod))
    sigstar <- cr(newxs, covmethod)
    Ystar <- egadp(newxs[, 1:ncol(responses)], newxs[, (ncol(responses) + 1):(ncol(responses) +
      ncol(predictors))], covmethod, regmethod)
    ## reordering based on ranks sy <- apply(Ystar, 2, sort, index.return=TRUE) sx <-
    ## apply(responses, 2, sort, index.return=TRUE) sy <- sort(Ystar[,1], index.return=TRUE) sx
    ## <- sort(responses[,1], index.return=TRUE) ## shuffle: for(i in 1:ncol(responses)){
    ## Ystar[sy[[i]]$ix,i] <- responses[sx[[i]]$ix,i] }
  }
  if (method == "mlm") {
    LMres <- lm(cbind(responses) ~ predictors)  #, data=x)
    Ystar <- predict(LMres, predictors)
    # sy <- apply(Ystar, 2, sort, index.return=TRUE) sx <- apply(responses, 2, sort,
    # index.return=TRUE) for(i in 1:ncol(responses)){ Ystar[sy[[i]]$ix,i] <-
    # responses[sx[[i]]$ix,i] }
  }
  # if(method=='mlts'){ LMres <- mlts(y=responses, x=predictors, gamma=0.75) Ystar <-
  # predict(LMres, x[,predictors]) sy <- apply(Ystar, 2, sort, index.return=TRUE) sx <-
  # apply(responses, 2, sort, index.return=TRUE) for(i in 1:ncol(responses)){
  # Ystar[sy[[i]]$ix,i] <- responses[sx[[i]]$ix,i] } }
  if (gadp == TRUE && (method != "ds_cob" || method != "ds"))
    gadpres <- egadp(responses, predictors, covmethod, regmethod) else gadpres = NULL
  shuffled <- Ystar
  resp <- responses
  for (i in 1:ncol(responses)) {
    # print(reverseMap(responses[,i], Ystar[,i]))
    shuffled[, i] <- reverseMap(responses[, i], Ystar[, i])
  }

  ### TODO: DELETE THIS corm=function(x)cor(x,method='spearman') corm(shuffled)
  ### x11();pairs(shuffled) x11();pairs(responses)
  if (length(missingid) > 0) {
    for (i in 1:ncol(data)) {
      if (colnames(data)[i] %in% colnames(responses)) {
        ii <- which(colnames(responses) == colnames(data)[i])
        shuffled[missingid[[i]], ii] <- NA
        Ystar[missingid[[i]], ii] <- NA
        gadpres[missingid[[i]], ii] <- NA
      }
    }
  }
  res <- list(shuffled = shuffled, perturbed = Ystar, egapd = gadpres)
  return(res)
  # pdf('model4.pdf') par(mfrow=c(2,2), cex.main=0.7) plot(mat[,c(1,3)], main=paste('x1
  # against s1\n', form[3])) points(x=Ystar[,1], y=mat[,3], col='blue', pch=2)
  # points(x=gadpres[,1], y=mat[,3], col='red', pch=3) legend('bottomright',
  # legend=c('original', 'shuffled','egadp'), pch=c(1,2,3), col=c('black','blue','red'))
  # plot(mat[,c(2,3)], main=paste('x2 against s1\n', form[3])) points(x=Ystar[,2],
  # y=mat[,3], col='blue', pch=2) points(x=gadpres[,2], y=mat[,3], col='red', pch=3)
  # plot(mat[,c(1,4)], main=paste('x1 against s2\n', form[3])) points(x=Ystar[,1],
  # y=mat[,4], col='blue', pch=2) points(x=gadpres[,1], y=mat[,4], col='red', pch=3)
  # plot(mat[,c(1:2)], main=paste('x1 against x2\n', form[3])) points(x=Ystar[,1],
  # y=Ystar[,2], col='blue', pch=2) points(x=gadpres[,1], y=gadpres[,2], col='red', pch=3)
  # dev.off() print(cor(cbind(Ystar[,1:2],mat[,3:3]))) print(cor(mat[,1:3]))

}
# testmat <- Prestige[1:15,1:4] library(car) data(Prestige) Prestige <- na.omit(Prestige)
# form <- formula(income + education ~ prestige + census + women + type, data=Prestige)
# form <- formula(income + education ~ type*(prestige + women) + census, data=Prestige)
# form <- formula(log(income) + education ~ prestige + census + women, data=Prestige) form
# <- formula(log(income) + education ~ prestige:type + census + women, data=Prestige) form
# <- formula(income + education ~ prestige, data=Prestige) sh <- shuffle(Prestige,form,
# regmethod='lm', covmethod='spearman') sh1 <- sh[[1]] cor(cbind(sh1[,1:2],mat[,3:3]))
# cor(mat[,1:3]) sh2 <- sh[[2]] cor(cbind(sh2[,1:2],mat[,3:3])) cor(mat[,1:3])
# par(mfrow=c(2,2)) plot(mat[,c(1,3)]) points(x=sh2[,1], y=mat[,3], col='blue', pch=2)
# points(x=sh1[,1], y=mat[,3], col='red', pch=3) plot(mat[,c(2,3)]) points(x=sh2[,2],
# y=mat[,3], col='blue', pch=2) points(x=sh1[,2], y=mat[,3], col='red', pch=3)
# plot(mat[,c(1,4)]) points(x=sh2[,1], y=mat[,4], col='blue', pch=2) points(x=sh1[,1],
# y=mat[,4], col='red', pch=3) plot(mat[,c(2,4)]) points(x=sh2[,2], y=mat[,4], col='blue',
# pch=2) points(x=sh1[,2], y=mat[,4], col='red', pch=3)
