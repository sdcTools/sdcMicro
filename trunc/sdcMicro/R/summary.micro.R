`summary.micro` <-
function (object, ...) 
{
`prcompRob` <-
function (X, k = 0, sca = "mad", scores = TRUE) 
{
    ## Copyright: Croux and Filzmoser
    n <- nrow(X)
    p <- ncol(X)
    if (k == 0) {
        p1 <- min(n, p)
    }
    else {
        p1 <- k
    }
    S <- rep(1, p1)
    V <- matrix(1:(p * p1), ncol = p1, nrow = p)
    P <- diag(p)
    m <- apply(X, 2, median)
    Xcentr <- scale(X, center = m, scale = FALSE)
    for (k in 1:p1) {
        B <- Xcentr %*% P
        Bnorm <- sqrt(apply(B^2, 1, sum))
        A <- diag(1/Bnorm) %*% B
        Y <- A %*% P %*% t(X)
        if (sca == "mad") 
            s <- apply(Y, 1, mad)
        #if (sca == "tau") 
        #    s <- apply(Y, 1, scale.tau)
        #if (sca == "A") 
        #    s <- apply(Y, 1, scale.a)
        j <- order(s)[n]
        S[k] <- s[j]
        V[, k] <- A[j, ]
        if (V[1, k] < 0) 
            V[, k] <- (-1) * V[, k]
        P <- P - (V[, k] %*% t(V[, k]))
    }
    if (scores) {
        list(scale = S, loadings = V, scores = Xcentr %*% V)
    }
    else list(scale = S, loadings = V)
}

    x1 <- as.data.frame(object$x)
    x2 <- if( length(as.data.frame(object$mx)) > 0 ) as.data.frame(object$mx) else as.data.frame(object$mx)
    colnames(x2) <- colnames(x1)
    amx <- mapply(mean, x1)
    amxn <- mapply(mean, x2)
    amean <- sum(abs(amx - amxn)/(abs(amx)))
    meds1 <- mapply(median, x1)
    meds2 <- mapply(median, x2)
    amedian <- sum(abs(meds1 - meds2)/abs(meds1), na.rm = TRUE)
    onestep <- function(x) {
        y <- x
        constant <- 3/1.486
        m1 <- mapply(median, x)
        m2 <- mapply(mad, x)
        limit1 <- m1 + constant * m2
        limit2 <- m1 - constant * m2
        for (i in 1:dim(x)[2]) {
            if (any(x[, i] > limit1[i])) {
                w <- which(x[, i] > limit1[i])
                le <- length(w)
                y[w, i] <- limit1[i]
            }
            if (any(x[, i] < limit2[i])) {
                w <- which(x[, i] < limit2[i])
                le <- length(w)
                y[w, i] <- limit2[i]
            }
        }
        y
    }
    aox <- onestep(x1)
    aox <- mapply(mean, aox)
    aoxm <- onestep(x2)
    aoxm <- mapply(mean, aoxm)
    aonestep <- sum(abs(aox - aoxm)/abs(aox), na.rm = TRUE)
    devvar <- sum(abs(var(x1) - var(x2))/abs(var(x1)))/length(x1)
    amx <- mapply(mad, x1)
    amxn <- mapply(mad, x2)
    amad <- sum(abs(amx - amxn)/(abs(amx)), na.rm = TRUE)
    acov <- sum(abs(cov(x1) - cov(x2))/abs(cov(x1)))/(2 * length(x1))
    #if (robCov == TRUE) {
    #    arcov <- sum(abs(covMcd(x1)$cov - covMcd(x2)$cov)/abs(covMcd(x1)$cov))/(2 * 
    #        length(x1))
    #}
    #else {
        arcov <- NA
    #}
    acor <- sum(abs(cor(x1) - cor(x2))/abs(cor(x1)))/(2 * length(x2))
    #if (robCov == TRUE) {
    #    arcor <- sum(abs(covMcd(x1, cor = TRUE)$cor - covMcd(x2, 
    #        cor = TRUE)$cor)/abs(covMcd(x1, cor = TRUE)$cor))/(2 * 
    #        length(x1))
    #}
    #else {
        arcor <- NA
    #}
    acors <- sum(abs(cor(x1, method = "spearman") - cor(x2, method = "spearman"))/abs(cor(x1, 
        method = "spearman")))/(2 * length(x1))
    l1 <- lm(as.matrix(x1[, 1]) ~ as.matrix(x1[, -1]))$coeff
    l2 <- lm(as.matrix(x2[, 1]) ~ as.matrix(x2[, -1]))$coeff
    adlm <- sum(abs(l1[2:length(l1)] - l2[2:length(l2)]), na.rm = TRUE)
    #if (robReg == TRUE) {
    #    l1 <- lqs(as.matrix(x1[, 1]) ~ as.matrix(x1[, -1]), method = "lts")$coeff
    #    l2 <- lqs(as.matrix(x2[, 1]) ~ as.matrix(x2[, -1]), method = "lts")$coeff
    #    adlts <- sum(abs(l1[2:length(l1)] - l2[2:length(l2)]))
    #}
    #else {
        adlts <- NA
    #}
    if (dim(x1)[1] > dim(x1)[2] && dim(x2)[1] > dim(x2)[2]) {
        p1 <- princomp(x1)
        p2 <- princomp(x2)
        cp1 <- colMeans(p1$load)
        cp2 <- colMeans(p2$load)
        apcaload <- sum(abs(cp1 - cp2)/abs(cp1))
    } else {
        apcaload = "too less observations"
    }
    if (dim(x1)[1] > dim(x1)[2] && dim(x2)[1] > dim(x2)[2]) {
        p1 <- prcompRob(x1)
        p2 <- prcompRob(x2)
        cp1 <- colMeans(p1$load)
        cp2 <- colMeans(p2$load)
        apppcaload <- sum(abs(cp1 - cp2)/abs(cp1))
    } else {
        apppcaload = "too less observations"
    }
    cmx1 <- apply(x1, 2, sum)
    cmx2 <- apply(x2, 2, sum) * object$fot
    atotals <- sum(abs((cmx1 - cmx2)/cmx1))
    pmtotals <- sum((cmx2 - cmx1)/cmx1)
    util1 <- dUtility(x1, x2)
    deigenvalues <- dUtility(x1, x2, method="eigen")
    risk0 <- dRisk(x1, x2)
    r <- dRiskRMD(x1, x2, k=0.7)
    risk1 <- r$risk1
    risk2 <- r$risk2
    wrisk1 <- r$wrisk1
    wrisk2 <- r$wrisk2
    list(meansx = summary(x1), meansxm = summary(x2), amean = amean, 
        amedian = amedian, aonestep = aonestep, devvar = devvar, 
        amad = amad, acov = acov, arcov = arcov, acor = acor, 
        arcor = arcor, acors = acors, adlm = adlm, adlts = adlts, 
        apcaload = apcaload, apppcaload = apppcaload, totalsOrig = cmx1, 
        totalsMicro = cmx2, atotals = atotals, pmtotals = pmtotals,
        util1 = util1, deigenvalues=deigenvalues, risk0=risk0,
        risk1=risk1, risk2=risk2, wrisk1=wrisk1, wrisk2=wrisk2)
}

