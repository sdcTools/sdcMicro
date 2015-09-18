#' Statistical Disclosure Control (SDC) for the generation of protected
#' microdata for researchers and for public use.
#'
#' This package includes all methods of the popular software mu-Argus plus
#' several new methods. In comparison with mu-Argus the advantages of this
#' package are that the results are fully reproducible even with the included
#' GUI, that the package can be used in batch-mode from other software, that
#' the functions can be used in a very flexible way, that everybody could look
#' at the source code and that there are no time-consuming meta-data management
#' is necessary. However, the user should have a detailed knowledge about SDC
#' when applying the methods on data.
#'
#' The package is programmed using S4-classes and it comes with a well-defined
#' class structure.
#'
#' The implemented graphical user interface (GUI) for microdata protection
#' serves as an easy-to-handle tool for users who want to use the sdcMicro
#' package for statistical disclosure control but are not used to the native R
#' command line interface.  In addition to that, interactions between objects
#' which results from the anonymization process are provided within the GUI.
#' This allows an automated recalculation and displaying information of the
#' frequency counts, individual risk, information loss and data utility after
#' each anonymization step. In addition to that, the code for every
#' anonymization step carried out within the GUI is saved in a script which can
#' then be easily modified and reloaded.
#'
#' \tabular{ll}{ Package: \tab sdcMicro\cr Type: \tab Package\cr Version: \tab
#' 2.5.9\cr Date: \tab 2009-07-22\cr License: \tab GPL 2.0 \cr }
#'
#' @name sdcMicro-package
#' @aliases sdcMicro-package sdcMicro
#' @docType package
#' @author Matthias Templ, Alexander Kowarik, Bernhard Meindl
#'
#' Maintainer: Matthias Templ <templ@@statistik.tuwien.ac.at>
#' @references Templ, M. and Meindl, B. \emph{Practical Applications in
#' Statistical Disclosure Control Using R}, Privacy and Anonymity in
#' Information Management Systems, Bookchapter, Springer London, pp. 31-62,
#' 2010
#'
#' Kowarik, A. and Templ, M. and Meindl, B. and Fonteneau, F. and Prantner, B.:
#' \emph{Testing of IHSN Cpp Code and Inclusion of New Methods into sdcMicro},
#' in: Lecture Notes in Computer Science, J. Domingo-Ferrer, I. Tinnirello
#' (editors.); Springer, Berlin, 2012, ISBN: 978-3-642-33626-3, pp. 63-77.
#'
#' Templ, M.  \emph{Statistical Disclosure Control for Microdata Using the
#' R-Package sdcMicro}, Transactions on Data Privacy, vol. 1, number 2, pp.
#' 67-85, 2008. \url{http://www.tdp.cat/issues/abs.a004a08.php}
#'
#' Templ, M.  \emph{New Developments in Statistical Disclosure Control and
#' Imputation: Robust Statistics Applied to Official Statistics},
#' Suedwestdeutscher Verlag fuer Hochschulschriften, 2009, ISBN: 3838108280,
#' 264 pages.
#' @keywords package
#' @examples
#'
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
#' f
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
#' ## individual risk calculation:
#' indivf <- indivRisk(f)
#' indivf$rk
#' ## Local Suppression
#' localS <- localSupp(f, keyVar=2, indivRisk=indivf$rk, threshold=0.25)
#' f2 <- freqCalc(localS$freqCalc, keyVars=c(2,4,5,6), w=8)
#' indivf2 <- indivRisk(f2)
#' indivf2$rk
#'
#' ## select another keyVar and run localSupp once again,
#' #if you think the table is not fully protected
#' data(free1)
#' f <- freqCalc(free1, keyVars=1:3, w=30)
#' ind <- indivRisk(f)
#' ## and now you can use the interactive plot for individual risk objects:
#' ## plot(ind)
#'
#' ## example from Capobianchi, Polettini and Lucarelli:
#' data(francdat)
#' l1 <- localSuppression(francdat, keyVars=c(2,4,5,6), importance=c(1,3,2,4))
#' l1
#' l1$x
#' l2 <- localSuppression(francdat, keyVars=c(2,4,5,6), k=2)
#' l3 <- localSuppression(francdat, keyVars=c(2,4,5,6), k=4)
#'
#' ## Data from mu-Argus:
#' ## Global recoding:
#' data(free1)
#' free1[, "AGE"] <- globalRecode(free1[,"AGE"], c(1,9,19,29,39,49,59,69,100), labels=1:8)
#'
#' ## Top coding:
#' topBotCoding(free1[,"DEBTS"], value=9000, replacement=9100, kind="top")
#'
#' ## Numerical Rank Swapping:
#' ## do not use the mu-Argus test data set (free1)
#' # since the numerical variables are (probably) faked.
#' data(Tarragona)
#' Tarragona1 <- rankSwap(Tarragona, P=10)
#'
#' ## Microaggregation:
#' m1 <- microaggregation(Tarragona, method="onedims", aggr=3)
#' m2 <- microaggregation(Tarragona, method="pca", aggr=3)
#' # summary(m1)
#' ## approx. 1 minute computation time
#' # valTable(Tarragona, method=c("simple","onedims","pca"))
#'
#'
#' data(microData)
#' m1 <- microaggregation(microData, method="mdav")
#' x <- m1$x  ### fix me
#' summary(m1)
#' plotMicro(m1, 0.1, which.plot=1)  # too less observations...
#' data(free1)
#' plotMicro(microaggregation(free1[,31:34], method="onedims"), 0.1, which.plot=1)
#'
#'
#' ## disclosure risk (interval) and data utility:
#' m1 <- microaggregation(Tarragona, method="onedims", aggr=3)
#' dRisk(obj=Tarragona, xm=m1$mx)
#' dRisk(obj=Tarragona, xm=m2$mx)
#' dUtility(obj=Tarragona, xm=m1$mx)
#' dUtility(obj=Tarragona, xm=m2$mx)
#'
#' ## S4 class code for Adding Noise methods will be included
#' #in the next version of sdcMicro.
#'
#' ## Fast generation of synthetic data with aprox.
#' #the same covariance matrix as the original one.
#'
#' data(mtcars)
#' cov(mtcars[,4:6])
#' cov(dataGen(mtcars[,4:6],n=200))
#' pairs(mtcars[,4:6])
#' pairs(dataGen(mtcars[,4:6],n=200))
#'
#' ## PRAM
#'
#' set.seed(123)
#' x <- factor(sample(1:4, 250, replace=TRUE))
#' pr1 <- pram(x)
#' length(which(pr1$xpramed == x))
#' x2 <- factor(sample(1:4, 250, replace=TRUE))
#' length(which(pram(x2)$xpramed == x2))
#'
#' data(free1)
#' marstatPramed <- pram(free1[,"MARSTAT"])
#' \dontrun{
#' # FOR OBJECTS OF CLASS sdcMicro
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
NULL
