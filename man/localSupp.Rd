\name{localSupp}
\docType{methods}
\alias{localSupp-methods}
\alias{localSupp,ANY-method}
\alias{localSupp,sdcMicroObj-method}
\alias{localSupp}
\title{ Local Suppression }
\description{
A simple method to perfom local suppression.
}
\usage{
localSupp(obj, threshold=0.15, keyVar,...)# indivRisk)
}

\section{Methods}{
\describe{

\item{\code{signature(obj = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "sdcMicroObj")}}{
%%  ~~describe this method here~~
}
}}
\arguments{
  \item{obj}{ object of class freqCalc or sdcMicroObj }
  \item{threshold}{ threshold for individual risk }
  \item{keyVar}{ Variable on which some values might be suppressed }
  \item{...}{see arguments below}
  \item{indivRisk}{ object from class indivRisk }
}
\details{
Values of high risk (above the threshold) of a certain variable (parameter keyVar) 
are suppressed.
}
\value{
Manipulated data with suppressions or the \dQuote{sdcMicroObj} object with manipulated data.
}
\references{ 
Templ, M.  
               \emph{Statistical Disclosure Control for Microdata Using the R-Package sdcMicro}, 
               Transactions on Data Privacy, 
               vol. 1, number 2, pp. 67-85, 2008. 
  \url{http://www.tdp.cat/issues/abs.a004a08.php}
 }
\author{ Matthias Templ }
\seealso{  \code{\link{freqCalc}}, \code{\link{indivRisk}} }
\examples{
## example from Capobianchi, Polettini and Lucarelli:
data(francdat)
f <- freqCalc(francdat, keyVars=c(2,4,5,6),w=8)
f
f$fk
f$Fk
## individual risk calculation:
indivf <- indivRisk(f)  
indivf$rk         
## Local Suppression            
localS <- localSupp(f, keyVar=2, indivRisk=indivf$rk, threshold=0.25)
f2 <- freqCalc(localS$freqCalc, keyVars=c(4,5,6), w=8)
indivf2 <- indivRisk(f2)
indivf2$rk
## select another keyVar and run localSupp once again,
# if you think the table is not fully protected


## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- localSupp(sdc, keyVar='urbrur')
}
\keyword{ manip }
