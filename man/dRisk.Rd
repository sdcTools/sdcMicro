\name{dRisk}
\docType{methods}
\alias{dRisk-methods}
\alias{dRisk,data.frame-method}
\alias{dRisk,matrix-method}
\alias{dRisk,sdcMicroObj-method}
\alias{dRisk}
\title{ overal disclosure risk }
\description{
  Distance-based disclosure risk estimation via standard deviation-based intervals around observations.
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "data.frame")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "matrix")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "sdcMicroObj")}}{
%%  ~~describe this method here~~
}
}}
\usage{
dRisk(obj,...)# xm, k = 0.05)
}
\arguments{
  \item{obj}{ original data or object of class sdcMicroObj  }
  \item{...}{see possible arguments below}
  \item{xm}{ perturbed data }
  \item{k}{ percentage of the standard deviation }
}
\details{
An interval (based on the standard deviation) is built around each value of the perturbed value. 
Then we look if the original values lay in these intervals or not.
With parameter k one can enlarge or down scale the interval.
}
\value{
The disclosure risk or/and the modified \dQuote{sdcMicroObj}
}
\references{ see method SDID in \url{http://vneumann.etse.urv.es/publications/sci/lncs3050Outlier.pdf} }
\author{Matthias Templ }
\seealso{ \code{\link{dUtility}}, \code{\link{dUtility}} }
\examples{
data(free1)
m1 <- microaggregation(free1[, 31:34], method="onedims", aggr=3)
m2 <- microaggregation(free1[, 31:34], method="pca", aggr=3)
dRisk(obj=free1[, 31:34], xm=m1$mx)
dRisk(obj=free1[, 31:34], xm=m2$mx)
dUtility(obj=free1[, 31:34], xm=m1$mx)
dUtility(obj=free1[, 31:34], xm=m2$mx)

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
## this is already made internally: sdc <- dRisk(sdc)
## and already stored in sdc
}
\keyword{ manip }
