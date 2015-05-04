\name{dUtility}
\docType{methods}
\alias{dUtility-methods}
\alias{dUtility,data.frame-method}
\alias{dUtility,matrix-method}
\alias{dUtility,sdcMicroObj-method}
\alias{dUtility}
\title{ data utility }
\description{
  IL1s data utility.
}
\usage{
dUtility(obj,...)#, xm, method="IL1")
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
\arguments{
  \item{obj}{ original data  or object of class sdcMicroObj}
  \item{...}{see arguments below}
  \item{xm}{ perturbed data }
  \item{method}{ method IL1 or eigen. More methods are implemented in summary.micro() }
}
\details{
The standardised distances of the perturbed data values to the original ones are measured. 
Measure IL1 measures the distances between 
the original values and the perturbed ones, scaled by the standard deviation. 
Method \sQuote{eigen} and \sQuote{robeigen} compares the 
eigenvalues and robust eigenvalues form the original data and the perturbed data.
}
\value{
data utility or modified entry for data utility the \dQuote{sdcMicroObj}.
}
\references{for IL1s: see \url{http://vneumann.etse.urv.es/webCrises/publications/isijcr/lncs3050Outlier.pdf},
 
 Templ, M. and Meindl, B., 
               \emph{Robust Statistics Meets {SDC}: New Disclosure Risk Measures for 
               Continuous Microdata Masking}, 
               Lecture Notes in Computer Science, Privacy in Statistical Databases, 
               vol. 5262, pp. 113-126, 2008.          
}
\author{ Matthias Templ }
\seealso{  \code{\link{dRisk}}, \code{\link{dRiskRMD}} }
\examples{
data(free1)
m1 <- microaggregation(free1[, 31:34], method="onedims", aggr=3)
m2 <- microaggregation(free1[, 31:34], method="pca", aggr=3)
dRisk(obj=free1[, 31:34], xm=m1$mx)
dRisk(obj=free1[, 31:34], xm=m2$mx)
dUtility(obj=free1[, 31:34], xm=m1$mx)
dUtility(obj=free1[, 31:34], xm=m2$mx)
data(Tarragona)
x <- Tarragona[, 5:7]
y <- addNoise(x)$xm
dRiskRMD(x, xm=y)
dRisk(x, xm=y)
dUtility(x, xm=y)
dUtility(x, xm=y, method="eigen")
dUtility(x, xm=y, method="robeigen")

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
## this is already made internally: 
## sdc <- dUtility(sdc)
## and already stored in sdc
}
\keyword{ manip }