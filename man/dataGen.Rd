\name{dataGen}
\docType{methods}
\alias{dataGen}
\alias{dataGen-methods}
\alias{dataGen,data.frame-method}
\alias{dataGen,matrix-method}
\alias{dataGen,sdcMicroObj-method}
\title{ Fast generation of synthetic data }
\description{
  Fast generation of (primitive) synthetic multivariate normal data.
}
\usage{
dataGen(obj,...)# n = 200)
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
  \item{obj}{ data.frame or matix }
  \item{...}{see possible arguments below}
  \item{n}{ amount of observations for the generated data }
}
\details{
Uses the cholesky decomposition to generate synthetic data with approx. the same means and covariances. 
For details see at the reference.
}
\value{
 the generated synthetic data.
}
\references{ Have a look at \url{http://crises2-deim.urv.cat/docs/publications/lncs/443.pdf} }
\author{ Matthias Templ }
\note{ With this method only multivariate normal distributed data with approxiomately the same covariance as the original data can be generated
without reflecting the distribution of real complex data, which are, in general, not follows 
a multivariate normal distribution. 
}
\seealso{ \code{\link{sdcMicroObj-class}}, \code{\link{shuffle}}}
\examples{
data(mtcars)
cov(mtcars[,4:6])
cov(dataGen(mtcars[,4:6]))
pairs(mtcars[,4:6])
pairs(dataGen(mtcars[,4:6]))

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- dataGen(sdc)
}
\keyword{ manip }

