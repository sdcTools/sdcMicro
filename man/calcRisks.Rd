\name{calcRisks}
\docType{methods}
\alias{calcRisks-methods}
\alias{calcRisks}
\alias{calcRisks,sdcMicroObj-method}
\title{Recompute Risk and Frequencies for a sdcMicroObj}
\description{
 Recomputation of Risk should be done after manual changing the content of an object of class 'sdcMicroObj'
}
\usage{
calcRisks(obj,...)
}
\arguments{
\item{obj}{
  	an object of class 'sdcMicroObj'
  }

  \item{...}{no arguments at the moment}
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "sdcMicroObj")}}{
%%  ~~describe this method here~~
}
}}
\details{
By applying this function, the dislosure risk is re-estimated and the corresponding slots of an object of 
class \dQuote{sdcMicro} are updated. This function mostly used internally to automatically update the risk after
an sdc method is applied.
}
\seealso{ \code{\link{sdcMicroObj-class}}}
\examples{
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- calcRisks(sdc)
}
\keyword{methods}
