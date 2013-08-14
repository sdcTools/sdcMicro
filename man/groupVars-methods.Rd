\name{groupVars}
\docType{methods}
\alias{groupVars}
\alias{groupVars-methods}
\alias{groupVars,sdcMicroObj-method}
\title{ Join levels of a keyVariable in an object of class 'sdcMicroObj'}
\description{
 Transforms the factor variable into a factors with less levels and recomputes risk.
}
\usage{
groupVars(obj, var, before, after)
}
\arguments{
  \item{obj}{ object of class 'sdcMicroObj }
  \item{var}{name of the keyVariable to change}
  \item{before}{vector of levels before}
  \item{after}{vector of levels after}  
}  
\section{Methods}{
\describe{

\item{\code{signature(obj = "sdcMicroObj")}}{
%%  ~~describe this method here~~
}
}}
\value{
 the modified \dQuote{sdcMicroObj}
}
\examples{
## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- groupVars(sdc, var="urbrur", before=c(1,2), after=1)

}
\keyword{methods}
