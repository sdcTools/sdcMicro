\name{varToFactor}
\docType{methods}
\alias{varToFactor}
\alias{varToNumeric}
\alias{varToFactor-methods}
\alias{varToNumeric-methods}
\alias{varToFactor,sdcMicroObj-method}
\alias{varToNumeric,sdcMicroObj-method}
\title{ Change the a keyVariable of an object of class 'sdcMicroObj' from Numeric to Factor or from Factor to Numeric}
\description{
 Change the scale of a variable
}
\usage{
varToNumeric(obj, var)
varToFactor(obj, var)
}
\arguments{
  \item{obj}{ object of class 'sdcMicroObj }
  \item{var}{name of the keyVariable to change}
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
sdc <- varToFactor(sdc, var="urbrur")

}
\keyword{methods}