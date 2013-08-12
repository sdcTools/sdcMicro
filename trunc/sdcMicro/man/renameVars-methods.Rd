\name{renameVars}
\docType{methods}
\alias{renameVars}
\alias{renameVars-methods}
\alias{renameVars,sdcMicroObj-method}
\title{ Change the name of levels of a keyVariable in an object of class 'sdcMicroObj'}
\description{
 Change the labels of levels.
}
\usage{
renameVars(obj, var, before, after)
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
sdc <- renameVars(sdc, var="urbrur", before=2, after=78)

}
\keyword{manip}