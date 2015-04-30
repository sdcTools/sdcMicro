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
  \item{before}{vector of levels before recoding}
  \item{after}{vector of levels after recoding}
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "sdcMicroObj")}}{

This method transform a factor variable with some levels into a new factor variable with less levels.
The user must make sure that all levels of the original variable are listed in argument 'before'
and that the number of elements in argument 'after' (the new levels) have the same length.
This means that there should be a one to one mapping from any level of the original factor to a level in the recoded variable.
}
}}
\value{
 the modified \dQuote{sdcMicroObj}
}
\examples{
## for objects of class sdcMicro:
data(testdata2)
testdata2$urbrur <- as.factor(testdata2$urbrur)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- groupVars(sdc, var="urbrur", before=c("1","2"), after=c("1","1"))

}
\keyword{methods}
