\name{removeDirectID}
\docType{methods}
\alias{removeDirectID}
\alias{removeDirectID-methods}
\alias{removeDirectID,sdcMicroObj-method}
\title{ Remove certain variables from the data set inside a sdc object.}
\description{
 Delete variables without changing anything else in the sdcObject (writing NAs).
}
\usage{
removeDirectID(obj, var)
}
\arguments{
  \item{obj}{ object of class 'sdcMicroObj }
  \item{var}{name of the variable(s) to be remove}
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
\author{ Alexander Kowarik}
\examples{
## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata, keyVars=c('urbrur','roof'), 
           numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- removeDirectID(sdc, var="age")

}
\keyword{methods}