\name{extractManipData}
\docType{methods}
\alias{extractManipData}
\alias{extractManipData-methods}
\alias{extractManipData,sdcMicroObj-method}
\title{ Remove certain variables from the data set inside a sdc object.}
\description{
 Extract the manipulated data from an object of class 'sdcMicroObj'
}
\usage{
extractManipData(obj,ignoreKeyVars=FALSE,ignorePramVars=FALSE,ignoreNumVars=FALSE,
        ignoreStrataVar=FALSE)
}
\arguments{
  \item{obj}{ object of class 'sdcMicroObj }
  \item{ignoreKeyVars}{If manipulated KeyVariables should be returned or the unchanged original variable}
 	\item{ignorePramVars}{If manipulated PramVariables should be returned or the unchanged original variable}
  \item{ignoreNumVars}{If manipulated NumericVariables should be returned or the unchanged original variable}
  \item{ignoreStrataVar}{If manipulated StrataVariables should be returned or the unchanged original variable}
}  
\section{Methods}{
\describe{

\item{\code{signature(obj = "sdcMicroObj")}}{
%%  ~~describe this method here~~
}
}}
\value{
 a data frame
}
\author{ Alexander Kowarik}
\examples{
## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata,
  keyVars=c('urbrur','roof'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- removeDirectID(sdc, var="age")
dataM <- extractManipData(sdc)
}
\keyword{methods}