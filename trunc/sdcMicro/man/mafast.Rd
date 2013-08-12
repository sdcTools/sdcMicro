\name{mafast}
\docType{methods}
\alias{mafast-methods}
\alias{mafast,ANY-method}
\alias{mafast,data.frame-method}
\alias{mafast,matrix-method}
\alias{mafast,sdcMicroObj-method}
\alias{mafast}
\title{ Fast and Simple Microaggregation}
\description{
  Function to perform a fast and simple (primitive) method of microaggregation. (for large datasets)
}
\usage{
mafast(obj, variables=NULL,by=NULL,aggr=3,measure=mean)
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "ANY")}}{
%%  ~~describe this method here~~
}

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
  \item{obj}{ either an object of class sdcMicroObj or a data frame or matrix }
  \item{variables}{variables to microaggregate. If obj is of class sdcMicroObj the numerical key variables are chosen per default.}
  \item{by}{grouping variable for microaggregation. If obj is of class sdcMicroObj the strata variables are chosen per default.}
  \item{aggr}{ aggregation level (default=3)}
  \item{measure}{ aggregation statistic, mean, median, trim, onestep (default = mean) }
  
  %\item{blow}{ if TRUE, the microaggregated data will have the same dimension as the original data set }
  %\item{blowxm}{ the microaggregated data with the same dimension as the original one. }
}
\value{
If \sQuote{obj} was of class \dQuote{sdcMicroObj} the corresponding slots are filled, 
  like manipNumVars, risk and utility. 
If \sQuote{obj} was of class \dQuote{data.frame} or \dQuote{matrix} an object of the same class is returned. 
}
\author{ Alexander Kowarik}
\seealso{ \code{\link{microaggregation}}}
\examples{
data(Tarragona)
m1 <- mafast(Tarragona, variables=c("GROSS.PROFIT","OPERATING.PROFIT","SALES"),aggr=3)
data(testdata)
m2 <- mafast(testdata,variables=c("expend","income","savings"),aggr=50,by="sex")
summary(m2)

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- dRisk(sdc)
sdc@risk$numeric
sdc1 <- mafast(sdc,aggr=4)
sdc1@risk$numeric

sdc2 <- mafast(sdc,aggr=10)
sdc2@risk$numeric
\dontrun{
### Performance tests
x <- testdata
for(i in 1:20){
  x <- rbind(x,testdata)
}
system.time(xx <- mafast(x,variables=c("expend","income","savings"),aggr=50,by="sex"))
}




}
\keyword{ manip}
