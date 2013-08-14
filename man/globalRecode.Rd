\name{globalRecode}
\docType{methods}
\alias{globalRecode-methods}
\alias{globalRecode,ANY-method}
\alias{globalRecode,sdcMicroObj-method}
\alias{globalRecode}
\title{ Global Recoding }
\description{
Global recoding
}

\section{Methods}{
\describe{

\item{\code{signature(obj = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "sdcMicroObj")}}{
%%  ~~describe this method here~~
}
}}

\usage{
globalRecode(obj,...)#, column,breaks, labels, method="equidistant")
}
\arguments{
  \item{obj}{ vector of class numeric or of class factor with integer labels for recoding or an object 
  of class sdcMicroObj}
  \item{column}{which keyVar should be changed}
  \item{...}{see possible arguments below}
  \item{breaks}{ either a numeric vector of cut points or number giving the number of intervals which x is to be cut into. }
  \item{labels}{ labels for the levels of the resulting category. By default, labels are constructed using "(a,b]" interval notation. 
                  If labels = FALSE, simple integer codes are returned instead of a factor. }
  \item{method}{ method \dQuote{equidistant} for equal sized intervalls 
  
                 method \dQuote{logEqui} for equal sized intervalls for log-transformed data
                 
                 method \dQuote{equalAmount} for intervalls with approxiomately the same amount of observations
  }
}
\details{
If a labels parameter is specified, its values are used to name the factor levels. 
If none is specified, the factor level labels are constructed. 
}
\value{
the modified \dQuote{sdcMicroObj} or a factor, unless labels = FALSE which results in the mere integer level codes.
}
\seealso{  \code{\link{cut}} }
\examples{
data(free1)
head(globalRecode(free1[,"AGE"], breaks=c(1,9,19,29,39,49,59,69,100), labels=1:8))
table(globalRecode(free1[,"AGE"], breaks=c(1,9,19,29,39,49,59,69,100), labels=1:8))
table(globalRecode(free1[,"AGE"], breaks=c(1,9,19,29,39,49,59,69,100)))
table(globalRecode(free1[,"AGE"], breaks=6))
table(globalRecode(free1[,"AGE"], breaks=6, method="logEqui"))
table(globalRecode(free1[,"AGE"], breaks=6, method="equalAmount"))

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- globalRecode(sdc, column="urbrur", breaks=5)


}
\keyword{ manip }
