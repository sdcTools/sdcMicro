\name{LocalRecProg}
\docType{methods}
\alias{LocalRecProg-methods}
\alias{LocalRecProg,data.frame-method}
\alias{LocalRecProg,matrix-method}
\alias{LocalRecProg,sdcMicroObj-method}
\alias{LocalRecProg}
\title{
Local recoding via Edmond's maximum weighted matching algorithm
}
\description{
To be used on both categorical and numeric input variables, although usage on categorical variables is the focus of the
development of this software.

Each record in the data represents a category of the original data, and hence all records in the input
data should be unique by the N Input Variables. To achieve bigger category sizes (k-anoymity), one can
form new categories based on the recoding result and repeatedly apply this algorithm. 
}
\usage{
LocalRecProg(obj,ancestors=NULL,ancestor_setting=NULL,
  k_level=2, FindLowestK=TRUE, weight=NULL, lowMemory=FALSE, missingValue=NA,...)
  #,categorical,numerical=NULL
  
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
  \item{obj}{
  Input data or object of class sdcMicroObj
}
 \item{ancestors}{
  Names of ancestors of the cateorical variables
}
  \item{ancestor_setting}{
  For each ancestor the corresponding categorical variable
}
 \item{k_level}{
 Level for k-anonymity
}
  \item{FindLowestK}{
  requests the program to look for the smallest k that results in complete matches of the data.
  }
   \item{weight}{
   A weight for each variable (Default=1)
  }
 \item{lowMemory}{
  Slower algorithm with less memory consumption
}
 \item{missingValue}{
  The output value for a suppressed value.
}
\item{...}{see arguments below}
  \item{categorical}{
  Names of categorical variables
}
  \item{numerical}{
  Names of numerical variables
}
}
\references{
\url{http://www.stat.t.u-tokyo.ac.jp/~takemura/papers/localrec.pdf}
}
\author{
Alexander Kowarik, Bernd Prantner, IHSN C++ source, Akimichi Takemura
}
\value{
dataframe with original variables and the supressed variables (suffix _lr). / 
 the modified \dQuote{sdcMicroObj}
}
\examples{
# LocalRecProg
data(testdata2)
r1=LocalRecProg(testdata2,
  categorical=c("urbrur", "roof", "walls", "water", "sex", "relat"),
  missingValue=-99)
r2=LocalRecProg(testdata2,
  categorical=c("urbrur", "roof", "walls", "water", "sex", "relat"),
    ancestor=c("water2", "water3", "relat2"),
    ancestor_setting=c("water","water","relat"),missingValue=-99)
r3=LocalRecProg(testdata2,
  categorical=c("urbrur", "roof", "walls", "water", "sex", "relat"),
    ancestor=c("water2", "water3", "relat2"),
    ancestor_setting=c("water","water","relat"),missingValue=-99,
    FindLowestK=FALSE)

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- LocalRecProg(sdc)
}
\keyword{ manip }