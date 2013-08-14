\name{topBotCoding}
\docType{methods}
\alias{topBotCoding-methods}
\alias{topBotCoding,ANY-method}
\alias{topBotCoding,sdcMicroObj-method}
\alias{topBotCoding}
\title{ Top and Bottom Coding }
\description{
Function for Top and Bottom Coding.
}
\usage{
topBotCoding(obj, value, replacement, kind = "top",column=NULL)
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
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{ vector or one-dimensional matrix or data.frame or object of class \dQuote{sdcMicroObj} }
  \item{value}{ limit, from where it should be top- or bottom-coded }
  \item{replacement}{ replacement value. }
  \item{kind}{ top or bottom }
  \item{column}{xxx}
}
\details{
Extreme values are replaced by one value to reduce the disclosure risk.
}
\value{
Top or bottom coded data or modified \dQuote{sdcMicroObj}.
}
\author{ Matthias Templ }
\seealso{ \code{\link{indivRisk}} }
\examples{
data(free1)
topBotCoding(free1[,"DEBTS"], value=9000, replacement=9100, kind="top")

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2, keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
           numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- topBotCoding(sdc, value=500000, replacement=1000, column="income")
testdataout <- extractManipData(sdc)

}
\keyword{ manip }

