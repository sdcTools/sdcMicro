\name{measure_risk}
\docType{methods}
\alias{measure_risk-methods}
\alias{measure_risk,data.frame-method}
\alias{measure_risk,matrix-method}
\alias{measure_risk,sdcMicroObj-method}
\alias{measure_risk}
\alias{ldiversity}
\alias{ldiversity-methods}
\alias{ldiversity,data.frame-method}
\alias{ldiversity,matrix-method}
\alias{ldiversity,sdcMicroObj-method}
\alias{print.measure_risk}
\alias{print.ldiversity}
%\alias{measure_hier}
%\alias{print.measure_hier}
%\alias{measure_thres}
%\alias{print.measure_thres}
\title{
Disclosure Risk for Categorical Variables
}
\description{
The function measures the disclosure risk for weighted or unweighted data.
It computes the individual risk (and household risk if reasonable) and the global risk.
It also computes a risk threshold based on a global risk value.

To be used when risk of disclosure for individuals within a family is considered to be 
statistical independent.

Internally, function \emph{freqCalc()} and \emph{indivRisk} are used for estimation.
}
\usage{
measure_risk(obj,...)
#measure_risk(data,keyVars,w=NULL,missing=-999,
#hid=NULL,max_global_risk=.01,fast_hier=TRUE)
ldiversity(obj,ldiv_index,l_recurs_c=2,missing=-999,...)
\method{print}{measure_risk}(x, ...)
\method{print}{ldiversity}(x, ...)
%measure_hier(data,variables,weight_variable=NULL,hid,missing=-999)
%\method{print}{measure_hier}(x, ...)
%measure_thres(data,variables,weight_variable=NULL,missing=-999)
%\method{print}{measure_thres}(x, ...)
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "data.frame")}}{
Method for object of class \dQuote{data.frame}
}

\item{\code{signature(obj = "matrix")}}{
Method for object of class \dQuote{matrix}
}

\item{\code{signature(obj = "sdcMicroObj")}}{
Method for object of S4 class \dQuote{sdcMicroObj}
}
}}
\arguments{

\item{obj}{Object of class \dQuote{sdcMicroObjet}}
\item{...}{see arguments below}
  \item{data}{
  Input data, either a matrix or a data.frame.
  }
  \item{keyVars}{
  Names of categorical key variables
}
  \item{w}{
  name of variable containing sample weights
}
 \item{hid}{
 name of the Household ID
}
  \item{missing}{
  A integer value to be used as missing value in the C++ routine
}
  \item{ldiv_index}{
  indices (or names) of the variables used for l-diversity
  }
 \item{l_recurs_c}{
 L-Diversity Constant
}

   \item{x}{
   Output of measure_risk, measure_hier or measure_thres
  }
  \item{max_global_risk}{Maximal global risk for threshold computation}
  \item{fast_hier}{If TRUE a faster approximation is computed if household data are provided.}
}
\value{
A modified \dQuote{sdcMicroObj} object or a 
list with the following elements:
\item{global_risk_ER}{ expected number of re-identification.}
\item{global_risk}{ global risk (sum of indivdual risks).}
\item{global_risk_pct}{ global risk in percent.}
\item{Res}{ matrix with the risk, frequency in the sample and grossed-up frequency in the population (and the hierachical risk) for each observation.}
\item{global_threshold}{ for a given max_global_risk the threshold for the risk of observations.}
\item{max_global_risk}{ the input max_global_risk of the function.}
\item{hier_risk_ER}{ expected number of re-identification with household structure.}
\item{hier_risk}{ global risk with household structure(sum of indivdual risks).}
\item{hier_risk_pct}{ global risk  with household structure in percent.}
\item{ldiverstiy}{ Matrix with Distinct_Ldiversity, Entropy_Ldiversity andRecursive_Ldiversity for each sensitivity variable.}

}
\references{
\url{http://neon.vb.cbs.nl/casc/Software/MuManual4.1.pdf}
}
\author{
Alexander Kowarik, Bernd Prantner, Matthias Templ, minor parts of IHSN C++ source
}
\seealso{
\code{\link{freqCalc}}, \code{\link{indivRisk}}
}
\examples{
## measure_risk with sdcMicro objects:
data(testdata)
sdc <- createSdcObj(testdata,
  keyVars=c('urbrur','roof','walls','water','electcon'),
numVars=c('expend','income','savings'), w='sampling_weight')
## risk is already estimated and available in...
names(sdc@risk)
## measure risk on data frames or matrices:
res <- measure_risk(testdata,
  keyVars=c("urbrur","roof","walls","water","sex"))
print(res)
head(res$Res)
resw <- measure_risk(testdata,
  keyVars=c("urbrur","roof","walls","water","sex"),w="sampling_weight")
print(resw)
head(resw$Res)
res1 <- ldiversity(testdata,
  keyVars=c("urbrur","roof","walls","water","sex"),ldiv_index="electcon")
print(res1)
head(res1)
res2 <- ldiversity(testdata,
  keyVars=c("urbrur","roof","walls","water","sex"),ldiv_index=c("electcon","relat"))
print(res2)
head(res2)
# measure risk with household risk
resh <- measure_risk(testdata,
  keyVars=c("urbrur","roof","walls","water","sex"),w="sampling_weight",hid="ori_hid")
print(resh)
# change max_global_risk
rest <- measure_risk(testdata,
  keyVars=c("urbrur","roof","walls","water","sex"),
  w="sampling_weight",max_global_risk=0.0001)
print(rest)
  
## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
## already interally applied and availabe in object sdc: 
## sdc <- measure_risk(sdc)
}
\keyword{ manip }