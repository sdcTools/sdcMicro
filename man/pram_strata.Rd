\name{pram_strata}
\docType{methods}
\alias{pram_strata-methods}
\alias{pram_strata,data.frame-method}
\alias{pram_strata,matrix-method}
\alias{pram_strata,sdcMicroObj-method}
\alias{pram_strata}
\alias{print.pram_strata}
\title{
Post Randomization
}
\description{
To be used on categorical data. It randomly change the values of variables on selected records 
(usually the risky ones) according to an invariant probability transition matrix.
}
\usage{
pram_strata(obj, variables=NULL,strata_variables=NULL,
        weights=NULL, pd=0.8, alpha=0.5)
\method{print}{pram_strata}(x, ...)
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
  \item{variables}{
  Names of variables for Post Randomization
}
  \item{strata_variables}{
  Names of variables for stratification (will be set automatically for an object of class 'sdcMicroObj'
}
  \item{weights}{
  a weight for each variable
}
\item{x}{
 Output of pram_strata
}
\item{...}{
further input
}
\item{pd}{ minimum diagonal entries for the generated transition matrix P. Either a vector of length 1 or a vector of length ( number of categories ). }
\item{alpha}{ amount of perturbation for the invariant Pram method }
  
}
\value{
a modified \dQuote{sdcMicroObj} object or new variables with the suffix "_pram" will  be created.
}
\references{
\url{http://www.gnu.org/software/glpk}

\url{http://www.ccsr.ac.uk/sars/guide/2001/pram.pdf}
}
\author{
Alexander Kowarik, Matthias Templ
}
\seealso{\code{pram}}
\examples{
data(testdata)
res <- pram_strata(testdata,
  variables="roof",strata_variables=c("urbrur","sex"))
print.pram_strata(res)
res1 <- pram_strata(testdata,variables=c("roof","walls","water"),
  strata_variables=c("urbrur","sex"))
print.pram_strata(res1)
res2 <- pram_strata(testdata,variables=c("roof","walls","water"),
  strata_variables=NULL)
print.pram_strata(res2)

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- pram_strata(sdc, variables=c("urbrur"))
  
}
\keyword{ manip }