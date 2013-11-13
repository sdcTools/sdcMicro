\name{pram}
\docType{methods}
\alias{pram-methods}
\alias{pram,data.frame-method}
\alias{pram,matrix-method}
\alias{pram,vector-method}
\alias{pram,sdcMicroObj-method}

\alias{pram}
\alias{print.pram}
\title{
Post Randomization
}
\description{
To be used on categorical data. It randomly change the values of variables on selected records 
(usually the risky ones) according to an invariant probability transition matrix.
}
\usage{
pram(obj, variables=NULL,strata_variables=NULL,pd=0.8, alpha=0.5)
\method{print}{pram}(x, ...)
}
\section{Methods}{
\describe{
\item{\code{signature(obj = "data.frame")}}{}

\item{\code{signature(obj = "data.frame")}}{}

\item{\code{signature(obj = "matrix")}}{}

\item{\code{signature(obj = "sdcMicroObj")}}{}

\item{\code{signature(obj = "ANY")}}{}
}}
\arguments{
  \item{obj}{
  Input data. Allowed input data are objects of class 'matrix', 'data.frame', 'vector' or 'sdcMicroObj'. 
}
  \item{variables}{
  Names of variables in 'obj' on which post-randomization should be applied. If obj is a vector, this argument is ignored.
}
  \item{strata_variables}{
  Names of variables for stratification (will be set automatically for an object of class 'sdcMicroObj'
}
\item{x}{
 Output of pram()
}
\item{...}{
further input, currently ignored.
}
\item{pd}{ minimum diagonal entries for the generated transition matrix P. Either a vector of length 1 or a vector of length ( number of categories ). }
\item{alpha}{ amount of perturbation for the invariant Pram method }
  
}
\value{
a modified \dQuote{sdcMicroObj} object or a new object containing original and post-randomized variables (with suffix "_pram").
}
\references{
\url{http://www.gnu.org/software/glpk}

\url{http://www.ccsr.ac.uk/sars/guide/2001/pram.pdf}
}
\author{
Alexander Kowarik, Matthias Templ, Bernhard Meindl
}
\note{The functionalities of pram_strata and pram are merged into pram, therefore pram_strata is deprecated.} 
\examples{
data(testdata)
res <- pram(testdata,
  variables="roof",
  strata_variables=c("urbrur","sex"))
print.pram(res)

res1 <- pram(testdata,variables=c("roof","walls","water"),strata_variables=c("urbrur","sex"))
print.pram(res1)
res2 <- pram(testdata,variables=c("roof","walls","water"),
  strata_variables=NULL)
print.pram(res2)

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- pram(sdc, variables=c("urbrur"))  
}
\keyword{ manip }