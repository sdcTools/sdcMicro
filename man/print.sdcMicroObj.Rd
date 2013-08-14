\name{freq-methods}
\docType{methods}
\alias{print-methods}
\alias{freq-methods}
\alias{print,sdcMicroObj-method}
\alias{freq,sdcMicroObj-method}
\alias{freq}
\alias{print}

\title{Print and Extractor Functions for objects of class 'sdcMicroObj'}
\description{
 Descriptive print function for Frequencies, local Supression, Recoding, categorical risk and numerical risk.
}
\usage{
print(x,...)
freq(obj,type="fk")
%\method{print}{sdcMicroObj}(x, type="freq",...)
%\method{freq}{sdcMicroObj}(obj, type="fk")
}
\arguments{
  \item{x}{An object of class 'sdcMicroObj'}
  \item{obj}{An object of class 'sdcMicroObj'}
  \item{type}{Selection of the content to be returned or printed-}
  \item{...}{the type argument for the print method}
}
\details{
Possible values for the type argument of the print function are:
"freq": for Frequencies,
"ls": for Local Supression output,
"recode":for Recodes,
"risk": forCategorical risk  and
"numrisk": for Numerical risk.

Possible values for the type argument of the freq function are:
"fk": Sample frequencies and 
"Fk": weighted frequencies.


}
\author{
Alexander Kowarik, Matthias Templ
}

\examples{
data(testdata)
sdc <- createSdcObj(testdata,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
  numVars=c('expend','income','savings'), w='sampling_weight')
fk=freq(sdc)
Fk=freq(sdc,type="Fk")
print(sdc)
print(sdc,type="ls")
print(sdc,type="recode")
print(sdc,type="risk")
print(sdc,type="numrisk")
}
\keyword{classes}
