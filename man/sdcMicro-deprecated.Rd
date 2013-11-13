\name{sdcMicro-deprecated}
\alias{sdcMicro-deprecated}
\alias{pram_strata}
\alias{pram_strata,ANY-method}
\title{Deprecated Functions in sdcMicro Package}

\description{
  These functions are provided for compatibility with older versions of the \pkg{sdcMicro} 
  package only, 
  and may be removed eventually. Commands that worked in versions of the 
  \pkg{sdcMicro} package prior to version 3.1.1
  will not necessarily work in version 3.1.2 and beyond, or may not work in the same manner.
}

\usage{
localSupp2(x, keyVars, w, importance=rep(1, length(keyVars)),
  method="minimizeSupp", k=1)
localSupp2Wrapper (x, keyVars, w, importance=rep(1, length(keyVars)),
  method="minimizeSupp", kAnon=2) 
pram_strata(obj, variables=NULL,strata_variables=NULL,pd=0.8, alpha=0.5)
}

\arguments{
  \item{x}{data frame or matrix }
  \item{keyVars}{column index of key variables}
  \item{w}{ column index of sampling weights }
  \item{importance}{ weights for each key variable }
  \item{method}{ \dQuote{minimizeSupp} (default), further methods will be included in future versions of the package }
  \item{k}{parameter for k-anonymity.}
  \item{kAnon}{parameter for k-anonymity.}
    \item{obj}{
  Input data. Allowed input data are objects of class 'matrix', 'data.frame', 'vector' or 'sdcMicroObj'. 
}
  \item{variables}{
  Names of variables in 'obj' on which post-randomization should be applied. If obj is a vector, this argument is ignored.
}
  \item{strata_variables}{
  Names of variables for stratification (will be set automatically for an object of class 'sdcMicroObj'
}
\item{pd}{ minimum diagonal entries for the generated transition matrix P. Either a vector of length 1 or a vector of length ( number of categories ). }
\item{alpha}{ amount of perturbation for the invariant Pram method }
  

}

\details{
\code{localSupp2} is now a synonym for \code{\link{localSuppression}}.
\code{localSupp2Wrapper} is now a synonym for \code{\link{localSuppression}}.
}