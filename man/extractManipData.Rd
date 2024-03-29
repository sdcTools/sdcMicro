% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aux_functions.r
\name{extractManipData}
\alias{extractManipData}
\title{Remove certain variables from the data set inside a sdc object.}
\usage{
extractManipData(
  obj,
  ignoreKeyVars = FALSE,
  ignorePramVars = FALSE,
  ignoreNumVars = FALSE,
  ignoreGhostVars = FALSE,
  ignoreStrataVar = FALSE,
  randomizeRecords = "no"
)
}
\arguments{
\item{obj}{object of class \code{\link{sdcMicroObj-class}}}

\item{ignoreKeyVars}{If manipulated KeyVariables should be returned or the
unchanged original variables}

\item{ignorePramVars}{if manipulated PramVariables should be returned or the
unchanged original variables}

\item{ignoreNumVars}{if manipulated NumericVariables should be returned or
the unchanged original variables}

\item{ignoreGhostVars}{if manipulated Ghost (linked) Variables should be returned or
the unchanged original variables}

\item{ignoreStrataVar}{if manipulated StrataVariables should be returned or
the unchanged original variables}

\item{randomizeRecords}{(logical) specifies, if the output records should be randomized. The following
options are possible:
\describe{
\item{'no'}{default, no randomization takes place}
\item{'simple'}{records are just randomly swapped.}
\item{'byHH'}{if slot 'hhId' is not \code{NULL}, the clusters defined by this variable are randomized across the dataset. If
slot 'hhId' is \code{NULL}, the records or the dataset are randomly changed.}
\item{'withinHH'}{if slot 'hhId' is not \code{NULL}, the clusters defined by this variable are randomized across the dataset and
additionally, the order of records within the clusters are also randomly changed. If slot 'hhId' is \code{NULL}, the records or the dataset are
randomly changed.}}}
}
\value{
a \code{data.frame} containing the anonymized data set
}
\description{
Extract the manipulated data from an object of class \code{\link{sdcMicroObj-class}}
}
\examples{
## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata,
  keyVars=c('urbrur','roof'),
  numVars=c('expend','income','savings'), w='sampling_weight')
sdc <- removeDirectID(sdc, var="age")
dataM <- extractManipData(sdc)
}
\author{
Alexander Kowarik, Bernhard Meindl
}
