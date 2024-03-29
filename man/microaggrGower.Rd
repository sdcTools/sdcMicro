% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/maG.R
\docType{methods}
\name{microaggrGower}
\alias{microaggrGower}
\title{Microaggregation for numerical and categorical key variables based on a
distance similar to the Gower Distance}
\usage{
microaggrGower(
  obj,
  variables = NULL,
  aggr = 3,
  dist_var = NULL,
  by = NULL,
  mixed = NULL,
  mixed.constant = NULL,
  trace = FALSE,
  weights = NULL,
  numFun = mean,
  catFun = VIM::sampleCat,
  addRandom = FALSE
)
}
\arguments{
\item{obj}{\code{\link{sdcMicroObj-class}}-object or a \code{data.frame}}

\item{variables}{character vector with names of variables to be aggregated
(Default for sdcMicroObj is all keyVariables and all numeric key variables)}

\item{aggr}{aggregation level (default=3)}

\item{dist_var}{character vector with variable names for distance
computation}

\item{by}{character vector with variable names to split the dataset before
performing microaggregation (Default for sdcMicroObj is strataVar)}

\item{mixed}{character vector with names of mixed variables}

\item{mixed.constant}{numeric vector with length equal to mixed, where the
mixed variables have the probability mass}

\item{trace}{TRUE/FALSE for some console output}

\item{weights}{numerical vector with length equal the number of variables
for distance computation}

\item{numFun}{function: to be used to aggregated numerical variables}

\item{catFun}{function: to be used to aggregated categorical variables}

\item{addRandom}{TRUE/FALSE if a random value should be added for the
distance computation.}
}
\value{
The function returns the updated sdcMicroObj or simply an altered
data frame.
}
\description{
The microaggregation is based on the distances computed similar to the Gower
distance. The distance function makes distinction between the variable types
factor,ordered,numerical and mixed (semi-continuous variables with a fixed
probability mass at a constant value e.g. 0)
}
\details{
The function sampleCat samples with probabilities corresponding to the
occurrence of the level in the NNs. The function maxCat chooses the level
with the most occurrences and random if the maximum is not unique.
}
\note{
In each by group all distance are computed, therefore introducing more
by-groups significantly decreases the computation time and memory
consumption.
}
\examples{

data(testdata,package="sdcMicro")
testdata <- testdata[1:200,]
\donttest{
for(i in c(1:7,9)) testdata[,i] <- as.factor(testdata[,i])
test <- microaggrGower(testdata,variables=c("relat","age","expend"),
  dist_var=c("age","sex","income","savings"),by=c("urbrur","roof"))

sdc <- createSdcObj(testdata,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
  numVars=c('expend','income','savings'), w='sampling_weight')

sdc <- microaggrGower(sdc)
}
}
\seealso{
\code{\link[VIM]{sampleCat}} and \code{\link[VIM]{maxCat}}
}
\author{
Alexander Kowarik
}
