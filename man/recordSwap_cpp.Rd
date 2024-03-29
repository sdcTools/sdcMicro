% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{recordSwap_cpp}
\alias{recordSwap_cpp}
\title{Targeted Record Swapping}
\usage{
recordSwap_cpp(
  data,
  hid,
  hierarchy,
  similar_cpp,
  swaprate,
  risk,
  risk_threshold,
  k_anonymity,
  risk_variables,
  carry_along,
  log_file_name,
  seed = 123456L
)
}
\arguments{
\item{data}{micro data set containing only integer values. A data.frame or data.table from R needs to be transposed beforehand so that data.size() ~ number of records - data.[0].size ~ number of varaibles per record.
\strong{NOTE:} \emph{data has to be ordered by hid beforehand.}}

\item{hid}{column index in \code{data} which refers to the household identifier.}

\item{hierarchy}{column indices of variables in \code{data} which refers to the geographic hierarchy in the micro data set. For instance county > municipality > district.}

\item{similar_cpp}{List where each entry corresponds to column indices of variables in \code{data} which should be considered when swapping households.}

\item{swaprate}{double between 0 and 1 defining the proportion of households which should be swapped, see details for more explanations}

\item{risk}{vector of vectors containing risks of each individual in each hierarchy level.}

\item{risk_threshold}{double indicating risk threshold above every household needs to be swapped.}

\item{k_anonymity}{integer defining the threshold of high risk households (k-anonymity). This is used as k_anonymity <= counts.}

\item{risk_variables}{column indices of variables in \code{data} which will be considered for estimating the risk.}

\item{carry_along}{integer vector indicating additional variables to swap besides to hierarchy variables.
These variables do not interfere with the procedure of finding a record to swap with or calculating risk.
This parameter is only used at the end of the procedure when swapping the hierarchies.}

\item{log_file_name}{character, path for writing a log file. The log file contains a list of household IDs (`hid`) which could not have been swapped and is only created if any such households exist.}

\item{seed}{integer defining the seed for the random number generator, for reproducibility.}
}
\value{
Returns data set with swapped records.
}
\description{
Applies targeted record swapping on micro data set, see \code{?recordSwap} for details.
\cr
\strong{NOTE:} This is an internal function called by the R-function \code{recordSwap()}. It's only purpose is to include the C++-function recordSwap() using Rcpp.
}
