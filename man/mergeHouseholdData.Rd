% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GUIfunctions.R
\name{mergeHouseholdData}
\alias{mergeHouseholdData}
\title{Replaces the raw household-level data with the anonymized household-level data in the full dataset
for anonymization of data with a household structure (or other hierarchical structure).
Requires a matching household ID in both files.}
\usage{
mergeHouseholdData(dat, hhId, dathh)
}
\arguments{
\item{dat}{a data.frame with the full dataset}

\item{hhId}{name of the household (cluster) ID (identical in both datasets)}

\item{dathh}{a dataframe with the treated household level data (generated for example with \link{selectHouseholdData})}
}
\value{
a data.frame with the treated household level variables and the raw individual level variables
}
\description{
Replaces the raw household-level data with the anonymized household-level data in the full dataset
for anonymization of data with a household structure (or other hierarchical structure).
Requires a matching household ID in both files.
}
\examples{
## Load data
x <- testdata
\donttest{
## donttest is necessary because of 
## Examples with CPU time > 2.5 times elapsed time
## caused by using C++ code and/or data.table
## Create household level dataset
x_hh <- selectHouseholdData(dat=x, hhId="ori_hid",
  hhVars=c("urbrur", "roof",  "walls", "water", "electcon", "household_weights"))
## Anonymize household level dataset and extract data
sdc_hh <- createSdcObj(x_hh, keyVars=c('urbrur','roof'), w='household_weights')
sdc_hh <- kAnon(sdc_hh, k = 3)
x_hh_anon <- extractManipData(sdc_hh)
 
## Merge anonymized household level data back into the full dataset
x_anonhh <- mergeHouseholdData(x, "ori_hid", x_hh_anon)
 
## Anonymize full dataset and extract data
sdc_full <- createSdcObj(x_anonhh, keyVars=c('sex', 'age', 'urbrur', 'roof'), w='sampling_weight')
sdc_full <- kAnon(sdc_full, k = 3)
x_full_anon <- extractManipData(sdc_full)
}
}
\author{
Thijs Benschop and Bernhard Meindl
}
