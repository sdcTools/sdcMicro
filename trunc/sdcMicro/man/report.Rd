\name{report}
\docType{methods}
\alias{report-methods}
\alias{report}
\alias{report,sdcMicroObj-method}
%\alias{report,reportObj-method}
%\alias{reportObj}
%\alias{reportObj-class}
%\alias{calcReportData}
%\alias{calcReportData-methods}
%\alias{calcReportData,sdcMicroObj-method}
\title{Generate a HTML/LATEX output from an sdcMicroObj}
\description{
 Summary statistics of the original and the perturbed data set
}
\usage{
report(obj,
  outdir=getwd(),filename="SDC-Report",format="HTML",title='SDC-Report',
  pram=TRUE,kAnon=TRUE,indivRisk=TRUE,hierRisk=TRUE,riskNumKeyVars=TRUE,
  dataUtility=TRUE,localSupps=TRUE,dataUtilityCont=TRUE,sessionInfo=TRUE)
%calcReportData(obj, pram=TRUE,kAnon=TRUE,indivRisk=TRUE,
%  riskNumKeyVars=TRUE,dataUtility=TRUE,localSupps=TRUE,dataUtilityCont=TRUE,
%  sessionInfo=TRUE)
}
\arguments{
\item{obj}{
  	an object of class 'sdcMicroObj' or 'reportObj'
  }
  \item{outdir}{
	output folder
  }
  \item{filename}{
   	output filename
  }
  \item{format}{
   	HTML or LATEX
  }
  \item{title}{
   	Title for the report
  }
  \item{pram}{TRUE/FALSE if information about pram should be included in the report (if it exists).}
  \item{kAnon}{TRUE/FALSE if information about k-anonymity should be included in the report (if it exists).}
  \item{indivRisk}{TRUE/FALSE if information about individual risk should be included in the report (if it exists).}
  \item{hierRisk}{TRUE/FALSE if information about hierachical risk should be included in the report (if it exists).}
  \item{riskNumKeyVars}{TRUE/FALSE if information about numeric risk should be included in the report (if it exists).}
  \item{dataUtility}{TRUE/FALSE if information about categorical data utitlity should be included in the report (if it exists).}
  \item{localSupps}{TRUE/FALSE if information about local suppressions should be included in the report (if it exists).}
  \item{dataUtilityCont}{TRUE/FALSE if information about continuous data utility should be included in the report (if it exists).}
  \item{sessionInfo}{TRUE/FALSE if information about the session should be included in the report (if it exists).}
}

\details{
The application of this function provides you with a html or pdf-report for your sdcMicro object that contains useful summaries about the anonymization process. 
}

\author{
Matthias Templ, Bernhard Meindl
}
\examples{
\dontrun{
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'), 
  numVars=c('expend','income','savings'), w='sampling_weight')
report(sdc)
}
}
\keyword{methods}
