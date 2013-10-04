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
report(obj, outdir=getwd(),filename="SDC-Report",format="HTML",title='SDC-Report',
  internal=FALSE)
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
   	HTML, TEXT or LATEX
  }
  \item{title}{
   	Title for the report
  }
  \item{internal}{TRUE/FALSE, if TRUE a detailled internal report is produced,
    else a non-disclosive overview}
}

\details{
The application of this function provides you with a html, text or pdf-report for your sdcMicro object that contains useful summaries about the anonymization process. 
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
