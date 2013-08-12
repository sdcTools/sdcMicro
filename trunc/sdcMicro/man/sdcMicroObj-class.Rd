\name{sdcMicroObj-class}
\Rdversion{1.1}
\docType{class}
\alias{sdcMicroObj-class}
\alias{get.sdcMicroObj}
\alias{nextSdcObj}
\alias{set.sdcMicroObj}
\alias{undolast}
\alias{get.sdcMicroObj,sdcMicroObj,character-method}
\alias{set.sdcMicroObj,sdcMicroObj,character,listOrNULL-method}
\alias{undolast,sdcMicroObj-method}
\alias{nextSdcObj,sdcMicroObj-method}
\alias{createSdcObj}

\title{Class \code{"sdcMicroObj"}}
\description{
Class to save all information about the SDC process
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("sdcMicroObj", ...)}.
}
\usage{createSdcObj(dat, keyVars, numVars = NULL, weightVar = NULL, hhId = NULL, 
    strataVar = NULL, sensibleVar=NULL, options = NULL) 
    undolast(obj)
    nextSdcObj(obj)
    }
\arguments{
  \item{dat}{A numeric matrix or data frame containing the data.}
  \item{obj}{An object of class 'sdcMicroObj'}
  \item{keyVars}{sfdf}
  \item{numVars}{sfdf}
  \item{weightVar}{sfdf}
  \item{hhId}{sfdf}
  \item{strataVar}{sfdf}
  \item{sensibleVar}{sfdf}
  \item{options}{sfdf}
}
\section{Slots}{
  \describe{
    \item{\code{origData}:}{Object of class \code{"dataframeOrNULL"} ~~ }
    \item{\code{keyVars}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{numVars}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{weightVar}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{hhId}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{strataVar}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{sensibleVar}:}{Object of class \code{"numericOrNULL"} ~~ }
    \item{\code{manipKeyVars}:}{Object of class \code{"dataframeOrNULL"} ~~ }
    \item{\code{manipNumVars}:}{Object of class \code{"dataframeOrNULL"} ~~ }
    \item{\code{manipStrataVar}:}{Object of class \code{"factorOrNULL"} ~~ }
    \item{\code{originalRisk}:}{Object of class \code{"listOrNULL"} ~~ }
    \item{\code{risk}:}{Object of class \code{"listOrNULL"} ~~ }
    \item{\code{utility}:}{Object of class \code{"listOrNULL"} ~~ }
    \item{\code{pram}:}{Object of class \code{"listOrNULL"} ~~ }
    \item{\code{localSuppression}:}{Object of class \code{"listOrNULL"} ~~ }
    \item{\code{options}:}{Object of class \code{"listOrNULL"} ~~ }
    \item{\code{additionalResults}:}{Object of class \code{"listOrNULL"} ~~ }
    \item{\code{set}:}{Object of class \code{"listOrNULL"} ~~ }
    \item{\code{prev}:}{Object of class \code{"sdcmicroOrNULL"} ~~ }
  }
}
%\section{Extends}{
%Class \code{"\linkS4class{sdcmicroOrNULL}"}, directly.
%}
\section{Methods}{
  \describe{
    \item{get.sdcMicroObj}{\code{signature(object = "sdcMicroObj", type = "character")}: ... }
    \item{set.sdcMicroObj}{\code{signature(object = "sdcMicroObj", type = "character", input = "listOrNULL")}: ... }
    \item{undo}{\code{signature(object = "sdcMicroObj")}: ... }
    \item{nextSdcObj}{\code{signature(object = "sdcMicroObj")}: ... }
	 }
}
\author{
Bernhard Meindl, Elias Rut, Alexander Kowarik, Matthias Templ
}

\examples{
showClass("sdcMicroObj")
\dontrun{
data(testdata)
sdc <- createSdcObj(testdata,
  keyVars=c('urbrur','roof','walls','water','electcon','relat','sex'),
  numVars=c('expend','income','savings'), w='sampling_weight')
head(sdc@manipNumVars)
### Display Risks
sdc@risk$global
sdc <- dRisk(sdc)
sdc@risk$numeric
### use addNoise without Parameters
sdc <- addNoise(sdc,variables=c("expend","income"))
head(sdc@manipNumVars)
sdc@risk$numeric
### undolast
sdc <- undolast(sdc)
head(sdc@manipNumVars)
sdc@risk$numeric
### redo addNoise with Parameter
sdc <- addNoise(sdc, noise=0.2)
head(sdc@manipNumVars)
sdc@risk$numeric
### dataGen
#sdc <- undolast(sdc)
#head(sdc@risk$individual)
#sdc@risk$global
#sdc <- dataGen(sdc)
#head(sdc@risk$individual)
#sdc@risk$global
### LocalSuppression
sdc <- undolast(sdc)
head(sdc@risk$individual)
sdc@risk$global
sdc <- localSuppression(sdc)
head(sdc@risk$individual)
sdc@risk$global
### microaggregation
sdc <- undolast(sdc)
head(get.sdcMicroObj(sdc, type="manipNumVars"))
sdc <- microaggregation(sdc)
head(get.sdcMicroObj(sdc, type="manipNumVars"))
### pram
sdc <- undolast(sdc)
head(sdc@risk$individual)
sdc@risk$global
sdc <- pram(sdc,keyVar="water")
head(sdc@risk$individual)
sdc@risk$global
### pram_strata
sdc <- undolast(sdc)
sdc <- pram_strata(sdc,variables=c("walls","water"))
head(sdc@risk$individual)
sdc@risk$global
### rankSwap
sdc <- undolast(sdc)
head(sdc@risk$individual)
sdc@risk$global
head(get.sdcMicroObj(sdc, type="manipNumVars"))
sdc <- rankSwap(sdc)
head(get.sdcMicroObj(sdc, type="manipNumVars"))
head(sdc@risk$individual)
sdc@risk$global
### suda2
sdc <- suda2(sdc)
sdc@risk$suda2
### topBotCoding
head(get.sdcMicroObj(sdc, type="manipNumVars"))
sdc@risk$numeric
sdc <- topBotCoding(sdc, value=60000000, replacement=62000000, column="income")
head(get.sdcMicroObj(sdc, type="manipNumVars"))
sdc@risk$numeric
### LocalRecProg
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c("urbrur", "roof", "walls", "water", "sex", "relat"))
sdc@risk$global
sdc <- LocalRecProg(sdc)
sdc@risk$global
### LLmodGlobalRisk
sdc <- undolast(sdc)
sdc <- LLmodGlobalRisk(sdc, inclProb=0.001)
sdc@risk$model
}
}
\keyword{classes}
