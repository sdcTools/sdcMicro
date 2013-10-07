\name{pram}
\docType{methods}
\alias{pram-methods}
\alias{pram,ANY-method}
\alias{pram,sdcMicroObj-method}
\alias{pram}
\title{ Post RAndomisation Method (PRAM) }
\description{
PRAM is a probabilistic, perturbative method which can be applied on categorical variables.
}
\usage{
pram(obj, pramVar=NULL,pd=0.8, alpha=0.5)
}
\section{Methods}{
\describe{

\item{\code{signature(obj = "ANY")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(obj = "sdcMicroObj")}}{
%%  ~~describe this method here~~
}
}}
\arguments{
  \item{obj}{ a numeric vector or factor, data.frame or object of class sdcMicroObj }
  \item{pramVar}{name of the pramVar that should be prammed}
  \item{pd}{ minimum diagonal entries for the generated transition matrix P. Either a vector of length 1 or a vector of length ( number of categories ). }
  \item{alpha}{ amount of perturbation for the invariant Pram method }
}
\details{
The method is implemented exactly as described in the citation in the references.
First a transition matrix is created in that way, that the diagonal entries of a matrix P
are random numbers between \sQuote{pd} and 1. The remaining entries of the matrix are generated such that the rowSums of 
the matrix is 1. Then a invariant transition matrix is generated.
}
\value{
a modified \dQuote{sdcMicroObj} object or the following list
  \item{x}{ original vector }
  \item{xpramed}{ the perturbed vector }
  \item{pd}{ randomly generated diagonal entry of the P (between original pd and 1) }
  \item{Rs}{ invariant transition matrix }
  \item{alpha}{ amount of perturbation for the invariant Pram method }
}
\references{ Shlomo, Natalie and de Waal, Ton (2006) Protection of Micro-data Subject to Edit Constraints Against Statistical Disclosure.
 Southampton, UK, Southampton Statistical Sciences Research Institute, 36pp. (S3RI Methodology Working Papers, M06/16) 
 }
\author{ Matthias Templ }
\examples{
set.seed(123)
x <- factor(sample(1:4, 250, replace=TRUE))
pr1 <- pram(x)
length(which(pr1$x == x))
x2 <- as.factor(sample(1:4, 250, replace=TRUE))
length(which(pram(x2)$x == x2))

data(free1)
free1 <- data.frame(free1)
free1[,"MARSTAT"] <- as.factor(free1[,"MARSTAT"])
marstatPramed <- pram(free1[,"MARSTAT"])  

## for objects of class sdcMicro:
data(testdata2)
sdc <- createSdcObj(testdata2,
  keyVars=c('urbrur','roof','walls','sex'), 
  pramVars=c('water','electcon','relat'),
  numVars=c('expend','income','savings'), weightVar='sampling_weight')
sdc <- pram(sdc, pramVar="water")
}
\keyword{ manip }
