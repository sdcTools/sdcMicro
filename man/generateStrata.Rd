\name{generateStrata}
\alias{generateStrata}
\title{
Generate one strata variable from multiple factors
}
\description{
For strata defined by multiple variables (e.g. sex,age,country) one combined variable is generated.
}
\usage{
generateStrata(df, stratavars, name)
}
\arguments{
  \item{df}{
  a data.frame
}
  \item{stratavars}{
character vector with variable name
}
  \item{name}{
name of the newly generated variable
}
}
\value{
The original data set with one new column.
}
\author{
Alexander Kowarik
}

\examples{
x <- testdata
x <- generateStrata(x,c("sex","urbrur"),"strataIDvar")
head(x)
}
