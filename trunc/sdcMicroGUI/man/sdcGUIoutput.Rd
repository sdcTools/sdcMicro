\name{sdcGUIoutput}
\alias{sdcGUIoutput}
\title{Retrieve the current dataset (with changes) from the sdcGUI}
\description{
After altering your data set inside the graphical user interface, you can retrieve the data set with 'sdcGUIoutput()' to use it further in R.
}
\usage{sdcGUI()}
\details{
Only possible if the GUI has been already used with a dataset.
}
\author{ Alexander Kowarik }
\examples{
\dontrun{
dataSDC <- sdcGUIoutput()
}
}
\keyword{IO}
