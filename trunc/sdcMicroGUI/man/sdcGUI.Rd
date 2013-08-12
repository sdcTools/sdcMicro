\name{sdcGUI}
\alias{sdcGUI}
\docType{data}
\title{ GUI for the sdcMicro package}
\description{
This graphical user interface supports the main functions of sdcMicro.
}
\usage{sdcGUI()}
\details{
This GUI provides an extension to the package sdcMicro. The developed GUI makes sdcMicro
accessible to a wider range of users including ones not used to the R command line interface. 
The user can access all basic functions for microdata protection by using this
GUI. 

The graphical user interface of sdcMicro allows an interactive interaction between objects. 
Flexibility is provided by automatic displaying of the main results (e.g.,
the summary of the frequency counts and the estimated disclosure risk) which
are updated after a user interaction automatically. Additional flexibility is
provided by storing all the users operations with all parameters in a script
which can then be saved, modified and/or reloaded. Thus, full reproducibility
is provided also when using this GUI instead of the CLI version.

It is programmed based on the gWidgetsRGtk2 and RGtk2 package.
}
\author{ Thomas Petelin, Matthias Templ, Alexander Kowarik }
\examples{
##sdcGUI()
}
\keyword{IO}
