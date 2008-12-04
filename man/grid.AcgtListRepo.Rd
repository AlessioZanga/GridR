\name{grid.AcgtListRepo}
\alias{grid.AcgtListRepo}
\title{grid.AcgtListRepo}
\description{
  returns a list of availible workflows in the ACGT Repository
  
}
\usage{
grid.AcgtListRepo()
}
\details{
!!!Only for members of the ACGT Project with a valid MyProxy Credential!!!
ACGTLIBPATH must point to a folder with the GridR-ACGTLibs

A list of availible workflowIDs and their input parameter is returned.
ie. urn:eu-acgt.org:service:columnappender needs for example one input file with the name "input\_csv"
}
\examples{
library("GridR")
grid.init(service="local")
grid.AcgtListRepo()
}
\author{ Malte Lohmeyer }
\seealso{
\code{\link{grid.AcgtEnactRepo}}
\code{\link{grid.AcgtUpload}}
\code{\link{grid.AcgtDownload}}
}
\keyword{ programming }