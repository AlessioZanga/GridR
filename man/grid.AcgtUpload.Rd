\name{grid.AcgtUpload}
\alias{grid.AcgtUpload}
\title{grid.AcgtUpload}
\description{
  uploads the specified file to the ACGT DMS and returns the fileID
}
\usage{
grid.AcgtUpload(file)
}
\arguments{
  \item{file}{the path of the file to upload}
  }
 \details{
 !!!Only for members of the ACGT Project with a valid MyProxy Credential!!!
 ACGTLIBPATH must point to a folder with the GridR-ACGTLibs
 }
\author{ Malte Lohmeyer }
\seealso{
\code{\link{grid.AcgtDownload}}
\code{\link{grid.AcgtListRepo}}
\code{\link{grid.AcgtEnactRepo}}
}
\keyword{ programming }