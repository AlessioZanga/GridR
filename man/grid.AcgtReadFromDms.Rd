\name{grid.AcgtReadFromDms}
\alias{grid.AcgtReadFromDms}
\title{grid.AcgtReadFromDms}
\description{
  downloads the file with ID "fileID" from the ACGT DMS to the file "localName" and returns it
}
\usage{
grid.AcgtReadFromDms(fileID,localName)
}
\arguments{
  \item{fileID}{the id of the file to download}
  \item{localName}{local path where to save the file} 
  }
 \details{
 !!!Only for members of the ACGT Project with a valid MyProxy Credential!!!
 ACGTLIBPATH must point to a folder with the GridR-ACGTLibs
 }
\author{ Malte Lohmeyer }
\seealso{
\code{\link{grid.AcgtUpload}}
\code{\link{grid.AcgtListRepo}}
\code{\link{grid.AcgtEnactRepo}}
}
\keyword{ programming }