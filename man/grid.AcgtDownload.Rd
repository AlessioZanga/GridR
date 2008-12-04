\name{grid.AcgtDownload}
\alias{grid.AcgtDownload}
\title{grid.AcgtDownload}
\description{
  downloads the file with ID "fileID" from the ACGT DMS to the file "localName"
}
\usage{
grid.AcgtDownload(fileID,localName)
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