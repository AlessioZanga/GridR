\name{grid.AcgtUploadToDms}
\alias{grid.AcgtUploadToDms}
\title{grid.AcgtUploadToDms}
\description{
  uploads the specified file with dataType to the ACGT DMS and returns the fileID
}
\usage{
grid.AcgtUploadToDms(file, dataType=NULL)
}
\arguments{
  \item{file}{the path of the file to upload}
  \item{dataType}{the datatype of the file ie. urn:eu-acgt.org:datatype:csv} 
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