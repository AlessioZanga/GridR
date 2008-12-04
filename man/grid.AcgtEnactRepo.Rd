\name{grid.AcgtEnactRepo}
\alias{grid.AcgtEnactRepo}
\title{grid.AcgtEnactRepo}
\description{
  enacts a specified ACGT workflow and returns the name and the id of a result file in the DMS
  
}
\usage{
grid.AcgtEnactRepo(wfid, param)
}
\arguments{
  \item{wfid}{the id of the workflow to enact}
  \item{param}{a list of parameter name and input file id} 
  }

\details{
!!!Only for members of the ACGT Project with a valid MyProxy Credential!!!
ACGTLIBPATH must point to a folder with the GridR-ACGTLibs

To execute this function myProxyUsername and myProxyPwd must be set.

A list of availible workflowIDs and their input parameter is returned by the function \link{grid.AcgtListRepo}.
ie. "urn:eu-acgt.org:service:columnappender" needs for example one input file with the name "input\_csv"
Thus param is a list with one entry which contains a vector [input\_csv, fileID]. fileID is an ID of a file in the DMS.
}
\examples{
input=list()
param1=vector()
param1[1]="input_csv"
param1[2]=7276
input[[1]]=param1

library("GridR")
grid.init(service="local")
grid.AcgtEnactRepo("urn:eu-acgt.org:service:columnappender", input)
}
\author{ Malte Lohmeyer }
\seealso{
\code{\link{grid.AcgtListRepo}}
\code{\link{grid.AcgtUpload}}
\code{\link{grid.AcgtDownload}}
}
\keyword{ programming }