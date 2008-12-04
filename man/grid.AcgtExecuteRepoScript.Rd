\name{grid.AcgtExecuteRepoScript}
\alias{grid.AcgtExecuteRepoScript}
\title{grid.AcgtExecuteRepoScript}
\description{
  executes a specified ACGT workflow and returns the name and the id of a result file in the DMS
  
}
\usage{
grid.AcgtExecuteRepoScript(scriptID, fileIDs)
}
\arguments{
  \item{scriptID}{the id of the script to enact}
  \item{fileIDs}{a list of input file ids from the dms} 
  }

\details{
!!!Only for members of the ACGT Project with a valid MyProxy Credential!!!
ACGTLIBPATH must point to a folder with the GridR-ACGTLibs

To execute this function myProxyUsername and myProxyPwd must be set.
}

\examples{

##path to ACGT Libraries (ACGTLIBPATH) is needed to run the following:
#fileIDs=list()
#fileIDs[[1]]=1618
#library("GridR")
#grid.init(service="local")
#grid.AcgtExecuteRepoScript("urn:eu-acgt.org:service:columnappender", fileIDs)
}
\author{ Malte Lohmeyer }
\seealso{
\code{\link{grid.AcgtListRepo}}
\code{\link{grid.AcgtUpload}}
\code{\link{grid.AcgtDownload}}
}
\keyword{ programming }