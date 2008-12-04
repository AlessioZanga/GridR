# 	GridR package
#	Copyright (C) 2008 Fraunhofer Institute Intelligent Analysis and Information Systems IAIS, Dennis Wegener (dennis.wegener@iais.fraunhofer.de), Malte Lohmeyer (malte.lohmeyer@iais.fraunhofer.de), Stefan Rueping (stefan.rueping@iais.fraunhofer.de)  name of author
#		
#	This program is free software; you can redistribute it and/or
#	modify it under the terms of the GNU General Public License Version 2
#	as published by the Free Software Foundation
#		
#	This program is distributed in the hope that it will be useful,
#	but WITHOUT ANY WARRANTY; without even the implied warranty of
#	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#	GNU General Public License for more details.
#		
#	You should have received a copy of the GNU General Public License
#	along with this program; if not, write to the Free Software
#	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

`grid.AcgtReadFromDms` <-
		function(fileID,localName){
	if(!.grid$acgt){
		cat("This function is only availible for members of the ACGT Project with the right libraries in ACGTLIBPATH and with a valid ACGT Myproxy Certificate\n")
		return(FALSE)
	}
	if(is.null(.grid$myProxyUsername))
	{
		cat("myProxyUsername is not specified in config file or grid.init\n")
		return(FALSE)
	}
	if(is.null(.grid$pwd))
	{
		cat("myProxyPwd is not specified in config file or grid.init\n")
		return(FALSE)
	}
	dataType=system(paste("java de.fhg.iais.kd.gridr.clients.GridRServiceACGTClient downloadFromDMSReturnType ", fileID, " ", localName, " ", .grid$myProxyUsername, " ", .grid$pwd, " ",.grid$myProxyHost, " ", .grid$credentialName, " ", .grid$myproxyPort, sep="") ,intern=TRUE)
	if(length(dataType)>1){
		
		print(paste("Error:\n", dataType))
	}
	else if(dataType=="urn:eu-acgt.org:datatype:csv")
	{
		return(read.csv(localName))
	}
	else if(dataType=="urn:eu-acgt.org:datatype:zipofcel"){
		return(system(paste("unzip -o ", localName)))
	}
	else if(dataType=="urn:eu-acgt.org:datatype:tgzofcel"){
		return(system(paste("tar xzf ", localName)))
	}
	else {
		print(paste("Datatype", dataType, "not supported.\n Content of file:"))
		return(scan(file=localName, what=character(0), sep="\n", quiet=TRUE ))
	}
			
}