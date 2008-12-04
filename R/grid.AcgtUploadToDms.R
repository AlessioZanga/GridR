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

`grid.AcgtUploadToDms` <-
		function(file, dataType=NULL){
	
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
	if(is.null(dataType)){
		ret=try(system(paste("java de.fhg.iais.kd.gridr.clients.GridRServiceACGTClient uploadToDMSWithType ", file, " ", .grid$myProxyUsername, " ", .grid$pwd, " ", dataType," ", .grid$myProxyHost, " ", .grid$credentialName, " ", .grid$myproxyPort, sep=""), intern=TRUE))
		if(length(ret)>1)
			print(ret)
		else
			return(as.numeric(ret))
	}
	else{
		ret=try(system(paste("java de.fhg.iais.kd.gridr.clients.GridRServiceACGTClient uploadToDMS ", file, " ", .grid$myProxyUsername, " ", .grid$pwd," ", .grid$myProxyHost, " ", .grid$credentialName, " ", .grid$myproxyPort, sep=""), intern=TRUE))
		if(inherits(ret, "try-error")){
			cat("Error, cannot execute GridRServiceACGTClient\n")
		}
		warn=getOption("warn")
		options(warn=-1)
		retNum=as.numeric(ret)
		options(warn=warn)
		if(any(is.na(retNum)))
			warning(ret)
		else
			return(retNum)
	}
}