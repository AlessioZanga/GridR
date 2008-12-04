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

`grid.init` <-
function(confFile=NULL, localTmpDir=NULL, verbose=TRUE, sshRemoteIp=NULL, sshUsername=NULL, sshRemoteDir=NULL, myProxyHost=NULL, myProxyUsername=NULL, credentialName=NULL, myProxyPwd=NULL, myProxyPort=NULL, service=NULL, acgtUrl=NULL, acgtHost=NULL, acgtLibPath=NULL, acgtDn=NULL, sshKey=NULL, debug=FALSE, sharedDir=NULL, remoteRPath=NULL){
#load config file  
	if(!exists(".grid", inherits=TRUE)){
		cat("Error,no .grid var!")
		return(FALSE)
	}
	
	env=loadNamespace("GridR")
	unlockBinding(".grid", env)

	if(debug)
		.grid$debug=TRUE
	else
		.grid$debug=FALSE
	
	.grid$javaClientPath=NULL
	for(i in 1:length(.libPaths())){
		if(file.access(paste(.libPaths()[i],"/GridR/GridR/",sep=""))==0)
			.grid$javaClientPath=paste(.libPaths()[i],"/GridR/GridR/",sep="")
	}
	if(is.null(.grid$javaClientPath))
		cat("Error: cannot find the path where GridR is installed, thus all functions which need java code will not work\n")
	
	

	configPath=""
	if(!is.null(confFile) && file.access(confFile)==0){	
			configPath=	confFile
		}
	else
	{
		if(file.access("~/gridr.conf")==0) 
			configPath=	"~/gridr.conf"
		if(file.access("gridr.conf")==0) 
			configPath=	"gridr.conf"
	}
	if(configPath=="")
	{
		if(.grid$debug)
			cat("GridR is not using a config file\n")
	}
	else {
		if(.grid$debug)
			cat(paste("using config file: ",configPath, "\n", sep=""))
		
		#open config file
		config=scan(configPath, what=character(0), quiet=TRUE, sep="\n")
		if(length(config)<2)
		{
			print(paste("wrong config file ( #lines <2)", configPath))
			return(FALSE)
		}
		i=1
		while( i < length(config)){
			#scan
			
			#found a comment?
			if(substr(config[i],1,1)=="#"){
				i=i+1
				next
			}
			start=gregexpr("<", config[i])[[1]] #find all < 
			end=gregexpr(">", config[i])[[1]]   # find all >
			if(i==1) {
				if(gregexpr("<?xml",config[i])[[1]][1]!=-1){
					if(gregexpr("<GRIDR>",config[i+1])[[1]]==-1){
						cat("wrong config file( second line != <GRIDR>\n")
						return(FALSE)
					}
					else{
						i=i+2
						next
					}
				}
				else if(gregexpr("<GRIDR>",config[i])[[1]]==-1){
					cat("wrong config file( first line != <GRIDR>\n")
					return(FALSE)
				}
				else{
					i=i+1
					next
				}
			} else if(length(start)!=length(end) ||length(end)!=2) {#check if number of start and end tags are differen or if there are more than 2 tags per line
				if(length(end)==1 && gregexpr("</GRIDR>",config[i])[[1]]!=-1)
					break
				else
					cat("Syntax error in config file in line ", i, " wrong < and >\n")
			}
			#look if start and end tag are the same
			tag=substr(config[i],start[1]+1, end[1]-1) 
			str=substr(config[i],start[2]+2, end[2]-1)
			if(tag!=str) {
				cat("Syntax error in confog file in line ", i, " start and end tag are different\n")
				return(FALSE)
			}
			
			value=substr(config[i],end[1]+1, start[2]-1)
			#delete " " at the beginning
			if(nchar(value)>1 && substr(value,1,1)==" ")
				value=substr(value,2,nchar(value))
			#delete " " at the end
			if(nchar(value)>1 && substr(value,nchar(value),nchar(value))==" ")
				value=substr(value,nchar(value)-1,nchar(value)-1)
			
			if(.grid$debug)
				cat(paste("found Config File entry : ",tag, " with value ", value, "\n", sep=""))
			
		#	save readed values from config file to the right variable
			if(tag=="LOCALTMPDIR")
				.grid$localDir=gsub("\\\\","/",value)
#			else if(tag=="GLOBUSWSURL")
#				.grid$globusUrl=value 
#			else if(tag=="CONDORWSURL")
#				.grid$condorUrl=value
			else if(tag=="SSHREMOTEIP")
				.grid$ssh$ip=value
			else if(tag=="SSHUSERNAME")
				.grid$ssh$username=value
			else if(tag=="MYPROXYUSERNAME")
				.grid$myProxyUsername=value
			else if(tag=="MYPROXYPWD")
				.grid$pwd=value
			else if(tag=="MYPROXYPORT")
				.grid$myproxyPort=value
			else if(tag=="CREDENTIALNAME")
				.grid$credentialName=value
			else if(tag=="SSHREMOTEDIR")
				.grid$ssh$remotePath=value
			else if(tag=="JAVACLIENTPATH")
				.grid$javaClientPath=value
			else if(tag=="GLOBUSEXECUTIONHOST")
				.grid$globusHost=value
			else if(tag=="MYPROXYHOST")
				.grid$myProxyHost=value
			else if(tag=="SERVICE")
				.grid$service=value
			else if(tag=="COGREMOTEDIR")
				.grid$cogDir=value
			else if(tag=="ACGTWSURL")
				.grid$acgtUrl=value
			else if(tag=="ACGTEXECUTIONHOST")
				.grid$acgtHost=value
			else if(tag=="SSHKEY")
				.grid$ssh$key=value
			else if(tag=="ACGTLIBPATH")
				.grid$acgtLibPath=value
			else if(tag=="ACGTDN")
				.grid$acgtDn=value
			else if(tag=="REMOTERPATH")
				.grid$remoteRPath=value
			else if(tag=="NFSDIR")
				.grid$nfs$dir=gsub("\\\\","/",value)
			else if(tag=="SHAREDDIR")
				.grid$nfs$dir=gsub("\\\\","/",value)
			
			i=i+1
		}
	}
	#save command line parameters to the right variable
  if(!is.null(service))
	  .grid$service<- service
  if(!is.null(localTmpDir))
	  .grid$localDir <- localTmpDir
  if(!is.null(sshRemoteIp))
	  .grid$ssh$ip=sshRemoteIp
#  if(!is.null(condorUrl))
#	  .grid$condorUrl=condorUrl
 # if(!is.null(globusUrl))
#	  .grid$globusUrl=globusUrl
  if(!is.null(sshRemoteDir))
	  .grid$ssh$remotePath=sshRemoteDir
#  if(!is.null(globusExecutionHost))
#	  .grid$globusHost=globusExecutionHost
  if(!is.null(sshUsername))
	  .grid$ssh$username=sshUsername
  if(!is.null(myProxyHost))
	  .grid$myProxyHost=myProxyHost
  if(!is.null(myProxyUsername))
	  .grid$myProxyUsername=myProxyUsername
  if(!is.null(myProxyPwd))
	  .grid$pwd=myProxyPwd
  if(!is.null(myProxyPort))
  	.grid$myproxyPort=myProxyPort
  if(!is.null(credentialName))
	  .grid$credentialName=credentialName
#  if(!is.null(cogRemoteDir))
#	  .grid$cogDir=cogRemoteDir
  if(!is.null(acgtLibPath))
	  .grid$acgtLibPath=acgtLibPath
  if(!is.null(acgtHost))
	  .grid$acgtHost=acgtHost
  if(!is.null(acgtUrl))
	  .grid$acgtUrl=acgtDn
  if(!is.null(acgtUrl))
	  .grid$acgtDn=acgtDn
  if(!is.null(sshKey))
  	  .grid$ssh$key=sshKey
  if(!is.null(remoteRPath))
	  .grid$remoteRPath=remoteRPath
  if(!is.null(sharedDir))
	  .grid$nfs$dir=sharedDir
 
  
	#check if all needed parameters are entered
	if(is.null(.grid$service)){
		cat("service is not specified. Check Config File and Parameters\n wrong service, availible services are: local, condor.ssh, remote.ssh, globus.cog (and acgt.ws) and variableSharing\n")
		return(FALSE)
	}
	if(!(.grid$service=="condor.ssh" ||.grid$service=="local" || .grid$service =="remote.ssh" || .grid$service=="acgt" || .grid$service=="globus.cog" ||.grid$service=="variableSharing"))
	{
		cat("wrong service, availible services are: local, condor.ssh, remote.ssh, globus.cog (and acgt) and variableSharing\n")
		return(FALSE)
	}
	err=FALSE
  if(is.null(.grid$localDir) && .grid$service!="variableSharing" && .grid$service!="acgt"){
	  cat("localTmpDir is not specified. Check config file and parameters.\n")
	  err=TRUE
	  }
  if(is.null(.grid$nfs$dir) ){
	  if(.grid$debug)
	  	cat("GridR variableSharing will be disabled, because sharedDir is not specified. \n")
	  .grid$nfs$run=FALSE
  }
  else
	  .grid$nfs$run=TRUE

  if (is.null(.grid$ssh$ip) && (.grid$service=="condor.ssh" ||.grid$service=="remote.ssh" )) {
	  cat("sshRemoteIp is not specified. Check Config File and Parameters\n")
	  err=TRUE
	  }

#  if(is.null(.grid$condorUrl) && .grid$service=="condor.ws" ) {
#	  cat("condorUrl is not specified. Check Config File and Parameters\n")
#	  err=TRUE
#	  }
	  
#  if(is.null(.grid$globusUrl) && ( .grid$service=="acgt.ws") ) {
#	  cat("globusUrl is not specified. Check Config File and Parameters\n")
#	  err=TRUE
#  }
	
 # if(is.null(.grid$acgtUrl) && .grid$service=="acgt.ws") {
#	  cat("acgtUrl is not specified. Check Config File and Parameters\n")
#	  err=TRUE
 # }
  if(is.null(.grid$acgtLibPath) && .grid$service=="acgt") {
	  cat("acgtLibPath is not specified. Check Config File and Parameters\n")
	  err=TRUE
  }  
  if(is.null(.grid$acgtDn) && .grid$service=="acgt") {
	  cat("acgtLibPath is not specified. Check Config File and Parameters\n")
	  err=TRUE
  }
  if (is.null(.grid$ssh$remotePath) && (.grid$service=="condor.ssh" ||.grid$service=="remote.ssh" )) {
	  cat("sshRemoteDir is not specified. Check Config File and Parameters\n")
	  err=TRUE
	  }
	if(is.null(.grid$myProxyUsername) && ( .grid$service=="acgt") ) {
		cat("myProxyUsername is not specified. Check Config File and Parameters\n")
		err=TRUE
		}
	if(is.null(.grid$myProxyHost) && .grid$service=="acgt" ) {
		cat("myProxyHost is not specified. Check Config File and Parameters\n")
		err=TRUE
	}
	if(is.null(.grid$globusHost) && ( .grid$service=="globus.cog" ) ) {
		cat("globusExecutionHost is not specified. Check Config File and Parameters\n")
		err=TRUE
	}
	if(is.null(.grid$ssh$username) && (.grid$service=="condor.ssh" ||.grid$service=="remote.ssh" )){
		cat("sshUsername is not specified. Check Config File and Parameters\n")
		err=TRUE
	}
	if(is.null(.grid$pwd) && ( .grid$service=="acgt") ) {
	  cat("myProxyPwd is not specified. Check Config File and Parameters\n")
	  err=TRUE
	}
	if(is.null(.grid$cogDir) && .grid$service=="globus.cog") {
		cat("cogRemoteDir is not specified. Check Config File and Parameters\n")
		err=TRUE
	}
	if (is.null(.grid$ssh$key) && Sys.info()["sysname"]=="Windows" && (.grid$service=="condor.ssh" ||.grid$service=="remote.ssh" )) {
		cat("sshKey is not specified. Check Config File and Parameters\n")
		err=TRUE
	}
	if(.grid$service=="condor.ssh" && is.null(.grid$remoteRPath))
		.grid$remoteRPath="/usr/bin/R"

  if(err)
	  return(FALSE)
  
  #add / to the end of dirs if not exists
  if(!is.null(.grid$localDir) && substring(.grid$localDir,nchar(.grid$localDir)) != "/"){ .grid$localDir <- paste(.grid$localDir,"/",sep="") }
  if(!is.null(.grid$nfs$dir) && substring(.grid$nfs$dir,nchar(.grid$nfs$dir)) != "/"){ .grid$nfs$dir <- paste(.grid$nfs$dir,"/",sep="") }
  if(!is.null(.grid$cogDir) && substring(.grid$cogDir,nchar(.grid$cogDir)) != "/"){ .grid$cogDir <- paste(.grid$cogDir,"/",sep="") }
  if(!is.null(.grid$acgtLibPath) && substring(.grid$acgtLibPath,nchar(.grid$acgtLibPath)) != "/"){ .grid$acgtLibPath <- paste(.grid$acgtLibPath,"/",sep="") }
  if(.grid$service=="condor.ssh" || .grid$service=="remote.ssh") {
      if(substring(.grid$ssh$remotePath,nchar(.grid$ssh$remotePath)) != "/"){ .grid$ssh$remotePath <- paste(.grid$ssh$remotePath,"/",sep="") }
  }
  
  #check if these dirs exists
	if(!is.null(.grid$localDir) && is.na(file.info(substr(.grid$localDir,1, nchar(.grid$localDir)-1))$"isdir"))
		dir.create(.grid$localDir)
	
	nfsDirExists=file.info(substr(.grid$nfs$dir,1, nchar(.grid$nfs$dir)-1))$"isdir"
	if(.grid$nfs$run && is.na(nfsDirExists))
		dir.create(.grid$nfs$dir)

#initialize .grid
.grid$uniqueName <- paste("grid",Sys.info()["nodename"],Sys.getpid(),gsub(" |:","-",perl=TRUE,as.character(Sys.time())),.grid$count,sep="-")
.grid$callback <- addTaskCallback(grid.callback)
if(.grid$nfs$run)
	.grid$shareCallback <- addTaskCallback(grid.shareCallback)

#start saving the history 
#grid.historyTaskCallback <- function(...){
#	.gridhistory = paste(.gridhistory, paste(list(...)[[1]], collapse=" "), "\n", sep="")
#	print(.gridhistory)
#	assign(".gridhistory", .gridhistory, loadNamespace("GridR"))
#	return(TRUE)
#}
#unlockBinding(".gridhistory", env)
#.grid$historyCallback = addTaskCallback(grid.historyTaskCallback)

.grid$verbose <- verbose

  #init classpath separator for different operating systems
  if(Sys.info()["sysname"]=="Linux"){
  		.grid$classSeparator=":"
		.grid$system="linux"
	}
	else if(Sys.info()["sysname"]=="Darwin"){
		.grid$classSeparator=":"
		.grid$system="linux"
	}
   else if(Sys.info()["sysname"]=="Windows"){
	   .grid$classSeparator=";"
	   .grid$system="windows"
   }
   else{
	   cat(paste("wrong Operating System, availible are: \"Windows\", \"Linux\" and \"Darvin\"\n your Operating system is: ",Sys.info()["sysname"] ))
	   return(FALSE)
   }
   #set CLASSPATH environment
	if(!is.null(.grid$acgtLibPath)){
		#scan .grid$acgtLibPath directory and add all libs to classpathpath var
		files=dir(.grid$acgtLibPath, pattern=".jar$", recursive=TRUE)
		if(length(files)>0)
		{	
			#add all files if not exists
			if(gregexpr(files[1],Sys.getenv("CLASSPATH"))[[1]][1]==-1){
				Sys.setenv(CLASSPATH=paste(Sys.getenv("CLASSPATH"),.grid$javaClientPath,"acgtClient/",.grid$classSeparator, .grid$acgtLibPath, files[1],sep=""))
				for (i in 2:length(files)){
					#activate ACGT mode
					if(gregexpr("EnactClientAPI.jar",files[i])[[1]][1]!=-1)
						.grid$acgt=TRUE
					Sys.setenv(CLASSPATH=paste(Sys.getenv("CLASSPATH"),.grid$classSeparator,.grid$acgtLibPath, files[i],sep=""))
				}
				#add dms_services & grms_services
				Sys.setenv(CLASSPATH=paste(Sys.getenv("CLASSPATH"),.grid$classSeparator,.grid$javaClientPath,"acgtClient/dms_services.jar",.grid$classSeparator,.grid$javaClientPath,"acgtClient/grms_services.jar" ,sep=""))
			}
		}
		else
		{
			cat("no jars found in ACGTLIBPATH")
		}
	}
	
	# add all jars in the GridR inst Path
	files=dir(.grid$javaClientPath, pattern=".jar$", recursive=TRUE)
	if(length(files)>0)
	{
		for (i in 1:length(files)){
			Sys.setenv(CLASSPATH=paste(Sys.getenv("CLASSPATH"),.grid$classSeparator,.grid$javaClientPath, files[i],sep=""))
		}
	}
  #assign(".grid",.grid,.GlobalEnv)
	assign(".grid",.grid, env)
	
	
}

