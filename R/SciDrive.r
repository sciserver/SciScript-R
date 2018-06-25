#require( jsonlite)
#require(httr)

# assumes Config is loaded

SciDrive.createContainer<-function(path){
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    containerBody = paste('<vos:node xmlns:xsi="http://www.w3.org/2001/thisSchema-instance" ',
                          'xsi:type="vos:ContainerNode" xmlns:vos="http://www.ivoa.net/xml/VOSpace/v2.0" ',
                          'uri="vos://',Config.SciDriveHost,'!vospace/',path,'">',
                          '<vos:properties/><vos:accepts/><vos:provides/><vos:capabilities/>',
                          '</vos:node>',sep="")
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SciDrive.createContainer"
    }else{
      taskName = "SciScript-R.SciDrive.createContainer"
    }
    
    url = paste(Config.SciDriveHost,'/vospace-2.0/nodes/',path,"?TaskName=",taskName,sep="")
    r = PUT(url, content_type_xml(),body=containerBody,add_headers('X-Auth-Token'=token))
    if(r$status_code < 200 || r$status_code >=300) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(TRUE)
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}


SciDrive.upload<-function(path, data="", localFilePath=""){
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SciDrive.upload"
    }else{
      taskName = "SciScript-R.SciDrive.upload"
    }
    
    url = paste(Config.SciDriveHost,'/vospace-2.0/1/files_put/dropbox/',path,"?TaskName=",taskName,sep='')
    if(localFilePath != ""){
      r = PUT(url, body=upload_file(localFilePath), add_headers('X-Auth-Token'=token))
    }else{
      r = PUT(url, body=data, add_headers('X-Auth-Token'=token))
    }
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return (content(r))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

#------------------------------------
#    retrieve public URL for file identified by path
SciDrive.publicUrl<-function(path){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SciDrive.publicUrl"
    }else{
      taskName = "SciScript-R.SciDrive.publicUrl"
    }
    
    url = paste(Config.SciDriveHost,"/vospace-2.0/1/media/sandbox/", path,"?TaskName=",taskName,sep="")
    r = GET(url ,add_headers('X-Auth-Token'=token))
    if(r$status_code == 200){
      return(content(r)$url)
    }else{
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SciDrive.directoryList<-function(path){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SciDrive.directoryList"
    }else{
      taskName = "SciScript-R.SciDrive.directoryList"
    }
    
    url = paste(Config.SciDriveHost,"/vospace-2.0/1/metadata/sandbox/", URLencode(path), "?list=True&path=", URLencode(path),"?TaskName=",taskName, sep="")
    r = GET(url ,add_headers('X-Auth-Token'=token))
    if(r$status_code == 200){
      return(content(r))
    }else{
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SciDrive.download<-function(path, format="text", localFilePath=NULL){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SciDrive.download"
    }else{
      taskName = "SciScript-R.SciDrive.download"
    }
    
    fileUrl = SciDrive.publicUrl(path)
    
    r = GET(paste(fileUrl,"?TaskName=",taskName,sep="")
    if(r$status_code == 200){
      if(!is.null(localFilePath) && localFilePath != ""){
        bytes = content(r, "raw");
        theFile = file(localFilePath, "wb")
        writeBin(bytes, theFile)
        close(theFile)
        return(TRUE)
      }else{
        if(!is.null(format) && format != ""){
          if(format == "text"){
            return(content(r, "text", encoding="UTF-8"))
          }else if(format == "raw"){
            return(content(r, "raw"))
          }else if(format == "response"){
            return(r)
          }else{
            stop(paste("Illegal format parameter specification: ", format))
          }
        }else{
          stop(paste("Wrong format parameter value"))
        }
      }
    }else{
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SciDrive.delete<-function(path){
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    containerBody = paste('<vos:node xmlns:xsi="http://www.w3.org/2001/thisSchema-instance" ',
                          'xsi:type="vos:ContainerNode" xmlns:vos="http://www.ivoa.net/xml/VOSpace/v2.0" ',
                          'uri="vos://',Config.SciDriveHost,'!vospace/',path,'">',
                          '<vos:properties/><vos:accepts/><vos:provides/><vos:capabilities/>',
                          '</vos:node>',sep="")
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SciDrive.delete"
    }else{
      taskName = "SciScript-R.SciDrive.delete"
    }
    
    url = paste(Config.SciDriveHost,'/vospace-2.0/nodes/',path,"?TaskName=",taskName,sep="")
    r = DELETE(url, content_type_xml(),body=containerBody,add_headers('X-Auth-Token'=token))
    if(r$status_code < 200 || r$status_code >=300) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(TRUE)
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}