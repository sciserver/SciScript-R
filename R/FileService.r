#require(httr)
#require(jsonlite)

#source('Authentication.r')

#--------------------------------------------------------

FileService.createDir <- function(path){

  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if !startsWith(path,"/"){
      path = paste("/", path, sep="")
    }
      
    url = paste(Config.FileServiceURL,"/data", path, "?dir=true", sep="")
    r = PUT(url,add_headers('X-Auth-Token'=token))

    if(r$status_code >= 200 && r$status_code < 300) {
      return(TRUE)
    } else {
      stop(paste("Error when creating new directory ", path, ".\nHttp Response from FileService API returned status code " + toString(res$status_code), ":\n" + content(r, as="text", encoding="UTF-8"), sep=""))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

FileService.getDirList <- function(path, options="X", level=1){

  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    if !startsWith(path,"/"){
      path = paste("/", path, sep="")
    }
    url = paste(Config.FileServiceURL,"/tree", path, "?options=", toString(options), "&level=", toString(level), sep="")
    r = PUT(url,add_headers('X-Auth-Token'=token))
    
    if(r$status_code >= 200 && r$status_code < 300) {
      return(content(r), sep=""))
    } else {
      stop(paste("Error when getting the contents of directory  ", path, ".\nHttp Response from FileService API returned status code " + toString(res$status_code), ":\n" + content(r, as="text", encoding="UTF-8"), sep=""))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

FileService.delete <- function(path){
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    if !startsWith(path,"/"){
      path = paste("/", path, sep="")
    }

    url = paste(Config.FileServiceURL,"/data", path, "?dir=true", sep="")
    r = DELETE(url,add_headers('X-Auth-Token'=token))
    
    if(r$status_code >= 200 && r$status_code < 300) {
      return(content(r), sep=""))
    } else {
      stop(paste("Error when deleting ", path, ".\nHttp Response from FileService API returned status code " + toString(res$status_code), ":\n" + content(r, as="text", encoding="UTF-8"), sep=""))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}
  
  
FileService.upload <- function(path, data=NULL, localFilePath=NULL){
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    if !startsWith(path,"/"){
      path = paste("/", path, sep="")
    }
    
    fileName = strsplit(path,"/")[-1]
    dirPath = gsub(fileName, "", path)

    createDir(dirPath)

    url = paste(Config.FileServiceURL,"/data", path, sep="")
    
    if( !is.null(localFilePath) && localFilePath != ""){
      r = PUT(url, body=upload_file(localFilePath), add_headers('X-Auth-Token'=token))
    }else{
      if(data != NULL){
        r = PUT(url, body=data, add_headers('X-Auth-Token'=token))
      }else{
        stop(paste("Error when uploading to " + path + ". No local file or data specified for uploading.",sep=""))
      }
    }

    if(r$status_code == 200) {
      return(TRUE)
    } else {
      stop(paste("Error when uploading to ", path, ".\nHttp Response from FileService API returned status code " + toString(res$status_code),":\n", content(r, as="text", encoding="UTF-8"), sep=""))      
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

  
FileService.download<-function(path, format="text", localFilePath=NULL){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if !startsWith(path,"/"){
      path = paste("/", path, sep="")
    }
    
    url = paste(Config.FileServiceURL,"/data", path, sep="")
    r = GET(url,add_headers('X-Auth-Token'=token))
    
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
            stop(paste("Unknown format ", format, " when trying to download from remote File System the file ", toString(path), ".\n", sep=""))
          }
        }else{
          stop(paste("Wrong format parameter value\n"))
        }
      }
    }else{
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}  