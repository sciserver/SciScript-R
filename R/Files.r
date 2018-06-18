#require(httr)
#require(jsonlite)

#source('Authentication.r')

#--------------------------------------------------------

Files.getFileServices <- function(verbose=TRUE){

  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {

    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Files.getFileServices"
    }else{
      taskName = "SciScript-R.Files.getFileServices"
    }

    url = paste(Config.RacmApiURL,"/storem/fileservices?TaskName=",taskName, sep="")
    r = GET(url,add_headers('X-Auth-Token'=token))

    if(r$status_code >= 200 && r$status_code < 300) {
      fileServices = list()
      fileServicesAPIs = content(r, encoding="UTF-8")
      for(i in 1:length(fileServicesAPIs)){
        url = fileServicesAPIs[[i]]$apiEndpoint
        name = fileServicesAPIs[[i]]$name
        url = paste(url,"api/volumes/?TaskName=",taskName,sep="")
        res = GET(url,add_headers('X-Auth-Token'=token))
        if(res$status_code >= 200 && res$status_code < 300) {
          fileServices[[i]] = content(res, encoding="UTF-8")
        }else{
          if(verbose){
            response_text = content(res, "text", encoding="UTF-8")
            warnings(paste("Error when getting definition of FileService named '",name,"'.\nHttp Response from FileService API returned status code ",res$status_code),". This FileService might be not available:\n",response_text)
          }
        }
        
      }
      return(fileServices)
      
    }else{
      response_text = content(r, "text", encoding="UTF-8")
      stop(paste("Error when getting the list of FileServices.\nHttp Response from FileService API returned status code ",r$status_code,":\n",response_text,sep=""));
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

Files.getFileServicesNames <- function(fileServices=NULL, verbose=TRUE){
  if(is.null(fileServices)){
    fileServices = Files.getFileServices(verbose);
  }
  
  fileServiceNames = list();
  for(i in 1:length(fileServices)){
      fileServiceNames[[i]] = list(name=fileServices[[i]]$name, description=fileServices[[i]]$description)
  }
  return(fileServiceNames)
}


Files.getFileServiceFromName <- function(fileServiceName, fileServices=NULL, verbose=TRUE){

  if(is.null(fileServices)){
    fileServices = Files.getFileServices(verbose);
  }
  
  if(length(fileServices) > 0 ){
    
    for(i in 1:length(fileServices)){
      if(fileServiceName == fileServices[[i]]$name){
        return(fileServices[[i]])
      }
    }
    
    if(verbose){
      warning(paste("FileService of name '", fileServiceName , "' is not available to the user or does not exist.",sep=""))
      return(NULL)
    }
  }else{
    if(verbose){
      warning(paste("There are no FileServices available for the user.",sep=""))
      return(NULL)
    }
  }
}




Files.__getFileServiceAPIUrl <- function(fileService){
  
  url = NULL
  if(typeof(fileService) == "character"){
    
    fileServices = Files.getFileServices(FALSE);
    fileService = Files.getFileServiceFromName(fileService, fileServices, verbose=FALSE);
    url = fileService$apiEndpoint

  }else{
    url = fileService$apiEndpoint
  }
  
  if(!endsWith(url, "/")){
    url = paste(url,"/",sep="")
  }
  
  return(url)
}



Files.getRootVolumesInfo <- function(fileService, verbose=TRUE){
  
  if(typeof(fileService) == "character"){
    fileService = Files.getFileServiceFromName(fileService, verbose=verbose);
  }
  
  rootVolumes = list()
  for(i in 1:length(fileService$rootVolumes)){
    rootVolumes[[i]] = list(rootVolumeName=  fileService$rootVolumes[[i]]$name, rootVolumeDescription=fileService$rootVolumes$description)
  }
  
  return(rootVolumes)
}




Files.getUserVolumesInfo <- function(fileService, rootVolumeName=NULL, verbose=TRUE){
  
  if(typeof(fileService) == "character"){
    fileService = Files.getFileServiceFromName(fileService, verbose=verbose);
  }
  
  userVolumes = list()
  k=1
  for(i in 1:length(fileService$rootVolumes)){
    for(j in 1:length(fileService$rootVolumes[[i]]$userVolumes )){
      path = paste(fileService$rootVolumes[[i]]$name,"/",fileService$rootVolumes[[i]]$userVolumes[[j]]$owner,"/",fileService$rootVolumes[[i]]$userVolumes[[j]]$name,"/",sep="")
      if(!is.null(rootVolumeName)){
        if(fileService$rootVolumes[[i]]$name == rootVolumeName){
          userVolumes[[k]] = list(userVolumeName= fileService$rootVolumes[[i]]$userVolumes[[j]]$name, path=path, userVolumeDescription=fileService$rootVolumes[[i]]$userVolumes[[j]]$description, rootVolumeName=fileService$rootVolumes[[i]]$name, rootVolumeDescription=fileService$rootVolumes[[i]]$description)
          k=k+1
        }
      }else{
        userVolumes[[k]] = list(userVolumeName= fileService$rootVolumes[[i]]$userVolumes[[j]]$name, path=path, userVolumeDescription=fileService$rootVolumes[[i]]$userVolumes[[j]]$description, rootVolumeName=fileService$rootVolumes[[i]]$name, rootVolumeDescription=fileService$rootVolumes[[i]]$description)
        k=k+1
      }
    }
  }
  return(userVolumes)
}





Files.splitPath <- function(path){
  
  if(startsWith(path,"/")){
    path = substring(path,2)
  }
  
  path = strsplit(path,"/")
  if(length(path[[1]]) < 3){
    stop("path variable does not conform with the format 'rootVolume/userVolumeOwner/userVolume/relativePath...'")
  }
  rootVolume = path[[1]][1]
  userVolumeOwner = path[[1]][2]
  userVolume = path[[1]][3]
  if(length(path[[1]])==3){
    relativePath = ""
  }else{
    relativePath = paste(path[[1]][4:(length(path[[1]]))] ,collapse = "/")
  }
  return(c(rootVolume,userVolumeOwner,userVolume,relativePath))
}



Files.createUserVolume <- function(fileService, path, quiet=TRUE){
  
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Files.createUserVolume"
    }else{
      taskName = "SciScript-R.Files.createUserVolume"
    }
    
    if(typeof(fileService) == "character"){
      fileService = Files.getFileServiceFromName(fileService, verbose=verbose);
    }
    
    vec = Files.splitPath(path)
    rootVolume = vec[1];userVolumeOwner=vec[2];userVolume=vec[3];relativePath=vec[4]
    
    url = paste(Files.__getFileServiceAPIUrl(fileService),"api/volume/",rootVolume,"/",userVolumeOwner,"/",userVolume,"?quiet=",tolower(as.character(quiet)),"&TaskName=",taskName,sep="");

    r = PUT(url,add_headers('X-Auth-Token'=token))
    
    if(r$status_code < 200 || r$status_code >= 300) {
      response_text = content(r, "text", encoding="UTF-8")
      stop(paste("Error when creating user volume  '",path,"' in file service '",fileService$name,"'.\nHttp Response from FileService API returned status code ",r$status_code,":\n",response_text,sep=""));
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}


Files.deleteUserVolume <- function(fileService, path, quiet=TRUE){
  
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Files.deleteUserVolume"
    }else{
      taskName = "SciScript-R.Files.deleteUserVolume"
    }
    
    if(typeof(fileService) == "character"){
      fileService = Files.getFileServiceFromName(fileService, verbose=verbose);
    }
    
    vec = Files.splitPath(path)
    rootVolume = vec[1];userVolumeOwner=vec[2];userVolume=vec[3];relativePath=vec[4]
    
    url = paste(Files.__getFileServiceAPIUrl(fileService),"api/volume/",rootVolume,"/",userVolumeOwner,"/",userVolume,"?quiet=",tolower(as.character(quiet)),"&TaskName=",taskName,sep="");
    
    r = DELETE(url,add_headers('X-Auth-Token'=token))
    
    if(r$status_code < 200 || r$status_code >= 300) {
      response_text = content(r, "text", encoding="UTF-8")
      stop(paste("Error when deleting user volume  '",path,"' in file service '",fileService$name,"'.\nHttp Response from FileService API returned status code ",r$status_code,":\n",response_text,sep=""));
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}




Files.createDir <- function(fileService, path, quiet=TRUE){

  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Files.createDir"
    }else{
      taskName = "SciScript-R.Files.createDir"
    }
    
    if(typeof(fileService) == "character"){
      fileService = Files.getFileServiceFromName(fileService, verbose=verbose);
    }

    vec = Files.splitPath(path)
    rootVolume = vec[1];userVolumeOwner=vec[2];userVolume=vec[3];relativePath=vec[4]

    url = paste(Files.__getFileServiceAPIUrl(fileService),"api/folder/",rootVolume,"/",userVolumeOwner,"/",userVolume,"/",relativePath,"?quiet=",tolower(as.character(quiet)),"&TaskName=",taskName,sep="");

    r = PUT(url,add_headers('X-Auth-Token'=token))

    if(r$status_code < 200 || r$status_code >= 300) {
      response_text = content(r, "text", encoding="UTF-8")
      stop(paste("Error when creating new directory  '",path,"' in file service '", fileService$name,"'.\nHttp Response from FileService API returned status code ",r$status_code,":\n",response_text,sep=""));
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}


Files.upload <- function(fileService, path, data=NULL, localFilePath=NULL, quiet=TRUE){
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {

    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Files.upload"
    }else{
      taskName = "SciScript-R.Files.upload"
    }
    
    if(typeof(fileService) == "character"){
      fileService = Files.getFileServiceFromName(fileService, verbose=verbose);
    }
    
    vec = Files.splitPath(path)
    rootVolume = vec[1];userVolumeOwner=vec[2];userVolume=vec[3];relativePath=vec[4]
    
    url = paste(Files.__getFileServiceAPIUrl(fileService),"api/file/",rootVolume,"/",userVolumeOwner,"/",userVolume,"/",relativePath,"?quiet=",tolower(as.character(quiet)),"&TaskName=",taskName,sep="");

    if( !is.null(localFilePath) && localFilePath != ""){
      r = PUT(url, body=upload_file(localFilePath), add_headers('X-Auth-Token'=token))
    }else{
      if(!is.null(data)){
        r = PUT(url, body=data, add_headers('X-Auth-Token'=token))
      }else{
        stop(paste("Error: No local file or data specified for uploading.",sep=""))
      }
    }
    
    if(r$status_code < 200 || r$status_code >= 300) {
      stop(paste("Error when uploading file to ", path, ".\nHttp Response from FileService API returned status code ", toString(res$status_code),":\n", content(r, as="text", encoding="UTF-8"), sep=""))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}


Files.download<-function(fileService, path, localFilePath=NULL, format="txt", quiet=TRUE){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Files.download"
    }else{
      taskName = "SciScript-R.Files.download"
    }
    
    if(typeof(fileService) == "character"){
      fileService = Files.getFileServiceFromName(fileService, verbose=verbose);
    }
    
    vec = Files.splitPath(path)
    rootVolume = vec[1];userVolumeOwner=vec[2];userVolume=vec[3];relativePath=vec[4]
    
    url = paste(Files.__getFileServiceAPIUrl(fileService),"api/file/",rootVolume,"/",userVolumeOwner,"/",userVolume,"/",relativePath,"?quiet=",tolower(as.character(quiet)),"&TaskName=",taskName,sep="");
    
    r = GET(url,add_headers('X-Auth-Token'=token))
    
    if(r$status_code >= 200 && r$status_code < 300){
      if(!is.null(localFilePath) && localFilePath != ""){
        bytes = content(r, "raw");
        theFile = file(localFilePath, "wb")
        writeBin(bytes, theFile)
        close(theFile)
        return(TRUE)
      }else{
        if(!is.null(format) && format != ""){
          if(format == "txt"){
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
      stop(paste("Error when downloading '",path,"' from file service '",fileService$name,"'.\nHttp Response from FileService API returned status code ",r$status_code,":\n", content(r, as="text", encoding="UTF-8"),sep=""));
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}  


Files.dirList <- function(fileService, path, level=1, options=""){

  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {

    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Files.dirList"
    }else{
      taskName = "SciScript-R.Files.dirList"
    }
    
    if(typeof(fileService) == "character"){
      fileService = Files.getFileServiceFromName(fileService, verbose=verbose);
    }
    
    vec = Files.splitPath(path)
    rootVolume = vec[1];userVolumeOwner=vec[2];userVolume=vec[3];relativePath=vec[4]
    
    url = paste(Files.__getFileServiceAPIUrl(fileService),"api/jsontree/",rootVolume,"/",userVolumeOwner,"/",userVolume,"/",relativePath,"?options=",options,"&level=",level,"&TaskName=",taskName,sep="");

    r = GET(url,add_headers('X-Auth-Token'=token))
    
    if(r$status_code >= 200 && r$status_code < 300) {
      return(content(r))
    } else {
      stop(paste("Error when getting the contents of '", path, "'.\nHttp Response from FileService API returned status code ", toString(r$status_code), ":\n", content(r, as="text", encoding="UTF-8"), sep=""))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}


Files.move <- function(fileService, path, destinationFileService, destinationPath, replaceExisting=TRUE, doCopy=TRUE){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Files.move"
    }else{
      taskName = "SciScript-R.Files.move"
    }
    
    if(typeof(fileService) == "character"){
      fileService = Files.getFileServiceFromName(fileService, verbose=verbose);
    }
    
    if(typeof(destinationFileService) == "character"){
      destinationFileService = Files.getFileServiceFromName(destinationFileService, verbose=verbose);
    }

    vec = Files.splitPath(path)
    rootVolume = vec[1];userVolumeOwner=vec[2];userVolume=vec[3];relativePath=vec[4]

    vec = Files.splitPath(destinationPath)
    destinationRootVolume = vec[1];destinationUserVolumeOwner=vec[2];destinationUserVolume=vec[3];destinationRelativePath=vec[4]

    if(is.null(userVolumeOwner)){
      userVolumeOwner = Authentication.getKeystoneUserWithToken(token)$userName;
    }
    
    if(is.null(destinationUserVolumeOwner)){
      destinationUserVolumeOwner = Authentication.getKeystoneUserWithToken(token)$userName;
    }
    
    if(is.null(destinationUserVolumeOwner)){
      destinationUserVolumeOwner = Authentication.getKeystoneUserWithToken(token)$userName;
    }
    
    if(typeof(destinationFileService) == "character"){
      destinationFileService = getFileServiceFromName(destinationFileService)
    }
    
    if(typeof(destinationFileService) == "character"){
      fileService = Files.getFileServiceFromName(destinationFileService, verbose=verbose);
    }
    
    if(typeof(destinationFileService) == "character"){
      destinationFileService = destinationFileService$name
    }
    
    if(typeof(destinationFileService) == "list"){
      destinationFileServiceName = destinationFileService$name
    }else{
      destinationFileServiceName = destinationFileService
    }
        
    url = paste(Files.__getFileServiceAPIUrl(fileService),"api/data/",rootVolume,"/",userVolumeOwner,"/",userVolume,"/",relativePath,"?replaceExisting=",tolower(as.character(replaceExisting)),"&doCopy=",tolower(as.character(doCopy)),"&TaskName=",taskName,sep="");
    
    data = list(
      destinationPath = destinationRelativePath,
      destinationRootVolume = destinationRootVolume,
      destinationUserVolume = destinationUserVolume,
      destinationOwnerName = destinationUserVolumeOwner,
      destinationFileService = destinationFileServiceName
    )
    
    r = PUT(url,body=data,content_type_json(), encode="json", accept("application/json"),add_headers('X-Auth-Token'=token))
    
    if(r$status_code < 200 || r$status_code >= 300) {
      print(fileService)
      print(destinationFileService)
      print(r)
      stop(paste("Error when moving '", path, "' in FileService '",fileService$name,"' to '", destinationPath, "' in FileService '",destinationFileService$name,"'.\nHttp Response from FileService API returned status code ", toString(r$status_code), ":\n",content(r, as="text", encoding="UTF-8"), sep=""))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}



Files.delete <- function(fileService, path, quiet=TRUE){
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Files.delete"
    }else{
      taskName = "SciScript-R.Files.delete"
    }
    
    if(typeof(fileService) == "character"){
      fileService = Files.getFileServiceFromName(fileService, verbose=verbose);
    }
    
    vec = Files.splitPath(path)
    rootVolume = vec[1];userVolumeOwner=vec[2];userVolume=vec[3];relativePath=vec[4]
    
    url = paste(Files.__getFileServiceAPIUrl(fileService),"api/data/",rootVolume,"/",userVolumeOwner,"/",userVolume,"/",relativePath,"?quiet=",as.character(quiet),"&TaskName=",taskName,sep="");
    
    r = DELETE(url,add_headers('X-Auth-Token'=token))
    
    if(r$status_code < 200 || r$status_code >= 300) {
      stop(paste("Error when deleting '", path, "'.\nHttp Response from FileService API returned status code ", toString(r$status_code), ":\n",content(r, as="text", encoding="UTF-8"), sep=""))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}
  
  
Files.shareUserVolume <- function(fileService, path, sharedWith, allowedActions, type="USER"){
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Files.shareUserVolume"
    }else{
      taskName = "SciScript-R.Files.shareUserVolume"
    }
    
    if(typeof(fileService) == "character"){
      fileService = Files.getFileServiceFromName(fileService, verbose=verbose);
    }
    
    vec = Files.splitPath(path)
    rootVolume = vec[1];userVolumeOwner=vec[2];userVolume=vec[3];relativePath=vec[4]
    

    data = list(list(
      name = sharedWith,
      type = type,
      allowedActions = allowedActions
    )
    )
    
    url = paste(Files.__getFileServiceAPIUrl(fileService),"api/share/",rootVolume,"/",userVolumeOwner,"/",userVolume,"?TaskName=",taskName,sep="");

    r = PATCH(url,body=data,content_type_json(), encode="json", accept("application/json"),add_headers('X-Auth-Token'=token))
    
    if(r$status_code < 200 || r$status_code >= 300) {
      stop(paste("Error when sharing userVolume '", path, "' in FileService '",fileService$name,"'.\nHttp Response from FileService API returned status code ", toString(r$status_code), ":\n", content(r, as="text", encoding="UTF-8"), sep=""))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}


