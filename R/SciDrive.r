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
    url = paste(Config.SciDriveHost,'/vospace-2.0/nodes/',path,sep="")
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
    url = paste(Config.SciDriveHost,'/vospace-2.0/1/files_put/dropbox/',path,sep='')
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
SciDrive.publicUrl<-function(scidrivePath){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    url = paste(Config.SciDriveHost,"/vospace-2.0/1/media/sandbox/", scidrivePath,sep="")
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

SciDrive.download<-function(scidrivePath){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    fileUrl = SciDrive.publicUrl(scidrivePath)
    
    r = GET(fileUrl)
    if(r$status_code == 200){
      return(r)
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
    url = paste(Config.SciDriveHost,'/vospace-2.0/nodes/',path,sep="")
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
