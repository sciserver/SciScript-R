#require( jsonlite)
#require(httr)

# assumes Config is loaded

SciDrive.createContainer<-function(path, token=NULL){
  if (is.null(token))
    token=Authentication.getToken()

    containerBody = paste('<vos:node xmlns:xsi="http://www.w3.org/2001/thisSchema-instance" ',
                     'xsi:type="vos:ContainerNode" xmlns:vos="http://www.ivoa.net/xml/VOSpace/v2.0" ',
                     'uri="vos://',Config.SciDriveHost,'!vospace/',path,'">',
                     '<vos:properties/><vos:accepts/><vos:provides/><vos:capabilities/>',
                     '</vos:node>',sep="")
    url = paste(Config.SciDriveHost,'/vospace-2.0/nodes/',path,sep="")
    r = PUT(url, content_type_xml(),body=containerBody,add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      #print("Error")
      #print(content(r,encoding="UTF-8"))
      #return (NULL)
      stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(r)
    }
}


SciDrive.upload<-function(path, data, token=NULL){
    if(is.null(token)) token = Authentication.getToken()
    url = paste(Config.SciDriveHost,'/vospace-2.0/1/files_put/dropbox/',path,sep='')
    r = PUT(url, body=upload_file(data), add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      #print("Error")
      #print(content(r,encoding="UTF-8"))
      #return (NULL)
      stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
    } else {
      return (r)
    }
}

#------------------------------------
#    retrieve public URL for file identified by path
SciDrive.publicUrl<-function(path, token=NULL){
    if (is.null(token)) token=Authentication.getToken()
    url = paste(Config.SciDriveHost,"/vospace-2.0/1/media/sandbox/", path,sep="")
    tryCatch({
      r = GET(url ,add_headers('X-Auth-Token'=token))
      if(r$status_code == 200){
        return(content(r)$url)
      }else{
        #print("Error")
        #print(content(res,encoding="UTF-8")$error)
        #return (NULL)
        stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
      }
    },error=function(e){stop(e)})
}