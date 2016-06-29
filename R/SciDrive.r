require( jsonlite)
require(httr)

# assumes Config is loaded

SciDrive.createContainer<-function(path, token=NULL){
  if (is.null(token))
    token=getToken()

    containerBody = paste('<vos:node xmlns:xsi="http://www.w3.org/2001/thisSchema-instance" ',
                     'xsi:type="vos:ContainerNode" xmlns:vos="http://www.ivoa.net/xml/VOSpace/v2.0" ',
                     'uri="vos://',Config.SciDriveHost,'!vospace/',path,'">',
                     '<vos:properties/><vos:accepts/><vos:provides/><vos:capabilities/>',
                     '</vos:node>',sep="")
    url = paste(Config.SciDriveHost,'/vospace-2.0/nodes/',path,sep="")
    res = PUT(url, content_type_xml(),body=containerBody,add_headers('X-Auth-Token'=token))
}


SciDrive.upload<-function(path, data, token=NULL){
    if(is.null(token)) token = getToken()
    url = paste(Config.SciDriveHost,'/vospace-2.0/1/files_put/dropbox/',path,sep='')
    res = PUT(url, body=data ,add_headers('X-Auth-Token'=token))
    return (res)
}

#------------------------------------
#    retrieve public URL for file identified by path
SciDrive.publicUrl<-function(path, token=NULL){
    if (is.null(token)) token=getToken()
    url = paste(Config.SciDriveHost,"/vospace-2.0/1/media/sandbox/", path,sep="")
    tryCatch({
      res = GET(url ,add_headers('X-Auth-Token'=token))
      if(res$status_code == 200) return(content(res)$url)
      else return
    },error=function(e){})
}