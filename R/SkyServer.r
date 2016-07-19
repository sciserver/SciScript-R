#require(httr)
#require(jsonlite)
#require(utils)


SkyServer.sqlSearch  <- function(sql,limit="10", token=NULL)
{

  url=paste(Config.SkyServerWSurl, '/', Config.DataRelease, "/SearchTools/SqlSearch?", sep="")
  url = paste(url,"format=csv&",sep="")
  url = paste(url,"cmd=",sql,"&",sep="")
  if(!is.null(limit)) {
    url = paste(url,"limit=",limit,"&",sep="")
  }
  url = URLencode(url)
  
  if(is.null(token)) {
    r= GET(url,accept("text/plain"))
  } else {
    r= GET(url,accept("text/plain"),add_headers('X-Auth-Token'=token))
  }
  if(r$status_code != 200) {
    writeLines(paste("Error: ",content(r)$'ErrorMessage',"\n","LogMessageID: ", content(r)$'LogMessageID',"\n","LogTime: ",content(r)$'LogTime',sep=""))
    return (NULL)
  } else {
    t=read.csv(textConnection(content(r, encoding="UTF-8")), comment.char = "#")
    return(t)
  }

}


SkyServer.getJpegImgCutout  <- function(ra, dec, scale=0.7, width=512, height=512, opt="", query="", token = NULL)
{
  
  url = paste(Config.SkyServerWSurl, '/', Config.DataRelease, "/ImgCutout/getjpeg?", sep="")
  url = paste(url,"ra=",ra,"&",sep="")
  url = paste(url,"dec=",dec,"&",sep="")
  url = paste(url,"scale=",scale,"&",sep="")
  url = paste(url,"width=",width,"&",sep="")
  url = paste(url,"height=",height,"&",sep="")
  url = paste(url,"opt=",opt,"&",sep="")
  url = paste(url,"query=",query,"&",sep="")
  
  url = URLencode(url)
  
  if(is.null(token)) {
    r= GET(url,accept("text/plain"))
  } else {
    r= GET(url,accept("text/plain"),add_headers('X-Auth-Token'=token))
  }
  if(r$status_code != 200) {
    writeLines(paste("Error: ", content(r)$'ErrorMessage',"\n","LogMessageID: ", content(r)$'LogMessageID',"\n","LogTime: ", content(r)$'LogTime',sep=""))
    return (NULL)
  } else {
    sdssImage = content(r)
    return(sdssImage)
  }
}

SkyServer.radialSearch  <- function(ra, dec, radius=1, coordType="equatorial", whichPhotometry="optical", limit="10", token=NULL)
{
  
  url=paste(Config.SkyServerWSurl, '/', Config.DataRelease, "/SearchTools/RadialSearch?", sep="")
  url = paste(url,"format=csv&",sep="")
  url = paste(url,"ra=",ra,"&",sep="")
  url = paste(url,"dec=",dec,"&",sep="")
  url = paste(url,"radius=",radius,"&",sep="")
  url = paste(url,"coordtype=",coordType,"&",sep="")
  url = paste(url,"whichphotometry=",whichPhotometry,"&",sep="")
  if(!is.null(limit)) {
    url = paste(url,"limit=",limit,"&",sep="")
  }
  url = URLencode(url)
  
  if(is.null(token)) {
    r= GET(url,accept("text/plain"))
  } else {
    r= GET(url,accept("text/plain"),add_headers('X-Auth-Token'=token))
  }
  if(r$status_code != 200) {
    writeLines(paste("Error: ",content(r)$'ErrorMessage',"\n","LogMessageID: ", content(r)$'LogMessageID',"\n","LogTime: ",content(r)$'LogTime',sep=""))
    return (NULL)
  } else {
    t=read.csv(textConnection(content(r, encoding="UTF-8")), comment.char = "#")
    return(t)
  }
}

SkyServer.rectangularSearch  <- function(min_ra, max_ra, min_dec, max_dec, coordType="equatorial", whichPhotometry="optical", limit="10", token=NULL)
{
  
  url=paste(Config.SkyServerWSurl, '/', Config.DataRelease, "/SearchTools/RectangularSearch?", sep="")
  url = paste(url,"format=csv&",sep="")
  url = paste(url,"min_ra=",min_ra,"&",sep="")
  url = paste(url,"max_ra=",max_ra,"&",sep="")
  url = paste(url,"min_dec=",min_dec,"&",sep="")
  url = paste(url,"max_dec=",max_dec,"&",sep="")
  url = paste(url,"coordtype=",coordType,"&",sep="")
  url = paste(url,"whichphotometry=",whichPhotometry,"&",sep="")
  if(!is.null(limit)) {
    url = paste(url,"limit=",limit,"&",sep="")
  }
  url = URLencode(url)
  
  if(is.null(token)) {
    r= GET(url,accept("text/plain"))
  } else {
    r= GET(url,accept("text/plain"),add_headers('X-Auth-Token'=token))
  }
  if(r$status_code != 200) {
    writeLines(paste("Error: ",content(r)$'ErrorMessage',"\n","LogMessageID: ", content(r)$'LogMessageID',"\n","LogTime: ",content(r)$'LogTime',sep=""))
    return (NULL)
  } else {
    t=read.csv(textConnection(content(r, encoding="UTF-8")), comment.char = "#")
    return(t)
  }
}

SkyServer.objectSearch  <- function(ra=NULL, dec=NULL, objID=NULL, specbBjID=NULL, apogeeID=NULL, name=NULL, token=NULL)
{
  
  url=paste(Config.SkyServerWSurl, '/', Config.DataRelease, "/SearchTools/ObjectSearch?query=loadexplore&format=json&", sep="")
  if(!is.null(ra) &&  !is.null(dec) ){
    url = paste(url,"&ra=",ra,"&dec=",dec,sep="")
  }else if(!is.null(objID)){
    url = paste(url,"&objID=",objID)
  }
  else{
    writeLines("There are missing parameter required for finding the object.")
    return(NULL)
  }
  url = URLencode(url)
  
  if(is.null(token)) {
    r= GET(url,accept("text/plain"))
  } else {
    r= GET(url,accept("text/plain"),add_headers('X-Auth-Token'=token))
  }
  if(r$status_code != 200) {
    writeLines(paste("Error: ",content(r)$'ErrorMessage',"\n","LogMessageID: ", content(r)$'LogMessageID',"\n","LogTime: ",content(r)$'LogTime',sep=""))
    return (NULL)
  } else {
    #t=read.csv(textConnection(content(r, encoding="UTF-8")), comment.char = "#")
    #return(t)
    #return(content(r, encoding="UTF-8"))
    return(r)
  }
}

