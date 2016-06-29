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
    r=httr::GET(url,httr::accept("text/plain"))
  } else {
    r=httr::GET(url,httr::accept("text/plain"),add_headers('X-Auth-Token'=token))
  }
  if(r$status_code != 200) {
    writeLines(paste("Error: ",httr::content(r)$'ErrorMessage',"\n","LogMessageID: ", httr::content(r)$'LogMessageID',"\n","LogTime: ",httr::content(r)$'LogTime',sep=""))
    return (NULL)
  } else {
    t=utils::read.csv(textConnection(httr::content(r, encoding="UTF-8")))
    return(t)
  }

}


SkyServer.getJpegImgCutout  <- function(ra, dec, scale=0.7, width=512, height=512, token = NULL)
{
  
  url = paste(SkyServerWSurl, '/', DataRelease, "/ImgCutout/getjpeg?", sep="")
  url = paste(url,"ra=",ra,"&",sep="")
  url = paste(url,"dec=",dec,"&",sep="")
  url = paste(url,"scale=",scale,"&",sep="")
  url = paste(url,"width=",width,"&",sep="")
  url = paste(url,"height=",height,"&",sep="")
  
  url = URLencode(url)
  
  if(is.null(token)) {
    r=httr::GET(url,httr::accept("text/plain"))
  } else {
    r=httr::GET(url,httr::accept("text/plain"),add_headers('X-Auth-Token'=token))
  }
  if(r$status_code != 200) {
    writeLines(paste("Error: ",httr::content(r)$'ErrorMessage',"\n","LogMessageID: ", httr::content(r)$'LogMessageID',"\n","LogTime: ",httr::content(r)$'LogTime',sep=""))
    return (NULL)
  } else {
    sdssImage = content(r)
    return(sdssImage)
  }
}

#image = content(r)
#plot(0:1, 0:1, type = "n")
#rasterImage(image, 0, 0, 1, 1)



