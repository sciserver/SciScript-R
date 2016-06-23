require(httr)
require(jsonlite)
require(utils)

#' @export
runSqlSearch  <- function(sql,limit="10", token=NULL)
{
  dataRelease = "DR12"
  
  url=paste("http://scitest02.pha.jhu.edu/SkyserverWS/", dataRelease, "/SearchTools/SqlSearch?",sep="")
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
    print("Error")
    print(content(r)$'Error Message')
    return (NULL)
  } else {
    t=utils::read.csv(textConnection(httr::content(r)))
    return(t)
  }

}
