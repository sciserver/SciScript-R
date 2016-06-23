#require(httr)
#require(jsonlite)

source('LoginPortal.r')

# synchronous query
queryCAS <- function(sql,context="MyDB",token=NULL) {
  url=paste(addSlash(CasJobsRESTUri),'contexts/',context,'/query',sep='')

  if(is.null(token)) {
    r=POST(url,encode="json",body=list(Query=unbox(sql))
    ,accept("text/plain"),content_type_json())
  } else {
    r=POST(url,encode="json",body=list(Query=unbox(sql))
    ,accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
  }
  if(r$status_code != 200) {
    print("Error")
    r=content(r)
    print(r$`Error Message`)
    return (NULL)
  } else {
    t=read.csv(textConnection(content(r)))
    return(t)
  }
}
