#require(httr)
#require(jsonlite)

#source('LoginPortal.r')

#--------------------------------------------------------
getSchemaName<-function(token=NULL){
    if (is.null(token) ){
        token = getToken()
    }
    keystoneUserId = getKeystoneUserWithToken(token)$id
    usersUrl = paste(addSlash(CasJobsRESTUri),"users/", keystoneUserId,sep="")
    r = GET(usersUrl,add_headers('X-Auth-Token'=token),content_type_json())
    r= content(r)
    return (paste("wsid_",r$WebServicesId,sep=""))
}


#--------------------------------------------------------
# return tables in specified contxt, accessible to user
getTables<-function(context="MyDB"){
    TablesUrl = paste(addSlash(CasJobsRESTUri),"contexts/", context, "/Tables",sep="")
    r = GET(TablesUrl,add_headers('X-Auth-Token'=getToken()),content_type_json())
    return (content(r))
}

#--------------------------------------------------------
# synchronous query
executeQuery <- function(sql,context="MyDB",token=NULL) {
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

#--------------------------------------------------------
# asynch query
#    Submits a query to the casjobs queue.  If a token is supplied then it will execute on behalf of the token's user.
#    Returns the casjobs jobID (int).
submitJob<-function(queryString, context="MyDB", acceptHeader="text/plain", token=NULL){

    QueryUrl = paste(addSlash(CasJobsRESTUri),"contexts/",context,"/jobs",sep="")
    body = list(Query=unbox(queryString))
    if (is.null(token))
	token=getToken()

    putResponse = PUT(QueryUrl,encode="json",body=body,content_type_json(),accept("text/plain"),
	add_headers('X-Auth-Token'=token))
    return (content(putResponse))
}


#--------------------------------------------------------
# check job status
#    Gets a casjobs job status.
#    Returns the dict object (https://docs.python.org/3.4/library/stdtypes.html#dict) coresponding to the json received from casjobs.
getJobStatus<-function(jobid){
    QueryUrl = paste(addSlash(CasJobsRESTUri),"jobs/", jobid,sep="")

    r = GET(QueryUrl,content_type_json(),accept("application/json"),add_headers('X-Auth-Token'=getToken()))
    return(content(r))
}


#--------------------------------------------------------
# check job status
#    Waits for the casjobs job to return a status of 3, 4, or 5.
#    Queries the job status from casjobs every 2 seconds.
waitForJob<-function(jobid){
    complete = FALSE

    waitingStr = "Waiting."
    print(waitingStr)
    complete=FALSE
    jobDesc="NONE"
    ok=list(3,4,5)
    while (!complete){
        waitingStr=paste(waitingStr,'.',sep="")
        print(waitingStr)
        jobDesc = getJobStatus(jobid)
        jobStatus = strtoi(jobDesc$Status)
        if (jobStatus %in% ok){
            complete = TRUE
            print("Done!")
        } else{
            Sys.sleep(2)
        }
    }
    return (jobDesc)
}

#------------------------------------
#  Uploads  cvs data into casjobs. 
# return response object with attributes such as status_code,headers,url
uploadCVSToTable<-function(CVSdata, tableName, context="MyDB", token=NULL){
    tablesUrl = paste(addSlash(CasJobsRESTUri),"contexts/",context,"/Tables/",tableName,sep="")

    if (is.null(token))
	token=getToken()
    
    r = POST(tablesUrl,encode="multipart",body=CVSdata,add_headers('X-Auth-Token'=token))
    return (r)
}

#-------------------------------------
# upload a dataframe to a table in a database
uploadDataFrameToTable<-function(df, tableName, context="MyDB", token=NULL){
  csv=capture.output(write.csv(t,row.names=FALSE,quote=FALSE))
  return (uploadCVSToTable(csv,tableName,context,token))
}
