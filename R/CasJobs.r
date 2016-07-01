#require(httr)
#require(jsonlite)

#source('LoginPortal.r')

#--------------------------------------------------------
CasJobs.getSchemaName<-function(token=NULL){
    if (is.null(token) ){
        token = LoginPortal.getToken()
    }
    if(!is.null(token))
    {
      keystoneUserId = LoginPortal.getKeystoneUserWithToken(token)$id
      usersUrl = paste(Config.CasJobsRESTUri,"/users/", keystoneUserId,sep="")
      r = GET(usersUrl,add_headers('X-Auth-Token'=token),content_type_json())
      if(r$status_code != 200) {
        r= content(r, encoding="UTF-8")
        print("Error")
        r=content(r, encoding="UTF-8")
        print(r$`Error Message`)
        return (NULL)
      } else {
        r= content(r, encoding="UTF-8")
        return (paste("wsid_",r$WebServicesId,sep=""))
      }
    }else{
      return (NULL)
    }
}


#--------------------------------------------------------
# return tables in specified contxt, accessible to user
CasJobs.getTables<-function(context="MyDB"){
    TablesUrl = paste(Config.CasJobsRESTUri,"/contexts/", context, "/Tables",sep="")
    r = GET(TablesUrl,add_headers('X-Auth-Token'=LoginPortal.getToken()),content_type_json())
    if(r$status_code != 200) {
      print("Error")
      r=content(r, encoding="UTF-8")
      print(r$`Error Message`)
      return (NULL)
    } else {
      return (content(r, encoding="UTF-8"))
    }
    
}

#--------------------------------------------------------
# synchronous query
CasJobs.executeQuery <- function(sql,context="MyDB",token=NULL) {
  url=paste(Config.CasJobsRESTUri,'/contexts/',context,'/query',sep='')

  if(is.null(token)) {
    r=POST(url,encode="json",body=list(Query=unbox(sql))
    ,accept("text/plain"),content_type_json())
  } else {
    r=POST(url,encode="json",body=list(Query=unbox(sql))
    ,accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
  }
  if(r$status_code != 200) {
    print("Error")
    r=content(r, encoding="UTF-8")
    print(r$`Error Message`)
    return (NULL)
  } else {
    t=read.csv(textConnection(content(r, encoding="UTF-8")))
    return(t)
  }
}

#--------------------------------------------------------
# asynch query
#    Submits a query to the casjobs queue.  If a token is supplied then it will execute on behalf of the token's user.
#    Returns the casjobs jobID (int).
CasJobs.submitJob<-function(queryString, context="MyDB", token=NULL){

    QueryUrl = paste(Config.CasJobsRESTUri,"/contexts/",context,"/jobs",sep="")
    body = list(Query=unbox(queryString))
    if (is.null(token))
	token=LoginPortal.getToken()

    putResponse = PUT(QueryUrl,encode="json",body=body,content_type_json(),accept("text/plain"),
	add_headers('X-Auth-Token'=token))
    if(putResponse$status_code != 200) {
      print("Error")
      putResponse=content(putResponse, encoding="UTF-8")
      print(putResponse$`Error Message`)
      return (NULL)
    } else {
      return (content(putResponse, encoding="UTF-8"))
    }
}


#--------------------------------------------------------
# check job status
#    Gets a casjobs job status.
#    Returns the dict object (https://docs.python.org/3.4/library/stdtypes.html#dict) coresponding to the json received from casjobs.
CasJobs.getJobStatus<-function(jobid){
    QueryUrl = paste(Config.CasJobsRESTUri,"/jobs/", jobid,sep="")

    r = GET(QueryUrl,content_type_json(),accept("application/json"),add_headers('X-Auth-Token'=LoginPortal.getToken()))
    if(r$status_code != 200) {
      print("Error")
      r=content(r, encoding="UTF-8")
      print(r$`Error Message`)
      return (NULL)
    } else {
      return(content(r, encoding="UTF-8"))
    }
}


#--------------------------------------------------------
# check job status
#    Waits for the casjobs job to return a status of 3, 4, or 5.
#    Queries the job status from casjobs every 2 seconds.
CasJobs.waitForJob<-function(jobid){
    complete = FALSE

    waitingStr = "Waiting."
    print(waitingStr)
    complete=FALSE
    jobDesc="NONE"
    ok=list(3,4,5)
    while (!complete){
        waitingStr=paste(waitingStr,'.',sep="")
        print(waitingStr)
        jobDesc = CasJobs.getJobStatus(jobid)
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
CasJobs.uploadCSVToTable<-function(csv, tableName, context="MyDB", token=NULL){
    tablesUrl = paste(Config.CasJobsRESTUri,"/contexts/",context,"/Tables/",tableName,sep="")

    if (is.null(token))
	token=LoginPortal.getToken()
    tryCatch({
      
    r = POST(tablesUrl,encode="multipart",body=upload_file(csv),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      print("Error")
      r=content(r, encoding="UTF-8")
      print(r$`Error Message`)
      return (NULL)
    } else {
      return (r)
    }
    } , error = function(e) {
      print(e)
      return (NULL)
    })
}

#-------------------------------------
# upload a dataframe to a table in a database
CasJobs.uploadDataFrameToTable<-function(df, tableName, context="MyDB", token=NULL){
  csv=capture.output(write.csv(df,row.names=FALSE,quote=FALSE))
  return (CasJobs.uploadCSVToTable(csv,tableName,context,token))
}
