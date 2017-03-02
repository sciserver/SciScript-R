#require(httr)
#require(jsonlite)

#source('Authentication.r')

#--------------------------------------------------------
CasJobs.getSchemaName<-function(){
    token = Authentication.getToken()
    if(!is.null(token) && token != "")
    {
      keystoneUserId = Authentication.getKeystoneUserWithToken(token)$id
      usersUrl = paste(Config.CasJobsRESTUri,"/users/", keystoneUserId,sep="")
      r = GET(usersUrl,add_headers('X-Auth-Token'=token),content_type_json())
      if(r$status_code != 200) {
        stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
      } else {
        r= content(r, encoding="UTF-8")
        return (paste("wsid_",r$WebServicesId,sep=""))
      }
    }else{
       stop(paste("User token is not defined. First log into SciServer."))
    }
}


#--------------------------------------------------------
# return tables in specified context, accessible to user
CasJobs.getTables<-function(context="MyDB"){
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    TablesUrl = paste(Config.CasJobsRESTUri,"/contexts/", context, "/Tables",sep="")
    r = GET(TablesUrl,add_headers('X-Auth-Token'=token),content_type_json())
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
    } else {
      return (content(r, encoding="UTF-8"))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

#--------------------------------------------------------
# synchronous query
CasJobs.executeQuery <- function(sql,context="MyDB") {
  
  token = Authentication.getToken()
  
  url=paste(Config.CasJobsRESTUri,'/contexts/',context,'/query',sep='')
  
  TaskName="";
  if(Config.isSciServerComputeEnvironment()){
    TaskName = "Compute.SciScript-R.CasJobs.executeQuery"
  }else{
    TaskName = "SciScript-R.CasJobs.executeQuery"
  }

  if(!is.null(token) && token != "") {
    r=POST(url,encode="json",body=list(Query=unbox(sql), TaskName=TaskName),accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
  } else {
    r=POST(url,encode="json",body=list(Query=unbox(sql), TaskName=TaskName), accept("text/plain"),content_type_json())
  }
  if(r$status_code != 200) {
    stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
  } else {
    t=read.csv(textConnection(content(r, encoding="UTF-8")))
    return(t)
  }
}

#--------------------------------------------------------
# asynch query
#    Submits a query to the casjobs queue.  If a token is supplied then it will execute on behalf of the token's user.
#    Returns the casjobs jobID (int).
CasJobs.submitJob<-function(sql="", context="MyDB"){

  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    QueryUrl = paste(Config.CasJobsRESTUri,"/contexts/",context,"/jobs",sep="")
    
    TaskName="";
    if(Config.isSciServerComputeEnvironment()){
      TaskName = "Compute.SciScript-R.CasJobs.executeQuery"
    }else{
      TaskName = "SciScript-R.CasJobs.executeQuery"
    }

    body = list(Query=unbox(sql), TaskName=TaskName)
    r = PUT(QueryUrl,encode="json",body=body,content_type_json(),accept("text/plain"),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
    } else {
      return (content(r, encoding="UTF-8"))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}


#--------------------------------------------------------
# check job status
#    Gets a casjobs job status.
#    Returns the dict object (https://docs.python.org/3.4/library/stdtypes.html#dict) coresponding to the json received from casjobs.
CasJobs.getJobStatus<-function(jobid){

  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    QueryUrl = paste(Config.CasJobsRESTUri,"/jobs/", jobid,sep="")

    r = GET(QueryUrl,content_type_json(),accept("application/json"),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r, encoding="UTF-8"))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
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
            print("Job Done!")
        } else{
            Sys.sleep(2)
        }
    }
    return (jobDesc)
}

#------------------------------------
#  Uploads  cvs data into casjobs. 
# return response object with attributes such as status_code,headers,url
CasJobs.uploadCSVToTable<-function(csv, tableName, context="MyDB"){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    if(file.exists(csv)){
      tablesUrl = paste(Config.CasJobsRESTUri,"/contexts/",context,"/Tables/",tableName,sep="")
      r = POST(tablesUrl,encode="multipart",body=upload_file(csv),add_headers('X-Auth-Token'=token))
      if(r$status_code == 200) {
        return(TRUE)
      } else {
        stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
      }
    }else{
      stop(paste("Unable to find CSV file: ",csv,sep=""))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }    
}

#-------------------------------------
# upload a dataframe to a table in a database
CasJobs.uploadDataFrameToTable<-function(df, tableName, context="MyDB"){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    tablesUrl = paste(Config.CasJobsRESTUri,"/contexts/",context,"/Tables/",tableName,sep="")
    body=capture.output(write.csv(df,row.names=FALSE,quote=FALSE))
    r = POST(tablesUrl,encode="multipart",body=body,add_headers('X-Auth-Token'=token))
    if(r$status_code == 200) {
      return(TRUE)
    } else {
      stop(paste("Http Response returned status code ", r$status_code, ": ",  content(r, as="text", encoding="UTF-8")))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }    
}
