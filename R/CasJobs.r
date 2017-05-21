#require(httr)
#require(jsonlite)

#source('Authentication.r')

#--------------------------------------------------------
CasJobs.getSchemaName<-function(){
    token = Authentication.getToken()
    if(!is.null(token) && token != "")
    {
    
      taskName = ""
      if(Config.isSciServerComputeEnvironment()){
	taskName = "Compute.SciScript-R.CasJobs.getSchemaName"
      }else{
	taskName = "SciScript-R.CasJobs.getSchemaName"
      }

      keystoneUserId = Authentication.getKeystoneUserWithToken(token)$id
      usersUrl = paste(Config.CasJobsRESTUri,"/users/", keystoneUserId,"?TaskName=",taskName,sep="")
      r = GET(usersUrl,add_headers('X-Auth-Token'=token),content_type_json())
      if(r$status_code != 200) {
        stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
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
 
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.CasJobs.getTables"
    }else{
      taskName = "SciScript-R.CasJobs.getTables"
    }

    TablesUrl = paste(Config.CasJobsRESTUri,"/contexts/", context, "/Tables","?TaskName=",taskName,sep="")
    r = GET(TablesUrl,add_headers('X-Auth-Token'=token),content_type_json())
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return (content(r, encoding="UTF-8"))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

#--------------------------------------------------------
# synchronous query
CasJobs.executeQuery <- function(sql, context="MyDB", format="dataframe") {
  
  token = Authentication.getToken()
  
  url=paste(Config.CasJobsRESTUri,'/contexts/',context,'/query',"?TaskName=",taskName,sep='')
  
  taskName="";
  if(Config.isSciServerComputeEnvironment()){
    taskName = "Compute.SciScript-R.CasJobs.executeQuery"
  }else{
    taskName = "SciScript-R.CasJobs.executeQuery"
  }

  atype = NULL
  if( format == "list" || format =="json"){
    atype="application/json"
  }else if (format == "csv" || format == "dataframe"){
    atype = "text/plain"
  }else if(format == "fits"){
    atype = "application/fits"
  }else if(format == "raw"){
    atype = "application/fits" # to be implemented later
  }else {
    stop(paste("Illegal format parameter specification: ", format))
  }

  if(!is.null(token) && token != "") {
    r=POST(url,encode="json",body=list(Query=unbox(sql), TaskName=taskName),accept(atype),content_type_json(),add_headers('X-Auth-Token'=token))
  } else {
    r=POST(url,encode="json",body=list(Query=unbox(sql), TaskName=taskName), accept(atype),content_type_json())
  }
  if(r$status_code != 200) {
    stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
  } else {
    if(format == "list"){
      return(content(r)) #read.csv(textConnection(content(r, encoding="UTF-8")))
    }else if(format == "dataframe"){
        #list = content(r);
        #return(data.frame(list[[1]]$Rows))
        table = content(r, encoding="UTF-8")
        if(table == "\n"){
          return(data.frame(NULL))
        }else{
          t = fread(table, showProgress = FALSE)  # showProgress = FALSE supresses the warning mesages
          return(t);
        }
    }else if(format == "json"){
      return(content(r, "text", encoding="UTF-8"))
    }else if(format == "csv"){
      return(content(r, encoding="UTF-8"))
    }else if(format == "fits"){
      return(content(r, "raw"))
    }else if(format == "raw"){
      return(content(r, "raw"))
    }else{
      stop(paste("Illegal format parameter specification: ", format))
    }
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
  
    taskName="";
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.CasJobs.submitJob"
    }else{
      taskName = "SciScript-R.CasJobs.submitJob"
    }
  
    QueryUrl = paste(Config.CasJobsRESTUri,"/contexts/",context,"/jobs","?TaskName=",taskName,sep="")

    body = list(Query=unbox(sql), TaskName=taskName)
    r = PUT(QueryUrl,encode="json",body=body,content_type_json(),accept("text/plain"),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
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
CasJobs.getJobStatus<-function(jobId){

  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {

    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.CasJobs.getJobStatus"
    }else{
      taskName = "SciScript-R.CasJobs.getJobStatus"
    }
  
    QueryUrl = paste(Config.CasJobsRESTUri,"/jobs/", jobId,"?TaskName=",taskName,sep="")

    r = GET(QueryUrl,content_type_json(),accept("application/json"),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r, encoding="UTF-8"))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

CasJobs.cancelJob<-function(jobId){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {

    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.CasJobs.cancelJob"
    }else{
      taskName = "SciScript-R.CasJobs.cancelJob"
    }
  
    QueryUrl = paste(Config.CasJobsRESTUri,"/jobs/", jobId,"?TaskName=",taskName,sep="")
    
    r = GET(QueryUrl,content_type_json(),accept("application/json"),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(TRUE)
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

#--------------------------------------------------------
# check job status
#    Waits for the casjobs job to return a status of 3, 4, or 5.
#    Queries the job status from casjobs every 2 seconds.
CasJobs.waitForJob<-function(jobId, verbose=TRUE){
    complete = FALSE

    if(verbose){
      waitingStr = "Waiting..."
      print(waitingStr)
    }
    complete=FALSE
    jobDesc= NULL
    ok=list(3,4,5)
    while (!complete){
      if(verbose){
        waitingStr=paste(waitingStr,sep="")
        print(waitingStr)
      }
      jobDesc = CasJobs.getJobStatus(jobId)
      jobStatus = strtoi(jobDesc$Status)
      if (jobStatus %in% ok){
          complete = TRUE
          if(verbose)
            print("Job Done!")
      } else{
          Sys.sleep(2)
      }
    }
    return (jobDesc)
}


CasJobs.writeFitsFileFromQuery <- function(fileName, queryString, context="MyDB"){

  bytes = CasJobs.executeQuery(queryString, context=context, format="fits")
  theFile = file(fileName, "wb")
  writeBin(bytes, theFile)
  close(theFile)
  return(TRUE)
}


#------------------------------------
#  Uploads  cvs data into casjobs. 
# return response object with attributes such as status_code,headers,url
CasJobs.uploadCSVFileToTable<-function(filePath, tableName, context="MyDB"){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    if(file.exists(filePath)){

      taskName = ""
      if(Config.isSciServerComputeEnvironment()){
	taskName = "Compute.SciScript-R.CasJobs.cancelJob"
      }else{
	taskName = "SciScript-R.CasJobs.cancelJob"
      }

      tablesUrl = paste(Config.CasJobsRESTUri,"/contexts/",context,"/Tables/",tableName,"?TaskName=",taskName,sep="")
      r = POST(tablesUrl,encode="multipart",body=upload_file(filePath),add_headers('X-Auth-Token'=token))
      if(r$status_code == 200) {
        return(TRUE)
      } else {
        stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
      }
    }else{
      stop(paste("Unable to find CSV file: ",filePath,sep=""))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }    
}

#------------------------------------
#  Uploads  cvs data into casjobs. 
# return response object with attributes such as status_code,headers,url
CasJobs.uploadCSVDataToTable<-function(csv, tableName, context="MyDB"){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {

      taskName = ""
      if(Config.isSciServerComputeEnvironment()){
	taskName = "Compute.SciScript-R.CasJobs.cancelJob"
      }else{
	taskName = "SciScript-R.CasJobs.cancelJob"
      }
  
      tablesUrl = paste(Config.CasJobsRESTUri,"/contexts/",context,"/Tables/",tableName,"?TaskName=",taskName,sep="")
      r = POST(tablesUrl,content_type("text/csv"),body=csv,add_headers('X-Auth-Token'=token))
      if(r$status_code == 200) {
        return(TRUE)
      } else {
        stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
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
  
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.CasJobs.cancelJob"
    }else{
      taskName = "SciScript-R.CasJobs.cancelJob"
    }
  
    tablesUrl = paste(Config.CasJobsRESTUri,"/contexts/",context,"/Tables/",tableName,"?TaskName=",taskName,sep="")
    body=capture.output(write.csv(df,row.names=FALSE,quote=FALSE))
    r = POST(tablesUrl,encode="multipart",body=body,add_headers('X-Auth-Token'=token))
    if(r$status_code == 200) {
      return(TRUE)
    } else {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }    
}
