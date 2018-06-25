#require(httr)
#require(jsonlite)
#require(utils)

######################################################################################################################
# Jobs:

SkyQuery.getJobStatus <- function(jobId)
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.getJobStatus"
    }else{
      taskName = "SciScript-R.SkyQuery.getJobStatus"
    }
    
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/jobs/',toString(jobId),"?TaskName=",taskName,sep="")
    
    r= GET(url,encode="json",accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
    
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SkyQuery.cancelJob <- function(jobId)
{
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.cancelJob"
    }else{
      taskName = "SciScript-R.SkyQuery.cancelJob"
    }
    
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/jobs/',toString(jobId),"?TaskName=",taskName,sep="")
    r= DELETE(url,encode="json",accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
    
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(TRUE)
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SkyQuery.listQueues <- function()
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.listQueues"
    }else{
      taskName = "SciScript-R.SkyQuery.listQueues"
    }
    
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/queues',"?TaskName=",taskName,sep="")
    r= GET(url,encode="json",accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      r = content(r, encoding="UTF-8")
      return (r$queues)
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SkyQuery.getQueueInfo <- function(queue)
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.getQueueInfo"
    }else{
      taskName = "SciScript-R.SkyQuery.getQueueInfo"
    }
    
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/queues/',toString(queue),"?TaskName=",taskName,sep="")
    r= GET(url,encode="json",accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      r = content(r, encoding="UTF-8")
      return (r$queue)
    }
  }else{ 
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SkyQuery.submitJob <- function(query, queue="quick") 
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.submitJob"
    }else{
      taskName = "SciScript-R.SkyQuery.submitJob"
    }
    
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/queues/',queue,'/jobs',"?TaskName=",taskName,sep="")
    data = list(queryJob=list(query=unbox(query)))
    r= POST(url,encode="json",body=data,accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      r = content(r, encoding="UTF-8")
      return (r$queryJob$guid)
    }
  }else{ 
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SkyQuery.waitForJob<-function(jobId, verbose=TRUE){
  complete = FALSE
  
  waitingStr = "Waiting..."
  if(verbose){
    print(waitingStr)
  }
  complete=FALSE
  jobDesc= NULL
  ok=list(3,4,5)
  while (!complete){
    if(verbose){
      print(waitingStr)
    }
    jobDesc = SkyQuery.getJobStatus(jobId)
    jobStatus = jobDesc$queryJob$status
    if (jobStatus == "completed" || jobStatus == "failed"){
      complete = TRUE
      if(verbose)
        print("Job Done!")
    } else{
      Sys.sleep(2)
    }
  }
  return (jobDesc$queryJob)
}

SkyQuery.listJobs <- function(queue="quick")
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.listJobs"
    }else{
      taskName = "SciScript-R.SkyQuery.listJobs"
    }  
    
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/queues/',queue,'/jobs',"?TaskName=",taskName,sep="")
    r= GET(url,encode="json",accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      r = content(r, encoding="UTF-8")
      return (r$jobs)
    }
  }else{ 
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

######################################################################################################################
# Schema:

SkyQuery.listAllDatasets <- function()
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.listAllDatasets"
    }else{
      taskName = "SciScript-R.SkyQuery.listAllDatasets"
    }
    
    url = paste(Config.SkyQueryUrl,'/Schema.svc/datasets',"?TaskName=",taskName,sep="")
    r= GET(url,encode="json",accept("application/json"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return (content(r)$datasets)
    }
  }else{ 
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SkyQuery.getDatasetInfo <- function(datasetName="MyDB")
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.getDatasetInfo"
    }else{
      taskName = "SciScript-R.SkyQuery.getDatasetInfo"
    }
    
    url = paste(Config.SkyQueryUrl,'/Schema.svc/datasets/',datasetName,"?TaskName=",taskName,sep="")
    r= GET(url,encode="json",accept("application/json"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return (content(r))
    }
  }else{ 
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SkyQuery.listDatasetTables <- function(datasetName="MyDB")
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.listDatasetTables"
    }else{
      taskName = "SciScript-R.SkyQuery.listDatasetTables"
    }
    
    url = paste(Config.SkyQueryUrl,'/Schema.svc/datasets/',datasetName,"/tables","?TaskName=",taskName,sep="")
    r= GET(url,encode="json",accept("application/json"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return (content(r)$tables)
    }
  }else{ 
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SkyQuery.getTableInfo <- function(tableName, datasetName="MyDB")
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.getTableInfo"
    }else{
      taskName = "SciScript-R.SkyQuery.getTableInfo"
    }
    
    url = paste(Config.SkyQueryUrl,'/Schema.svc/datasets/',datasetName,"/tables/",tableName,"?TaskName=",taskName,sep="")
    r= GET(url,encode="json",accept("application/json"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return (content(r))
    }
  }else{ 
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SkyQuery.listTableColumns <- function(tableName, datasetName="MyDB")
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.listTableColumns"
    }else{
      taskName = "SciScript-R.SkyQuery.listTableColumns"
    }  
    
    url = paste(Config.SkyQueryUrl,'/Schema.svc/datasets/',datasetName,"/tables/",tableName,"/columns","?TaskName=",taskName,sep="")
    r= GET(url,encode="json",accept("application/json"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return (content(r)$columns)
    }
  }else{ 
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

######################################################################################################################
# Data:

SkyQuery.getTable <- function(tableName, datasetName="MyDB", top = NULL)
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.getTable"
    }else{
      taskName = "SciScript-R.SkyQuery.getTable"
    }
    
    url = paste(Config.SkyQueryUrl,'/Data.svc/',datasetName,'/',tableName,sep="")
    if(!is.null(top) && top != "")
      url = paste(url,'?top=',toString(top),"&TaskName=",taskName,sep="")
    else
      url = paste(url,"?TaskName=",taskName,sep="")
    
    r= GET(url,encode="json",accept("application/json"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      #t=read.csv(textConnection(content(r, encoding="UTF-8")),sep="\t")
      table = content(r, encoding="UTF-8")
      t = fread(table, sep="\t")
      if(startsWith(names(t)[1],"#")){
        names(t)[1] <- substring(names(t)[1],2)
      }
      return(t)
    }
  }else{ 
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SkyQuery.dropTable <- function(tableName, datasetName="MyDB")
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.dropTable"
    }else{
      taskName = "SciScript-R.SkyQuery.dropTable"
    }
    
    url = paste(Config.SkyQueryUrl,'/Data.svc/',datasetName,'/',tableName,"?TaskName=",taskName,sep="")
    r= DELETE(url,encode="json",accept("application/json"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(TRUE)
    }
  }else{ 
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

SkyQuery.uploadTable <- function(uploadData, tableName, datasetName="MyDB", format="csv")
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    taskName = ""
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.SkyQuery.uploadTable"
    }else{
      taskName = "SciScript-R.SkyQuery.uploadTable"
    }
    
    url = paste(Config.SkyQueryUrl,'/Data.svc/',datasetName,'/',tableName,"?TaskName=",taskName,sep="")
    ctype = ""
    if(format == "csv"){
      ctype = 'text/csv'
    }else{
      ctype = 'text/csv'
    }

    r= PUT(url, body = uploadData, accept("application/json"), content_type(ctype),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(TRUE)
    }
  }else{ 
    stop(paste("User token is not defined. First log into SciServer."))
  }
}
