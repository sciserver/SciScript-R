#require(httr)
#require(jsonlite)
#require(utils)

######################################################################################################################
# Jobs:

SkyQuery.getJobStatus <- function(jobID)
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/jobs/',toString(jobID),sep="")
    
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

SkyQuery.cancelJob <- function(jobID)
{
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/jobs/',toString(jobID),sep="")
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
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/queues',sep="")
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
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/queues/',toString(queue),sep="")
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
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/queues/',queue,'/jobs',sep="")
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

SkyQuery.listJobs <- function(queue="quick")
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    url = paste(Config.SkyQueryUrl,'/Jobs.svc/queues/',queue,'/jobs?',sep="")
    data = list(queryJob=list(query=unbox(query)))
    r= GET(url,encode="json",body=data,accept("text/plain"),content_type_json(),add_headers('X-Auth-Token'=token))
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
    url = paste(Config.SkyQueryUrl,'/Schema.svc/datasets',sep="")
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

SkyQuery.getDatasetInfo <- function(datasetName)
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    url = paste(Config.SkyQueryUrl,'/Schema.svc/datasets/',datasetName,sep="")
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

SkyQuery.listDatasetTables <- function(datasetName)
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    url = paste(Config.SkyQueryUrl,'/Schema.svc/datasets/',datasetName,"/tables",sep="")
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

SkyQuery.getTableInfo <- function(datasetName, tableName)
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    url = paste(Config.SkyQueryUrl,'/Schema.svc/datasets/',datasetName,"/tables/",tableName,sep="")
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

SkyQuery.listTableColumns <- function(datasetName, tableName)
{
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    url = paste(Config.SkyQueryUrl,'/Schema.svc/datasets/',datasetName,"/tables/",tableName,"/columns",sep="")
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
    url = paste(Config.SkyQueryUrl,'/Data.svc/',datasetName,'/',tableName,sep="")
    if(!is.null(top) && top != "")
      url = paste(url,'?top=',toString(top),sep="")

    r= GET(url,encode="json",accept("application/json"),content_type_json(),add_headers('X-Auth-Token'=token))
    if(r$status_code != 200) {
      stop(paste("Http Response returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      t=read.csv(textConnection(content(r, encoding="UTF-8")),sep="\t")
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
    url = paste(Config.SkyQueryUrl,'/Data.svc/',datasetName,'/',tableName,sep="")
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
    url = paste(Config.SkyQueryUrl,'/Data.svc/',datasetName,'/',tableName,sep="")
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
