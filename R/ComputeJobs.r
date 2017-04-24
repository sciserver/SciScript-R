#require(httr)
#require(jsonlite)

#source('Authentication.r')

#--------------------------------------------------------
ComputeJobs.getJobDirectory <- function(){

  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    computeJobDirectoryFile = Config.ComputeJobDirectoryFile
    if(file.exists(computeJobDirectoryFile)){
      jobDirectory = NULL;
      jobDirectory =  readLines(f)
      jobDirectory = qsub("\n", "", jobDirectory)
      return jobDirectory;
    }else{
      stop("Cannot find jobs directory.")
    }
  }else{
  stop(paste("User token is not defined. First log into SciServer."))
  }
}

  
ComputeJobs.getComputeDomains <- function(batch=TRUE, interactive=FALSE){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {

    queryString1 <- if(as.logical(batch) == True) "batch=true&" else "batch=false&";
    queryString2 <- if(as.logical(interactive) == True) "interactive=true&" else "interactive=false&";
    queryString <- paste(queryString1, queryString2, sep="")
    
    url = paste(Config.JobmApiURL,"/computedomains?", queryString, sep="")
    r = GET(url,add_headers('X-Auth-Token'=token),accept("application/json"))

    if(r$status_code != 200) {
      stop(paste("Error when getting Compute Domains from JOBM API.\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

    
ComputeJobs.getJobTypes <-function(){
  return("")
}

ComputeJobs.getJobList <- function(){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    url = paste(Config.JobmApiURL,"/jobs", queryString, sep="")
    r = GET(url,add_headers('X-Auth-Token'=token),accept("application/json"))
    
    if(r$status_code != 200) {
      stop(paste("Error when getting Jobs list from JOBM API.\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

ComputeJobs.getJobDescription <- function(jobId){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    url = paste(Config.JobmApiURL,"/jobs", toString(jobId), sep="")
    r = GET(url,add_headers('X-Auth-Token'=token),accept("application/json"))
    
    if(r$status_code != 200) {
      stop(paste("Error when getting Job description of jobId=", toString(jobId), ".\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}


ComputeJobs.getJobStatus <- function(jobId){

  intStatus = SciDrive.getJobDescription(jobId)["status"]
  
  if(intStatus == 1)
    return("PENDING")
  }else if(intStatus == 2){
    return("QUEUED")
  }else if(intStatus == 4){
    return("ACCEPTED")
  }else if(intStatus == 8){
    return("STARTED")
  }else if(intStatus == 16){
    return("FINISHED")
  }else if(intStatus == 32){
    return("SUCCESS")
  }else if(intStatus == 64){
    return("ERROR")
  }else if(intStatus == 128){
    return("CANCELED")
  }else{
    stop("Invalid integer value given to job status.")
  }
}



ComputeJobs.submitNotebookJob <- function(domainApiEndpoint, notebookPath, dockerImage="", volumes=list(), parameters="", jobAlias = ""){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    dockerJobModel = list(
                          command = parameters,
                          scriptURI = notebookPath,
                          submitterDID = jobAlias,
                          dockerComputeEndpoint = domainApiEndpoint,
                          dockerImageName = dockerImage,
                          volumeContainers = volumes
                         )
    
    url = paste(Config.JobmApiURL,"/jobs/docker", sep="")
    r = POST(url,add_headers('X-Auth-Token'=token),data = dockerJobModel, content_type_json(), encode="json", accept("application/json"))

    if(r$status_code != 200) {
      stop(paste("Error when submitting a job to the JOBM API.\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

ComputeJobs.submitShellCommandJob <- function(domainApiEndpoint, shellCommand, dockerImage="", volumes=list(), jobAlias = ""){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    dockerJobModel = list(
      command = shellCommand,
      submitterDID = jobAlias,
      dockerComputeEndpoint = domainApiEndpoint,
      dockerImageName = dockerImage,
      volumeContainers = volumes
    )
    
    url = paste(Config.JobmApiURL,"/jobs/docker", sep="")
    r = POST(url,add_headers('X-Auth-Token'=token),data = dockerJobModel, content_type_json(), encode="json", accept("application/json"))
    
    if(r$status_code != 200) {
      stop(paste("Error when submitting a job to the JOBM API.\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}


ComputeJobs.cancelJob <- function(jobId){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {

    url = paste(Config.JobmApiURL,"/jobs/",toString(jobId), sep="")
    r = POST(url,add_headers('X-Auth-Token'=token))
    
    if(r$status_code != 200) {
      stop(paste("Error when cancelling job labeled by jobId=", toString(jobId), ".\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(TRUE)
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

ComputeJobs.waitForJob<-function(jobId, verbose=TRUE, pollTime = 5){
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
      jobDesc = ComputeJobs.getJobDescription(jobId)
      jobStatus = jobDesc$status
      if (jobStatus >= 32){
          complete = TRUE
          if(verbose)
            print("Job Done!")
      } else{
          Sys.sleep(max(pollTime,5))
      }
    }
    return (jobDesc)
}
