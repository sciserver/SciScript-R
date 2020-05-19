#require(httr)
require(jsonlite)

#source('Authentication.r')

#--------------------------------------------------------




Jobs.getDockerComputeDomains <- function(){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {

    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Jobs.getDockerComputeDomains"
    }else{
      taskName = "SciScript-R.Jobs.getDockerComputeDomains"
    }
    
    url = paste(Config.RacmApiURL,"/jobm/rest/computedomains?batch=true&interactive=false&TaskName=",taskName,sep="")
    r = GET(url,add_headers('X-Auth-Token'=token),accept("application/json"))

    if(r$status_code != 200) {
      stop(paste("Error when getting Docker Compute Domains from JOBM API.\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

Jobs.getDockerComputeDomainsNames <- function(dockerComputeDomains=NULL){
  if(is.null(dockerComputeDomains)){
    dockerComputeDomains = Jobs.getDockerComputeDomains()
  }
  dockerComputeDomainsNames = list()
  for(i in 1:length(dockerComputeDomains)){
    dockerComputeDomainsNames[[i]] = dockerComputeDomains[[i]]$name
  }
  return(dockerComputeDomainsNames)
}

Jobs.getDockerComputeDomainFromName <- function(dockerComputeDomainName, dockerComputeDomains = NULL){

  if(is.null(dockerComputeDomainName)){
    stop("dockerComputeDomainName is not defined.")
  }else{
    
    if(is.null(dockerComputeDomains)){
      dockerComputeDomains = Jobs.getDockerComputeDomains()
    }

    if(length(dockerComputeDomains) == 0){
      
      stop("There are no DockerComputeDomains available for the user.")
      
    }else{
      for(i in 1:length(dockerComputeDomains)){
        if(dockerComputeDomains[[i]]$name == dockerComputeDomainName){
          return(dockerComputeDomains[[i]])
        }
      }
    }
      
  }
  stop(paste("DockerComputeDomain of name '",dockerComputeDomainName,"' is not available or does not exist.",sep=""));
}



Jobs.getRDBComputeDomains <- function(){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Jobs.getRDBComputeDomains"
    }else{
      taskName = "SciScript-R.Jobs.getRDBComputeDomains"
    }
    
    url = paste(Config.RacmApiURL,"/jobm/rest/computedomains/rdb?TaskName=",taskName,sep="")
    r = GET(url,add_headers('X-Auth-Token'=token),accept("application/json"))
    
    if(r$status_code != 200) {
      stop(paste("Error when getting RDB Compute Domains from JOBM API.\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r))
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

Jobs.getRDBComputeDomainsNames <- function(rdbComputeDomains=NULL){
  if(is.null(rdbComputeDomains)){
    rdbComputeDomains = Jobs.getRDBComputeDomains()
  }
  rdbComputeDomainsNames = list()
  for(i in 1:length(rdbComputeDomains)){
    rdbComputeDomainsNames[[i]] = rdbComputeDomains[[i]]$name
  }
  return(rdbComputeDomainsNames)
}

Jobs.getRDBComputeDomainFromName <- function(rdbComputeDomainName, rdbComputeDomains = NULL){
  
  if(is.null(rdbComputeDomainName)){
    stop("rdbComputeDomainName is not defined.")
  }else{
    
    if(is.null(rdbComputeDomains)){
      rdbComputeDomains = Jobs.getRDBComputeDomains()
    }
    
    if(length(rdbComputeDomains) == 0){
      
      stop("There are no RDBComputeDomains available for the user.")
      
    }else{
      for(i in 1:length(rdbComputeDomains)){
        if(rdbComputeDomains[[i]]$name == rdbComputeDomainName){
          return(rdbComputeDomains[[i]])
        }
      }
    }
    
  }
  stop(paste("RDBComputeDomain of name '",rdbComputeDomainName,"' is not available or does not exist.",sep=""));
}


Jobs.getJobsList <- function(top=10, open=NULL, start=NULL, end=NULL, type="all"){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Jobs.getJobList"
    }else{
      taskName = "SciScript-R.Jobs.getJobList"
    }
    
    topString <- if(!is.null(top)) paste("top=",top,"&",sep="") else "" 
    startString  <- if(!is.null(start)) paste("start=",start,"&",sep="") else ""
    endString <- if(!is.null(end)) paste("end=",end,"&",sep="") else ""

    openString <- if(!is.null(open)) paste("open=",tolower(as.character(open)),"&",sep="") else ""
    
    url = paste(Config.RacmApiURL,"/jobm/rest/jobs?",sep="")
    if(type=='rdb'){
      url = paste(Config.RacmApiURL,"/jobm/rest/rdbjobs?",sep="")
    }
    
    if(type=='docker'){
      url = paste(Config.RacmApiURL,"/jobm/rest/dockerjobs?",sep="")
    }

    url = paste(url,topString,startString,endString,openString,"TaskName=",taskName,sep="")
    
    
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

Jobs.getDockerJobsListQuick <- function(top=10, open=NULL, start=NULL, end=NULL, labelReg=NULL){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Jobs.getJobList"
    }else{
      taskName = "SciScript-R.Jobs.getJobList"
    }
    
    topString <- if(!is.null(top)) paste("top=",top,"&",sep="") else "" 
    startString  <- if(!is.null(start)) paste("start=",start,"&",sep="") else ""
    endString <- if(!is.null(end)) paste("end=",end,"&",sep="") else ""
    
    openString <- if(!is.null(open)) paste("open=",tolower(as.character(open)),"&",sep="") else ""
    labRegString <- if(!is.null(labelReg)) paste("labelReg=",labelReg,"&",sep="") else ""
    
    
    url = paste(Config.RacmApiURL,"/jobm/rest/dockerjobs/quick?",sep="")
    url = paste(url,topString,startString,endString,labRegString,openString,"TaskName=",taskName,sep="")

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

Jobs.getJobQueues <- function(format="dataframe"){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Jobs.getJobQueues"
    }else{
      taskName = "SciScript-R.Jobs.getJobQueues"
    }
    
    url = paste(Config.RacmApiURL,"/jobm/rest/jobs/queues?","TaskName=",taskName,sep="")

    r = GET(url,add_headers('X-Auth-Token'=token),accept("application/json"))
    
    if(r$status_code != 200) {
      stop(paste("Error when getting job queues from JOBM API.\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      
      if(format == "dataframe"){
        r = fromJSON(content(r, "text", encoding="UTF-8"))
        df <- data.frame(r$rows)
        colnames(df) <- r$columns
        return(df)
      }else{
        return(content(r))  
      }
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

Jobs.getJobDescription <- function(jobId){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Jobs.getJobDescription"
    }else{
      taskName = "SciScript-R.Jobs.getJobDescription"
    }
    
    
    url = paste(Config.RacmApiURL,"/jobm/rest/jobs/", toString(jobId),"?TaskName=",taskName,sep="")
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

Jobs.getJobStatus <- function(jobId){
  
  intStatus = Jobs.getJobDescription(jobId)$status
  
  if(intStatus == 1){
    return(list(status=intStatus, statusMeaning="PENDING", jobId=jobId))
  }else if(intStatus == 2){
    return(list(status=intStatus, statusMeaning="QUEUED", jobId=jobId))
  }else if(intStatus == 4){
    return(list(status=intStatus, statusMeaning="ACCEPTED", jobId=jobId))
  }else if(intStatus == 8){
    return(list(status=intStatus, statusMeaning="STARTED", jobId=jobId))
  }else if(intStatus == 16){
    return(list(status=intStatus, statusMeaning="FINISHED", jobId=jobId))
  }else if(intStatus == 32){
    return(list(status=intStatus, statusMeaning="SUCCESS", jobId=jobId))
  }else if(intStatus == 64){
    return(list(status=intStatus, statusMeaning="ERROR", jobId=jobId))
  }else if(intStatus == 128){
    return(list(status=intStatus, statusMeaning="CANCELED", jobId=jobId))
  }else{
    stop("Invalid integer value given to job status.")
  }
}


Jobs.submitNotebookJob <- function(notebookPath, dockerComputeDomain=NULL, dockerImageName=NULL, userVolumes=NULL,  dataVolumes=NULL, resultsFolderPath="", parameters="", jobAlias= ""){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Jobs.submitNotebookJob"
    }else{
      taskName = "SciScript-R.Jobs.submitNotebookJob"
    }
    
    if(is.null(dockerComputeDomain)){
      dockerComputeDomains = getDockerComputeDomains();
      if(length(dockerComputeDomains) > 0){
        dockerComputeDomain = dockerComputeDomains[0];
      }else{
          stop("There are no dockerComputeDomains available for the user.")
      }
    }
    
    if(is.null(dockerImageName)){
      images = dockerComputeDomain$images
      if(length(images) > 0){
        dockerImageName = images[[1]]$name;
      }else{
        stop("dockerComputeDomain has no docker images available for the user.")
      }
    }
    
    uVols = list();
    if(is.null(userVolumes)){
      for(i in 1:length(dockerComputeDomain$userVolumes)){
        vol = dockerComputeDomain$userVolumes[[i]]
        if("write" %in% dockerComputeDomain$userVolumes[[i]]$allowedActions){
          uVols[[length(uVols)+1]] <- list(userVolumeId=dockerComputeDomain$userVolumes[[i]]$id, needsWriteAccess=TRUE)
        }else{
          uVols[[length(uVols)+1]] <- list(userVolumeId=dockerComputeDomain$userVolumes[[i]]$id, needsWriteAccess=FALSE)
        }
      }
    }else{
      
      for(i in 1:length(userVolumes)){
        uVol =  userVolumes[[i]]
        found = FALSE;
        for(j in 1:length(dockerComputeDomain$userVolumes)){
          vol = dockerComputeDomain$userVolumes[[j]]
          if(uVol$name == vol$name && uVol$rootVolumeName == vol$rootVolumeName && uVol$owner == vol$owner){
            
            found = TRUE
            if(!is.null(uVol$needsWriteAccess)){

              if(uVol$needsWriteAccess == TRUE && ('write' %in% vol$allowedActions) ){
                uVols[[length(uVols)+1]] <- list(userVolumeId= vol$id, needsWriteAccess= TRUE)
              }else{
                uVols[[length(uVols)+1]] <- list(userVolumeId= vol$id, needsWriteAccess= FALSE)
              }

            }else{

              if('write' %in% vol$allowedActions ){
                uVols[[length(uVols)+1]] <- list(userVolumeId= vol$id, needsWriteAccess= TRUE)
              }else{
                uVols[[length(uVols)+1]] <- list(userVolumeId= vol$id, needsWriteAccess= FALSE)
              }
              
            }
          }
        }
        if(!found){
          stop(paste("User volume '", uVol$name,  "' not found within Compute domain",sep=""))
        }
      
      }
    }
    
    dataVols = list()
    if( is.null(dataVolumes)){
      for( i in 1:length(dockerComputeDomain$volumes)){
        vol = dockerComputeDomain$volumes[[i]]
        if(vol$writable){
          dataVols[[length(dataVols)+1]] <- list(id=vol$id, name= vol$name, writable=TRUE)
        }else{
          dataVols[[length(dataVols)+1]] <- list(id=vol$id, name= vol$name, writable=FALSE)
        }
      }
    }else{
      for(i in 1:length(dataVolumes)){
        dVol = dataVolumes[[i]]
        found = FALSE;
        for(j in 1: length(dockerComputeDomain$volumes)){
          vol = dockerComputeDomain$volumes[[j]]
          if( vol$name == dVol$name ){
            found = TRUE;
            if(!is.null(dVol$needsWriteAccess)){
              
              if(dVol$needsWriteAccess == TRUE){
                dataVols[[length(dataVols)+1]] <- list(id= vol$id, name=vol$name, writable= TRUE)
              }else{
                dataVols[[length(dataVols)+1]] <- list(id= vol$id, name=vol$name, writable= FALSE)
              }
              
            }else{
              
              if(vol$writable){
                dataVols[[length(dataVols)+1]] <- list(id= vol$id, name=vol$name, writable= TRUE)
              }else{
                dataVols[[length(dataVols)+1]] <- list(id= vol$id, name=vol$name, writable= FALSE)
              }
              
            }
            
          }
        }
        
        if(!found){
          stop(paste("Data volume '", dVol$name, "' not found within Compute domain",sep=""))
        }
      }
    }
    
    dockerComputeEndpoint = dockerComputeDomain$apiEndpoint;
    
    dockerJobModel = list(
        command= parameters,
        scriptURI= notebookPath,
        submitterDID= jobAlias,
        dockerComputeEndpoint= dockerComputeEndpoint,
        dockerImageName= dockerImageName,
        resultsFolderURI= resultsFolderPath,
        volumeContainers= dataVols,
        userVolumes= uVols
      )
    
    #cat(toJSON(dockerJobModel))
    url = paste(Config.RacmApiURL,"/jobm/rest/jobs/docker?TaskName=",taskName,sep="")
    r = POST(url,add_headers('X-Auth-Token'=token),body = dockerJobModel, content_type_json(), encode="json", accept("application/json"))

    if(r$status_code != 200) {
      stop(paste("Error when submitting a job to the JOBM API.\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r)$id)
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}

Jobs.submitShellCommandJob <- function(shellCommand, dockerComputeDomain = NULL, dockerImageName = NULL, userVolumes = NULL, dataVolumes = NULL, resultsFolderPath = "", jobAlias = ""){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Jobs.submitShellCommandJob"
    }else{
      taskName = "SciScript-R.Jobs.submitShellCommandJob"
    }
    
    if(is.null(dockerComputeDomain)){
      dockerComputeDomains = getDockerComputeDomains();
      if(length(dockerComputeDomains) > 0){
        dockerComputeDomain = dockerComputeDomains[0];
      }else{
        stop("There are no dockerComputeDomains available for the user.")
      }
    }
    
    if(is.null(dockerImageName)){
      images = dockerComputeDomain$images
      if(length(images) > 0){
        dockerImageName = images[[1]]$name;
      }else{
        stop("dockerComputeDomain has no docker images available for the user.")
      }
    }
    
    uVols = list();
    if(is.null(userVolumes)){
      for(i in 1:length(dockerComputeDomain$userVolumes)){
        vol = dockerComputeDomain$userVolumes[[i]]
        if("write" %in% dockerComputeDomain$userVolumes[[i]]$allowedActions){
          uVols[[length(uVols)+1]] <- list(userVolumeId=dockerComputeDomain$userVolumes[[i]]$id, needsWriteAccess=TRUE)
        }else{
          uVols[[length(uVols)+1]] <- list(userVolumeId=dockerComputeDomain$userVolumes[[i]]$id, needsWriteAccess=FALSE)
        }
      }
    }else{
      
      for(i in 1:length(userVolumes)){
        uVol =  userVolumes[[i]]
        found = FALSE;
        for(j in 1:length(dockerComputeDomain$userVolumes)){
          vol = dockerComputeDomain$userVolumes[[j]]
          if(uVol$name == vol$name && uVol$rootVolumeName == vol$rootVolumeName && uVol$owner == vol$owner){
            
            found = TRUE
            if(!is.null(uVol$needsWriteAccess)){
              
              if(uVol$needsWriteAccess == TRUE && ('write' %in% vol$allowedActions) ){
                uVols[[length(uVols)+1]] <- list(userVolumeId= vol$id, needsWriteAccess= TRUE)
              }else{
                uVols[[length(uVols)+1]] <- list(userVolumeId= vol$id, needsWriteAccess= FALSE)
              }
              
            }else{
              
              if('write' %in% vol$allowedActions ){
                uVols[[length(uVols)+1]] <- list(userVolumeId= vol$id, needsWriteAccess= TRUE)
              }else{
                uVols[[length(uVols)+1]] <- list(userVolumeId= vol$id, needsWriteAccess= FALSE)
              }
              
            }
          }
        }
        if(!found){
          stop(paste("User volume '", uVol$name,  "' not found within Compute domain",sep=""))
        }
        
      }
    }
    
    dataVols = list()
    if( is.null(dataVolumes)){
      for( i in 1:length(dockerComputeDomain$volumes)){
        vol = dockerComputeDomain$volumes[[i]]
        if(vol$writable){
          dataVols[[length(dataVols)+1]] <- list(id=vol$id, name= vol$name, writable=TRUE)
        }else{
          dataVols[[length(dataVols)+1]] <- list(id=vol$id, name= vol$name, writable=FALSE)
        }
      }
    }else{
      for(i in 1:length(dataVolumes)){
        dVol = dataVolumes[[i]]
        found = FALSE;
        for(j in 1: length(dockerComputeDomain$volumes)){
          vol = dockerComputeDomain$volumes[[j]]
          if( vol$name == dVol$name ){
            found = TRUE;
            if(!is.null(dVol$needsWriteAccess)){
              
              if(dVol$needsWriteAccess == TRUE){
                dataVols[[length(dataVols)+1]] <- list(id= vol$id, name=vol$name, writable= TRUE)
              }else{
                dataVols[[length(dataVols)+1]] <- list(id= vol$id, name=vol$name, writable= FALSE)
              }

            }else{
              
              if(vol$writable){
                dataVols[[length(dataVols)+1]] <- list(id= vol$id, name=vol$name, writable= TRUE)
              }else{
                dataVols[[length(dataVols)+1]] <- list(id= vol$id, name=vol$name, writable= FALSE)
              }
              
            }
            
          }
        }
        
        if(!found){
          stop(paste("Data volume '", dVol$name, "' not found within Compute domain",sep=""))
        }
      }
    }
    
    dockerComputeEndpoint = dockerComputeDomain$apiEndpoint;
    
    dockerJobModel = list(
      command= shellCommand,
      submitterDID= jobAlias,
      dockerComputeEndpoint= dockerComputeEndpoint,
      dockerImageName= dockerImageName,
      volumeContainers= dataVols,
      userVolumes= uVols,
      resultsFolderURI= resultsFolderPath
    )
    
    
    url = paste(Config.RacmApiURL,"/jobm/rest/jobs/docker?TaskName=",taskName,sep="")
    r = POST(url,add_headers('X-Auth-Token'=token),body = dockerJobModel, content_type_json(), encode="json", accept("application/json"))

    if(r$status_code != 200) {
      stop(paste("Error when submitting a job to the JOBM API.\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r)$id)
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}


Jobs.submitRDBQueryJob <- function(sqlQuery, rdbComputeDomain=NULL, databaseContextName = NULL, resultsName='queryResults', resultsFolderPath="", jobAlias = ""){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Jobs.submitRDBQueryJob"
    }else{
      taskName = "SciScript-R.Jobs.submitRDBQueryJob"
    }
    
    if(is.null(rdbComputeDomain)){
      rdbComputeDomains = getRDBComputeDomains();
      if(length(rdbComputeDomains) > 0){
        rdbComputeDomain = rdbComputeDomains[0];
      }else{
        stop("There are no rdbComputeDomains available for the user.")
      }
    }
    
    if(is.null(databaseContextName)){
      databaseContexts = rdbComputeDomain$databaseContexts
      if(length(databaseContexts) > 0){
        databaseContextName = rdbComputeDomain$databaseContexts[[1]]$name;
      }else{
        stop("rbdComputeDomain has no database contexts available for the user.")
      }
    }
    
    
    targets = list();
    if(typeof(resultsName) == "character"){
      targets[[length(targets)+1]] <- list(location= resultsName, type= 'FILE_CSV', resultNumber= 1);
      
    }
    else if(typeof(resultsName) == "list"){
    #  if(length(set(resultsName)) != length(resultsName))
    #    stop("Elements of parameter 'resultsName' must be unique");
    #}
      for( i in 1:length(resultsName)){
        if(typeof(resultsName[[i]]) == "character")
          targets[[length(targets)+1]] <- list(location= resultsName[[i]], type= 'FILE_CSV', resultNumber= i)
        else
          stop("Elements of array 'resultsName' are not strings");
      }
    }else{
        stop("Type of parameter 'resultsName' is not supported");
    }
    
    rdbDomainId = rdbComputeDomain$id
    
    dockerJobModel = list(
      inputSql= sqlQuery,
      submitterDID= jobAlias,
      databaseContextName= databaseContextName,
      rdbDomainId= rdbDomainId,
      targets= targets,
      resultsFolderURI= resultsFolderPath
      )
    
    
    url = paste(Config.RacmApiURL,"/jobm/rest/jobs/rdb?TaskName=",taskName,sep="")
    r = POST(url,add_headers('X-Auth-Token'=token),body = dockerJobModel, content_type_json(), encode="json", accept("application/json"))
    
    if(r$status_code != 200) {
      stop(paste("Error when submitting a job to the JOBM API.\nHttp Response from JOBM API returned status code ", r$status_code, ":\n",  content(r, as="text", encoding="UTF-8")))
    } else {
      return(content(r)$id)
    }
  }else{
    stop(paste("User token is not defined. First log into SciServer."))
  }
}




Jobs.cancelJob <- function(jobId){
  
  token = Authentication.getToken()
  if(!is.null(token) && token != "")
  {
    
    if(Config.isSciServerComputeEnvironment()){
      taskName = "Compute.SciScript-R.Jobs.cancelJob"
    }else{
      taskName = "SciScript-R.Jobs.cancelJob"
    }

    url = paste(Config.RacmApiURL,"/jobm/rest/jobs/",toString(jobId),"/cancel?TaskName=",taskName, sep="")
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

Jobs.waitForJob<-function(jobId, verbose=TRUE, pollTime = 5){
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
      jobDesc = Jobs.getJobDescription(jobId)
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
