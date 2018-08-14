library(SciServer) 

token1 = Authentication.login(Authentication_loginName, Authentication_loginPassword);

test.ComputeJobs_getComputeDomains<-function(){
  
  domains = ComputeJobs.getComputeDomains(batch=True, interactive=False)
  checkTrue(  domains[0]["apiEndpoint"] != "" &&  !is.null(domains[0]["apiEndpoint"])  )
  
}

test.ComputeJobs_submitNotebookJob_getJobsList_getJobDescription_waitForJob_getJobStatus_submitShellCommandJob_cancelJob<-function(){
  
  res = try(FileService.upload(path=ComputeJobs_RemoteNotebookPath,localFilePath=ComputeJobs_LocalNotebookPath), silent = TRUE)

  domains = ComputeJobs.getComputeDomains(batch=TRUE, interactive=FALSE)
  checkTrue( domains[0]['id'] > 0 )
  
  domainIndex = NULL
  imageIndex = NULL
  try(
    for(i in 1:length(domains)){
      images = domains[i]['images']
      for(j in 1:length(images)){
        if( grepl(ComputeJobs_ImageNameLike,toLower(toString(images[j]['name']))) || grepl(ComputeJobs_ImageNameLike,toLower(toString(images[j]['description']))) ){
          domainIndex = i
          imageIndex = j
          stop("")
        }
      }
    }
  , silet = TRUE)

  domainIndex = 2
  imageIndex = 1
  
  domainApiEndpoint = domains[domainIndex]['apiEndpoint']
  dockerImage = domains[domainIndex]['images'][imageIndex]['name']
  
  volumes = list();
  if( length(domains[domainIndex]['volumes']) > 0){
    volumes = list(name = domains[domainIndex]['volumes'][0]['name'])
  }
  
  notebookPath = ComputeJobs_RemoteNotebookPath
  parameters = ComputeJobs_Parameters
  jobAlias = ComputeJobs_JobAlias
  
  job = ComputeJobs.submitNotebookJob(domainApiEndpoint=domainApiEndpoint, notebookPath=notebookPath, dockerImage=dockerImage, volumes=volumes, parameters=parameters, jobAlias=jobAlias)
  checkTrue(job['scriptURI'] == ComputeJobs_RemoteNotebookPath)
  checkTrue(job['dockerImageName'] == dockerImage)
  checkTrue(grepl(job['dockerComputeEndpoint'], domainApiEndpoint))
  checkTrue(job['submitterDID'] == jobAlias)
  checkTrue(job['command'] == parameters)
  checkTrue(job['status'] >= 1)
  
  jobList = ComputeJobs.getJobsList();
  jobId = jobList[0]['id'];
  
  job = ComputeJobs.waitForJob(jobId=jobId, verbose=True, pollTime=2)
  checkTrue(jobList[0] == job)
  
  jobDesc = ComputeJobs.getJobDescription(jobId)
  checkTrue(jobDesc == job)
  
  status = ComputeJobs.getJobStatus(jobId=jobId)
  checkTrue(status == "SUCCESS")
  
  job = ComputeJobs.submitShellCommandJob(domainApiEndpoint=domainApiEndpoint, shellCommand=ComputeJobs_ShellCommand, dockerImage=dockerImage, volumes=volumes, jobAlias=jobAlias)
  checkTrue(job['dockerImageName'] == dockerImage)
  checkTrue(grepl(job['dockerComputeEndpoint'], domainApiEndpoint) )
  checkTrue(job['submitterDID'] == jobAlias)
  checkTrue(job['command'] == ComputeJobs_ShellCommand)
  checkTrue(job['status'] >= 1)

  job = ComputeJobs.submitNotebookJob(domainApiEndpoint=domainApiEndpoint, notebookPath=notebookPath, dockerImage=dockerImage, volumes=volumes, parameters=parameters, jobAlias=jobAlias)
  isCanceled = ComputeJobs.cancelJob(job['jobId'])
  checkTrue(isCanceled)
  status = ComputeJobs.getJobStatus(jobId=jobId)
  checkTrue(status == "CANCELED")
  
  res = try(FileService.delete(path=ComputeJobs_RemoteNotebookPath), silent = TRUE)
  
}

test.ComputeJobs_getJobDirectory <- function(){
  checkTrue(TRUE)
}
    




