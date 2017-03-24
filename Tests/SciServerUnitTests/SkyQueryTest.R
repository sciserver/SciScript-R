library(SciServer) 

token1 = Authentication.login(Authentication_loginName, Authentication_loginPassword);

test.SkyQuery_listQueues <-function(){
  queueList = SkyQuery.listQueues()
} 

test.SkyQuery_getQueueInfo <-function(){
  queueInfo = SkyQuery.getQueueInfo('quick')
  queueInfo = SkyQuery.getQueueInfo('long')
}
  
test.SkyQuery_submitJob <-function(){
  jobId = SkyQuery.submitJob(query=SkyQuery_Query, queue="quick")
  checkTrue(jobId != "")
}

test.SkyQuery_getJobStatus <-function(){
  jobId = SkyQuery.submitJob(query=SkyQuery_Query, queue="quick")
  jobDescription = SkyQuery.getJobStatus(jobId=jobId)
}
  
test.SkyQuery_waitForJob <-function(){
  jobId = SkyQuery.submitJob(query=SkyQuery_Query, queue="quick")
  jobDescription = SkyQuery.waitForJob(jobId=jobId, verbose=TRUE)
  checkTrue(jobDescription$status == "completed")
}

test.SkyQuery_cancelJob <-function(){
  isCanceled = SkyQuery.cancelJob(SkyQuery.submitJob(query=SkyQuery_Query, queue="long"))
  checkTrue(isCanceled, TRUE)
}

test.SkyQuery_uploadTable_getTable_getTableInfo_listTableColumns_dropTable <-function(){

  res = try(SkyQuery.dropTable(tableName=SkyQuery_TestTableName, datasetName="MyDB"), silent=TRUE)
  
  result = SkyQuery.uploadTable(uploadData=SkyQuery_TestTableCSV, tableName=SkyQuery_TestTableName, datasetName="MyDB", format="csv")
  checkTrue(result, TRUE)
  
  df = SkyQuery.getTable(tableName=SkyQuery_TestTableName, datasetName="MyDB", top=10)
  dfString = capture.output(write.csv(df,row.names=FALSE,quote=FALSE))
  dfString = paste(paste(dfString,collapse="\n"),"\n",sep="")
  checkTrue(SkyQuery_TestTableCSVdownloaded == dfString)
  
  info = SkyQuery.getTableInfo(tableName=paste("webuser.",SkyQuery_TestTableName, sep=""), datasetName="MyDB")
  columns = SkyQuery.listTableColumns(tableName=paste("webuser.", SkyQuery_TestTableName, sep=""), datasetName="MyDB")
  
  result = SkyQuery.dropTable(tableName=SkyQuery_TestTableName, datasetName="MyDB");
  checkTrue(result, TRUE)
}

test.SkyQuery_listJobs <-function(){
  
  longJobsList = SkyQuery.listJobs('quick')
  jobsList = SkyQuery.listJobs('long')
}  

test.SkyQuery_listAllDatasets <-function(){
    datasets = SkyQuery.listAllDatasets()
}

test.SkyQuery_getDatasetInfo <-function(){
    info = SkyQuery.getDatasetInfo("MyDB")
}
  
test.SkyQuery_listDatasetTables <-function(){
    tables = SkyQuery.listDatasetTables("MyDB")
}
