library(SciServer) 


#==================================================================================================================================
# AUTHENTICATION
#==================================================================================================================================

test.Authentication_allMethods<- function(){
  
  newToken1 = "myToken1"
  
  token1 = Authentication.login(Authentication_loginName, Authentication_loginPassword);
  token2 = Authentication.getToken()
  checkEquals(token1, token2)
  token4 = Authentication.token
  checkEquals(token1, token4)
  
  user = Authentication.getKeystoneUserWithToken(token1)
  checkEquals(Authentication_loginName, user$userName)
  
  Authentication.setToken(newToken1)
  checkEquals(newToken1, Authentication.getToken())
  
  token1 = Authentication.login(Authentication_loginName, Authentication_loginPassword);
}

#==================================================================================================================================
# LOGINPORTAL
#==================================================================================================================================

test.LoginPortal_allMethods<- function(){
  
  newToken1 = "myToken1"
  
  token1 = LoginPortal.login(Authentication_loginName, Authentication_loginPassword);
  token2 = LoginPortal.getToken()
  checkEquals(token1, token2)
  
  user = LoginPortal.getKeystoneUserWithToken(token1)
  checkEquals(Authentication_loginName, user$userName)
  
  LoginPortal.setToken(newToken1)
  checkEquals(newToken1, LoginPortal.getToken())
  
  token1 = LoginPortal.login(Authentication_loginName, Authentication_loginPassword);
}


#==================================================================================================================================
# CASJOBS
#==================================================================================================================================


test.CasJobs_getSchemaName<-function(){
  
  casJobsId = CasJobs.getSchemaName()
  checkTrue(casJobsId != "")
}

test.CasJobs_getTables<-function(){
  
  tables = CasJobs.getTables(context="MyDB")
}

test.CasJobs_executeQuery<-function(){
  
  df = CasJobs.executeQuery(sql=CasJobs_TestQuery, context=CasJobs_TestDatabase, format="dataframe")
  dfString = capture.output(write.csv(df,row.names=FALSE,quote=FALSE))
  dfString = paste(paste(dfString,collapse="\n"),"\n",sep="")
  checkTrue(dfString == CasJobs_TestTableCSV)
}

test.CasJobs_submitJob<-function(){
  
  res = try(CasJobs.executeQuery(sql=paste("DROP TABLE ", CasJobs_TestTableName1, sep=""), context="MyDB", format="dataframe"), silent=TRUE)
  
  jobId = CasJobs.submitJob(sql=paste(CasJobs_TestQuery, " into MyDB.", CasJobs_TestTableName1, sep=""), context="MyDB")
  checkTrue(jobId != "")
  jobDescription = CasJobs.waitForJob(jobId=jobId, verbose=TRUE)
  df = CasJobs.executeQuery(sql=paste("DROP TABLE ", CasJobs_TestTableName1, sep=""), context="MyDB", format="dataframe")
}

test.CasJobs_getJobStatus<-function(){
  
  jobId = CasJobs.submitJob(sql=CasJobs_TestQuery, context=CasJobs_TestDatabase)
  jobDescription = CasJobs.getJobStatus(jobId)
  checkTrue(jobDescription$JobID == jobId)
}

test.CasJobs_cancelJob<-function(){
  
  jobId = CasJobs.submitJob(sql=CasJobs_TestQuery, context=CasJobs_TestDatabase)
  isCanceled = CasJobs.cancelJob(jobId=jobId)
  checkTrue(isCanceled)
}

test.CasJobs_waitForJob<-function(){
  
  jobId = CasJobs.submitJob(sql=CasJobs_TestQuery, context=CasJobs_TestDatabase)
  jobDescription = CasJobs.waitForJob(jobId=jobId, verbose=TRUE)
  checkTrue(jobDescription$Status >= 3)
}

test.CasJobs_writeFitsFileFromQuery<-function(){
  
  res = try(file.remove(CasJobs_TestFitsFile), silent=TRUE)
  
  didWrite = CasJobs.writeFitsFileFromQuery(fileName=CasJobs_TestFitsFile, queryString=CasJobs_TestQuery, context="MyDB")
  checkTrue(didWrite)
  checkTrue(file.exists(CasJobs_TestFitsFile))
  file.remove(CasJobs_TestFitsFile)
}

test.CasJobs_uploadDataFrameToTable_uploadCSVDataToTable<-function(){
  
  res = try(CasJobs.executeQuery(sql=paste("DROP TABLE ", CasJobs_TestTableName2, sep=""), context="MyDB", format="dataframe"),silent=TRUE)
  
  
  df = read.csv(textConnection(CasJobs_TestTableCSV))
  
  result = CasJobs.uploadDataFrameToTable(df, tableName=CasJobs_TestTableName2, context="MyDB")
  checkTrue(result)
  table = CasJobs.executeQuery(sql=paste("select * from ", CasJobs_TestTableName2, sep=""), context="MyDB", format="dataframe")
  checkEquals(df,table)
  result2 = CasJobs.executeQuery(sql=paste("DROP TABLE ", CasJobs_TestTableName2, sep=""), context="MyDB", format="dataframe")
  
  result = CasJobs.uploadCSVDataToTable(csv=CasJobs_TestTableCSV, tableName=CasJobs_TestTableName2, context="MyDB")
  checkTrue(result)
  df2 = CasJobs.executeQuery(sql=paste("select * from ", CasJobs_TestTableName2, sep=""), context="MyDB", format="dataframe")
  checkEquals(df,df2)
  result2 = CasJobs.executeQuery(sql=paste("DROP TABLE ", CasJobs_TestTableName2, sep=""), context="MyDB", format="dataframe")
}


#==================================================================================================================================
# SKYSERVER
#==================================================================================================================================

test_SkyServer_sqlSearch <- function(){
  
  df = SkyServer.sqlSearch(sql=SkyServer_TestQuery, dataRelease=SkyServer_DataRelease)
  dfString = capture.output(write.csv(df,row.names=FALSE,quote=FALSE))
  dfString = paste(paste(dfString,collapse="\n"),"\n",sep="")
  checkTrue(dfString == SkyServer_QueryResultCSV)
}

test_SkyServer_getJpegImgCutout <- function(){
  
  img = SkyServer.getJpegImgCutout(ra=197.614455642896, dec=18.438168853724, width=50, height=50, scale=0.4, dataRelease=SkyServer_DataRelease,opt="OG",query="SELECT TOP 100 p.objID, p.ra, p.dec, p.r FROM fGetObjFromRectEq(197.6,18.4,197.7,18.5) n, PhotoPrimary p WHERE n.objID=p.objID")
  checkTrue(dim(img)[1] == SkyServer_ImageSideLength)
}

test_SkyServer_radialSearch <- function(){
  
  df = SkyServer.radialSearch(ra=258.25, dec=64.05, radius=0.1, dataRelease=SkyServer_DataRelease)
  dfString = capture.output(write.csv(df,row.names=FALSE,quote=FALSE))
  dfString = paste(paste(dfString,collapse="\n"),"\n",sep="")
  #checkTrue(dfString == SkyServer_RadialSearchResultCSV)
  #checkTrue(as.int64(SkyServer_RadialSearchResultObjID), as.int64(df$objid))
  checkTrue(dim(df)[2] == SkyServer_RadialSearchResultNumColumns)
}

test_SkyServer_rectangularSearch <- function(){
  #rectangular search
  df = SkyServer.rectangularSearch(min_ra=258.3, max_ra=258.31, min_dec=64,max_dec=64.01, dataRelease=SkyServer_DataRelease)
  dfString = capture.output(write.csv(df,row.names=FALSE,quote=FALSE))
  dfString = paste(paste(dfString,collapse="\n"),"\n",sep="")
  #checkTrue(dfString == SkyServer_RectangularSearchResultCSV)
  #checkTrue(SkyServer_RectangularSearchResultObjID, df$objid)
  checkTrue(dim(df)[2] == SkyServer_RectangularSearchResultNumColumns)
}

test_SkyServer_objectSearch <- function(){
  
  object = SkyServer.objectSearch(ra=258.25, dec=64.05, dataRelease=SkyServer_DataRelease)
  checkTrue(SkyServer_ObjectSearchResultObjID == object[1][[1]]$Rows[[1]]$id)
}

#==================================================================================================================================
# SCIDRIVE
#==================================================================================================================================

test.SciDrive_createContainer_directoryList_delete <- function(){
  
  res = try(SciDrive.createContainer(SciDrive_Directory), silent = TRUE)
  
  responseCreate = SciDrive.createContainer(SciDrive_Directory)
  checkTrue(responseCreate == TRUE)
  dirList = SciDrive.directoryList(SciDrive_Directory)
  checkTrue(grepl(SciDrive_Directory,dirList$path))
  responseDelete = SciDrive.delete(SciDrive_Directory)
  checkTrue(responseDelete == TRUE)
}

test_SciDrive_publicUrl <-function(){
  
  res = try(SciDrive.createContainer(SciDrive_Directory), silent = TRUE)
  
  responseCreate = SciDrive.createContainer(SciDrive_Directory)
  checkTrue(responseCreate == TRUE)
  url = SciDrive.publicUrl(SciDrive_Directory)
  checkTrue(TRUE == startsWith(url, "http"))
  responseDelete = SciDrive.delete(SciDrive_Directory)
  checkTrue(responseDelete == TRUE)
}

test.SciDrive_upload_download_delete <- function(){
  
  res = try(file.remove(SciDrive_FileName), silent = TRUE)
  
  file = file(SciDrive_FileName)
  writeLines(SciDrive_FileContent, file, sep="")
  close(file)
  
  responseUpload = SciDrive.upload(path=paste(SciDrive_Directory, "/", SciDrive_FileName, sep=""), localFilePath=SciDrive_FilePath)
  checkTrue(responseUpload$path == paste(SciDrive_Directory, "/", SciDrive_FileName, sep=""))
  fileContent = SciDrive.download(paste(SciDrive_Directory, "/", SciDrive_FileName, sep=""), format="text")
  checkTrue(fileContent == SciDrive_FileContent)
  responseDelete = SciDrive.delete(SciDrive_Directory)
  checkTrue(responseDelete == TRUE)
  file.remove(SciDrive_FileName)
}


#==================================================================================================================================
# SKYQUERY
#==================================================================================================================================

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
