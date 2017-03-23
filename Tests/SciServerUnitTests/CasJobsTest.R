library(SciServer) 





  token1 = Authentication.login(Authentication_loginName, Authentication_loginPassword);
  
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
    jobId = CasJobs.submitJob(sql=paste(CasJobs_TestQuery, " into MyDB.", CasJobs_TestTableName1, sep=""), context="MyDB")
    jobDescription = CasJobs.waitForJob(jobId=jobId, verbose=FALSE)
    df = CasJobs.executeQuery(sql=paste("DROP TABLE ", CasJobs_TestTableName1, sep=""), context="MyDB", format="dataframe")
    checkTrue(jobId != "")
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
    
    didWrite = CasJobs.writeFitsFileFromQuery(fileName=CasJobs_TestFitsFile, queryString=CasJobs_TestQuery, context="MyDB")
    checkTrue(didWrite)
    checkTrue(file.exists(CasJobs_TestFitsFile))
    file.remove(CasJobs_TestFitsFile)
  }
  
  test.CasJobs_uploadPandasDataFrameToTable_uploadCSVDataToTable<-function(){
    
    df = read.csv(textConnection(CasJobs_TestTableCSV))
  
    result = CasJobs.uploadDataFrameToTable(df, tableName=CasJobs_TestTableName2, context="MyDB")
    table = CasJobs.executeQuery(sql=paste("select * from ", CasJobs_TestTableName2, sep=""), context="MyDB", format="dataframe")
    result2 = CasJobs.executeQuery(sql=paste("DROP TABLE ", CasJobs_TestTableName2, sep=""), context="MyDB", format="dataframe")
    checkTrue(result)
    checkEquals(df,table)
    
    result = CasJobs.uploadCSVDataToTable(csv=CasJobs_TestTableCSV, tableName=CasJobs_TestTableName2, context="MyDB")
    df2 = CasJobs.executeQuery(sql=paste("select * from ", CasJobs_TestTableName2, sep=""), context="MyDB", format="dataframe")
    result2 = CasJobs.executeQuery(sql=paste("DROP TABLE ", CasJobs_TestTableName2, sep=""), context=CasJobs_TestDatabase, format="dataframe")    
    checkTrue(result)
    checkEquals(df,df2)
  }
  
