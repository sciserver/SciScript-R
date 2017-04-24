library(SciServer) 

token = Authentication.login(Authentication_loginName, Authentication_loginPassword)
  
test.FileService_createDir_getDirList_delete<-function(){

  res = try(FileService.delete(FileService_RemoteDirName), silent = TRUE)
  
  wasDirCreated = FileService.createDir(FileService_RemoteDirName)
  checkTrue(wasDirCreated)
    
  dirList = FileService.getDirList(FileService_RemoteDirName)
  checkTrue(grep(FileService_NewDirName, dirList$"path"))
    
  wasDirDeleted = FileService.delete(FileService_RemoteDirName)
  checkTrue(wasDirDeleted)
  
  res = try(FileService.delete(FileService_RemoteDirName), silent = TRUE)
}
  
test.FileService_FileService_upload_getDirList_download_delete<-function(){
  
  
  res = try(file.remove(FileService_NewFileName), silent = TRUE)
  
  file = file(FileService_NewFileName)
  writeLines(FileService_newFileContent, file, sep="")
  close(file)

  isUploaded = FileService.upload(path=FileService_RemoteFileName, localFilePath=FileService_NewFileName)
  dirList = FileService.getDirList(FileService_RemoteDirName)
  fileContent = FileService.download(path=FileService_RemoteFileName, format="text")
  isDeleted = FileService.delete(FileService_RemoteFileName)
  checkTrue(isUploaded)
  checkTrue(grep(FileService_NewFileName, dirList))
  checkTrue(fileContent == FileService_newFileContent)
  checkTrue(isDeleted)
      
  isUploaded = FileService.upload(path=FileService_RemoteFileName, data=FileService_newFileContent)
  dirList = FileService.getDirList(FileService_RemoteDirName)
  fileContent = FileService.download(path=FileService_RemoteFileName)
  isDeleted = FileService.delete(FileService_RemoteFileName)
  checkTrue(isUploaded)
  checkTrue(grep(FileService_NewFileName, dirList))
  checkTrue(fileContent == FileService_newFileContent)
  checkTrue(isDeleted)
  
  res = try(file.remove(FileService_NewFileName), silent = TRUE)
  
}
