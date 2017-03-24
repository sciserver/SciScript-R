library(SciServer) 

token1 = Authentication.login(Authentication_loginName, Authentication_loginPassword)


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