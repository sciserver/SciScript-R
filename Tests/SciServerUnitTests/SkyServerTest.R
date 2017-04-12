library(SciServer)
library(jpeg)

token1 = Authentication.login(Authentication_loginName, Authentication_loginPassword)


test_SkyServer_sqlSearch <- function(){
  
  df = SkyServer.sqlSearch(sql=SkyServer_TestQuery, dataRelease=SkyServer_DataRelease)
  dfString = capture.output(write.csv(df,row.names=FALSE,quote=FALSE))
  dfString = paste(paste(dfString,collapse="\n"),"\n",sep="")
  checkTrue(dfString == SkyServer_QueryResultCSV)
}

test_SkyServer_getJpegImgCutout <- function(){
  
  img = SkyServer.getJpegImgCutout(ra=197.614455642896, dec=18.438168853724, width=512, height=512, scale=0.4, dataRelease=SkyServer_DataRelease,opt="OG",query="SELECT TOP 100 p.objID, p.ra, p.dec, p.r FROM fGetObjFromRectEq(197.6,18.4,197.7,18.5) n, PhotoPrimary p WHERE n.objID=p.objID")
  im = readJPEG("TestGalaxy.jpeg")
  checkTrue(all.equal(img,im))
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