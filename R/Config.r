Config.CasJobsRESTUri = "http://scitest02.pha.jhu.edu/CasJobs/RestApi"
Config.LoginPortalURL = "http://scitest02.pha.jhu.edu/login-portal/keystone/v3/tokens"
Config.SciDriveHost = 'http://scitest09.pha.jhu.edu'

Config.addSlash<-function(url){
  if(substr(url ,nchar(url ),nchar(url )) !='/'){
    url = paste(url,'/',sep='')
  }
  url
}