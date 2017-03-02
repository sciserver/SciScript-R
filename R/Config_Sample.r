Config.CasJobsRESTUri = "http://skyserver.sdss.org/CasJobs/RestApi"
Config.AuthenticationURL = "http://portal.sciserver.org/login-portal/keystone/v3/tokens"
Config.SciDriveHost = 'http://www.scidrive.org'
Config.SkyQueryUrl = 'http://voservices.net/skyquery/Api/V1'
Config.DataRelease = 'DR13' # SDSS data release. E.g., DR13
Config.SkyServerWSurl = 'http://skyserver.sdss.org'
Config.Version = "sciserver-v1.9.3" #sciserver release version
Config.KeystoneTokenPath =  "/home/idies/keystone.token" #this path to the file containing the user's keystone token is hardcoded in the sciserver-compute environment


# returns TRUE if the library is running inside the SciServer-Compute, and FALSE if not
Config.isSciServerComputeEnvironment<-function()
{
  if(file.exists(Config.KeystoneTokenPath)){
    return (TRUE)
  }else{
    return (FALSE)
  }
}