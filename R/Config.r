#The SciServer.Config module contains important parameters for the correct functioning of the SciServer library.
#Although these parameters must be set/defined by the admin or user before the installation of the library, they can also be accessed and changed on-the-fly while on the R session.\n

#Config.CasJobsRESTUri: defines the base URL of the CasJobs web API (string).
Config.CasJobsRESTUri = "http://skyserver.sdss.org/CasJobs/RestApi"
#Config.AuthenticationURL: defines the base URL of the CasJobs web service API
Config.AuthenticationURL = "http://portal.sciserver.org/login-portal/keystone/v3/tokens"
#Config.SciDriveHost: defines the base URL of the SciDrive web service API 
Config.SciDriveHost = 'http://www.scidrive.org'
#Config.SkyQueryUrl**: defines the base URL of the SkyQuery web service API 
Config.SkyQueryUrl = 'http://voservices.net/skyquery/Api/V1'
#Config.SkyServerWSurl: defines the base URL of the SkyServer web service API 
Config.SkyServerWSurl = 'http://skyserver.sdss.org'
#Config.DataRelease: defines the SDSS data release, to be used to build the full SkyServer API url along with Config.SkyServerWSurl
Config.DataRelease = 'DR13' # SDSS data release. E.g., DR13
#Config.KeystoneTokenPath: defines the local path (string) to the file containing the user's authentication token in the SciServer-Compute environment
Config.KeystoneTokenFilePath =  "/home/idies/keystone.token" #this path to the file containing the user's keystone token is hardcoded in the sciserver-compute environment
#Config.version: defines the SciServer release tag, to which this package belongs
Config.Version = "sciserver-v1.10.0" #sciserver release version
#Config.ComputeJobDirectoryFile: defines the path to the file in the "Docker job container" that shows the directory path where the asynchronous compute job is being executed.
Config.ComputeJobDirectoryFile = "/home/idies/jobs.path" 
#Config.RacmApiURL: defines the base URL of the multiple APIs in RACM
Config.RacmApiURL = 'http://scitest12.pha.jhu.edu/racm';



# returns TRUE if the library is running inside the SciServer-Compute, and FALSE if not
Config.isSciServerComputeEnvironment<-function()
{
  if(file.exists(Config.KeystoneTokenFilePath)){
    return (TRUE)
  }else{
    return (FALSE)
  }
}