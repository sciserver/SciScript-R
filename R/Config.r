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
Config.Version = "sciserver-v2.0.0" #sciserver release version
#Config.ComputeJobDirectoryFile: defines the path to the file in the "Docker job container" that shows the directory path where the asynchronous compute job is being executed.
Config.ComputeJobDirectoryFile = "/home/idies/jobs.path" 
#Config.RacmApiURL: defines the base URL of the multiple APIs in RACM
Config.RacmApiURL = 'http://scitest12.pha.jhu.edu/racm';

.onLoad <- function(libname, pkgname) {
	readSciServerConfig <- function(filename) {
		if (file.exists(filename)) {
			config.file.data <- jsonlite::fromJSON(SCISERVER_CONFIG)
			Config.CasJobsRESTUri <<- if (is.null(config.file.data$CasJobsRESTUri)) SciServer:::Config.CasJobsRESTUri else config.file.data$CasJobsRESTUri
			Config.AuthenticationURL <<- if (is.null(config.file.data$AuthenticationURL)) SciServer:::Config.AuthenticationURL else config.file.data$AuthenticationURL
			Config.SciDriveHost <<- if (is.null(config.file.data$SciDriveHost)) SciServer:::Config.SciDriveHost else config.file.data$SciDriveHost
			Config.SkyQueryUrl <<- if (is.null(config.file.data$SkyQueryUrl)) SciServer:::Config.SkyQueryUrl else config.file.data$SkyQueryUrl
			Config.SkyServerWSurl <<- if (is.null(config.file.data$SkyServerWSurl)) SciServer:::Config.SkyServerWSurl else config.file.data$SkyServerWSurl
			Config.DataRelease <<- if (is.null(config.file.data$DataRelease)) SciServer:::Config.DataRelease else config.file.data$DataRelease
			Config.KeystoneTokenFilePath <<- if (is.null(config.file.data$KeystoneTokenPath)) SciServer:::Config.KeystoneTokenFilePath else config.file.data$KeystoneTokenPath
			Config.Version <<- if (is.null(config.file.data$version)) SciServer:::Config.Version else config.file.data$version
		}
	}

	SYSTEM_CONFIG_DIR <- '/etc'
	CONFIG_DIR <- Sys.getenv("XDG_CONFIG_HOME", file.path(normalizePath("~"), ".config"))

	for (configDir in c(SYSTEM_CONFIG_DIR, CONFIG_DIR)) {
		readSciServerConfig(file.path(configDir, 'sciserver', 'sciscript.json'))
	}
}

# returns TRUE if the library is running inside the SciServer-Compute, and FALSE if not
Config.isSciServerComputeEnvironment<-function()
{
  if(file.exists(Config.KeystoneTokenFilePath)){
    return (TRUE)
  }else{
    return (FALSE)
  }
}
