\name{Jobs.submitShellCommandJob}
\alias{Jobs.submitShellCommandJob}
\title{
Submit shell command job
}
\description{
Submits a shell command for execution (as an asynchronous job) inside a Docker compute domain.
}
\usage{Jobs.submitShellCommandJob(shellCommand, dockerComputeDomain = NULL, dockerImageName = NULL, userVolumes = NULL, dataVolumes = NULL, resultsFolderPath = "", jobAlias = "")}
\arguments{
  \item{shellCommand}{shell command (string) defined by the user.}
  \item{dockerComputeDomain}{object (list) that defines a Docker compute domain. A list of these kind of objects available to the user is returned by the function Jobs.getDockerComputeDomains().}
  \item{dockerImageName}{name (string) of the Docker image for executing the notebook. E.g.,  dockerImageName="Python (astro)". An array of available Docker images is defined as the 'images' property in the dockerComputeDomain object.}
  \item{userVolumes}{a list with the names of user volumes (with optional write permissions) that will be mounted to the docker Image. E.g.: 
  userVolumes = list( list(name="JobsTestVolume", rootVolumeName="Storage", owner="myUserName", needsWriteAccess=TRUE), list(name="scratch", rootVolumeName="Temporary", owner="myUserName", needsWriteAccess=TRUE) ). A list of available user volumes can be found as the 'userVolumes' property in the dockerComputeDomain object. If userVolumes=Null, then all available user volumes are mounted, with 'needsWriteAccess' = TRUE if the user has Write permissions on the volume.}
  \item{dataVolumes}{a list with the names of data volumes (with optional write permissions) that will be mounted to the docker Image. E.g.: dataVolumes=list(list(name='SDSS DAS', needsWriteAccess=FALSE)). A list of available data volumes can be found as the 'volumes' property in the dockerComputeDomain list. If dataVolumes=NULL, then all available data volumes are mounted.}
  \item{resultsFolderPath}{full path to results folder (string) where the original notebook is copied to and executed. E.g.: /home/idies/workspace/rootVolume/username/userVolume/jobsFolder. If not set, then a default folder will be set automatically.}
  \item{jobAlias}{alias (string) of job, defined by the user.}
}  
\value{the job ID (integer)}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Jobs.submitNotebookJob}},\code{\link[SciServer]{Jobs.getJobStatus}},\code{\link[SciServer]{Jobs.getDockerComputeDomains}},\code{\link[SciServer]{Jobs.cancelJob}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
jobId = Jobs.submitShellCommandJob("pwd")
}
\keyword{SciServer}