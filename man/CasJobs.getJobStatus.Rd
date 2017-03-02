\name{CasJobs.getJobStatus}
\alias{CasJobs.getJobStatus}
\title{
Get status of job.
}
\description{

Shows the status of a job submitted to CasJobs.

}
\usage{CasJobs.getJobStatus(jobid)}
\arguments{
  \item{jobid}{id of the job}
}
\value{ Returns a list containing the job status and related metadata. If \code{jobid} is the empty string, then returns a list with the statuses of all previous jobs. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the CasJobs API returns an error.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.submitJob}}, \code{\link[SciServer]{CasJobs.waitForJob}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
status <- CasJobs.getJobStatus(CasJobs.submitJob("select 1"))
}
\keyword{SciServer}