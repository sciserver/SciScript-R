\name{CasJobs.waitForJob}
\alias{CasJobs.waitForJob}
\title{
Wait for job status.
}
\description{
Queries the job status from casjobs every 2 seconds and
waits for the casjobs job to return a status of 3, 4, or 5.
}
\usage{CasJobs.waitForJob(jobid, verbose=TRUE)}
\arguments{
  \item{jobid}{id of job (integer).}
  \item{verbose}{message printing option.} If TRUE, will print "wait" messages on the screen while the job is not done. If FALSE, will suppress printing messages on the screen.
}
\value{Returns a list containing the job status and related metadata. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the CasJobs API returns an error.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.submitJob}}, \code{\link[SciServer]{CasJobs.getJobStatus}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
CasJobs.waitForJob(CasJobs.submitJob("select 1"))
}
\keyword{SciServer}