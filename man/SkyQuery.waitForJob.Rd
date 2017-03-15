\name{SkyQuery.waitForJob}
\alias{SkyQuery.waitForJob}
\title{
Wait for finished job.
}
\description{
Queries the job status from SkyQuery every 2 seconds and waits for the SkyQuery job to be completed.
}
\usage{SkyQuery.waitForJob(jobid, verbose=TRUE)}
\arguments{
  \item{jobid}{id of job (string).}
  \item{verbose}{message printing option.} If TRUE, will print "wait" messages on the screen while the job is not done. If FALSE, will suppress printing messages on the screen.
}
\value{Returns a list containing the job status and related metadata. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the SkyQuery API returns an error.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyQuery.submitJob}}, \code{\link[SciServer]{SkyQuery.getJobStatus}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
SkyQuery.waitForJob(SkyQuery.submitJob("select 1"))
}
\keyword{SciServer}