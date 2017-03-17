\name{CasJobs.cancelJob}
\alias{CasJobs.cancelJob}
\title{
Cancel a Job.
}
\description{

Cancels a job already submitted.

}
\usage{CasJobs.cancelJob(jobId)}
\arguments{
  \item{jobId}{id of the job}
}
\value{ Returns a list containing the job status and related metadata. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the CasJobs API returns an error.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.submitJob}}, \code{\link[SciServer]{CasJobs.waitForJob}}, \code{\link[SciServer]{CasJobs.getJobStatus}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\dontrun{status <- CasJobs.cancelJob(CasJobs.submitJob("select 1"))}
}
\keyword{SciServer}