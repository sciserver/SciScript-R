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
\value{ Returns TRUE if the job was canceled successfully. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the CasJobs API returns an error.}
\author{
Manuchehr Taghizadeh-Popp \cr
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
\dontrun{isCanceled = CasJobs.cancelJob(CasJobs.submitJob("select 1"))}
}
\keyword{SciServer}