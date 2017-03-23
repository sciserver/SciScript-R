\name{SkyQuery.getJobStatus}
\alias{SkyQuery.getJobStatus}
\title{
Get job status.
}
\description{
Gets the status of a job, as well as other related metadata (more info in http://www.voservices.net/skyquery).
}
\usage{SkyQuery.getJobStatus(jobId)}
\arguments{
  \item{jobId}{the ID of the job (string), which is obtained at the moment of submitting the job.}
}
\value{Returns a list containing the job status and other related metadata. Throws an exception if the user is not logged into SciServer (use Authentication.login for that purpose). Throws an exception if the HTTP request to the SkyQuery API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyQuery.submitJob}}, \code{\link[SciServer]{SkyQuery.cancelJob}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.voservices.net/skyquery}{http://www.voservices.net/skyquery}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
status = SkyQuery.getJobStatus(SkyQuery.submitJob("select 1 as foo"))
}
\keyword{SciServer}
