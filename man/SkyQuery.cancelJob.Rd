\name{SkyQuery.cancelJob}
\alias{SkyQuery.cancelJob}
\title{
Cancel a job
}
\description{
Cancels a job already submitted (more info in http://www.voservices.net/skyquery).
}
\usage{SkyQuery.cancelJob(jobId)}
\arguments{
  \item{jobId}{the ID of the job (string), which is obtained at the moment of submitting the job.}
}
\value{ Returns TRUE if the job was canceled successfully. Throws an exception if the user is not logged into SciServer (use Authentication.login for that purpose). Throws an exception if the HTTP request to the SkyQuery API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyQuery.submitJob}}, \code{\link[SciServer]{SkyQuery.getJobStatus}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.voservices.net/skyquery}{http://www.voservices.net/skyquery}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
isCanceled = SkyQuery.cancelJob(SkyQuery.submitJob("select 1 as foo"))
}
\keyword{SciServer}
