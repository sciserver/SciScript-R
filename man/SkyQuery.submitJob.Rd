\name{SkyQuery.submitJob}
\alias{SkyQuery.submitJob}
\title{
Job submission
}
\description{
Submits a new job (more info in http://www.voservices.net/skyquery).
}
\usage{SkyQuery.submitJob(query, queue="quick")}
\arguments{
  \item{query}{sql query (string).}
  \item{queue}{queue name (string).}
}
\value{ Returns the jobID (string), unique identifier of the job. Throws an exception if the user is not logged into SciServer (use Authentication.login for that purpose). Throws an exception if the HTTP request to the SkyQuery API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyQuery.getJobStatus}}, \code{\link[SciServer]{SkyQuery.listQueues}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.voservices.net/skyquery}{http://www.voservices.net/skyquery}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
jobId = SkyQuery.submitJob('select 1 as foo',"quick")
}
\keyword{SciServer}
