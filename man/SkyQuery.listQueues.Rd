\name{SkyQuery.listQueues}
\alias{SkyQuery.listQueues}
\title{
List queues
}
\description{
Returns a list of all available job queues and related metadata (more info in http://www.voservices.net/skyquery).
}
\usage{SkyQuery.listQueues()}
\value{ Returns a list of all available job queues and related metadata (more info in http://www.voservices.net/skyquery). Throws an exception if the user is not logged into SciServer (use Authentication.login for that purpose). Throws an exception if the HTTP request to the SkyQuery API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyQuery.getQueueInfo}}, \code{\link[SciServer]{SkyQuery.submitJob}}, \code{\link[SciServer]{SkyQuery.getJobStatus}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.voservices.net/skyquery}{http://www.voservices.net/skyquery}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
queueList = SkyQuery.listQueues()
}
\keyword{SciServer}
