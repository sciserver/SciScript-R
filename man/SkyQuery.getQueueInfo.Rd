\name{SkyQuery.getQueueInfo}
\alias{SkyQuery.getQueueInfo}
\title{
Get queue info
}
\description{
Returns information about a particular job queue (more info in http://www.voservices.net/skyquery).
}
\usage{SkyQuery.getQueueInfo(queue)}
\arguments{
  \item{queue}{queue name (string).}
}
\value{ Returns a list containing information associated to the queue. Throws an exception if the user is not logged into SciServer (use Authentication.login for that purpose). Throws an exception if the HTTP request to the SkyQuery API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyQuery.listQueues}}, \code{\link[SciServer]{SkyQuery.submitJob}}, \code{\link[SciServer]{SkyQuery.getJobStatus}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.voservices.net/skyquery}{http://www.voservices.net/skyquery}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
queueInfo = SkyQuery.getQueueInfo('quick')
}
\keyword{SciServer}
