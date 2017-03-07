\name{SkyQuery.listJobs}
\alias{SkyQuery.listJobs}
\title{
List Jobs.
}
\description{
Lists the jobs in the queue in descending order by submission time. Only jobs of the authenticated user are listed (more info in http://www.voservices.net/skyquery).
}
\usage{SkyQuery.listJobs(queue="quick")}
\arguments{
  \item{queue}{queue name (string).}
}
\value{ Returns job definitions as a list object. Throws an exception if the user is not logged into SciServer (use Authentication.login for that purpose). Throws an exception if the HTTP request to the SkyQuery API returns an error.}
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
jobsList = SkyQuery.listJobs('quick')
}
\keyword{SciServer}
