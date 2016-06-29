\name{CasJobs.submitJob}
\alias{CasJobs.submitJob}
\title{
Submit asynchronous SQL query.
}
\description{
Submits an asynchronous SQL query to the CasJobs queue. If a token is supplied then it will execute on behalf of the token's user.
}
\usage{CasJobs.submitJob(queryString,context,acceptHeader,token)}
\arguments{
  \item{queryString}{sql query}
  \item{context}{database context}
  \item{acceptHeader}{Header-specified content type for the request to CasJobs API.}
  \item{token}{Sciserver's authentication token for the user}
}
\value{ Returns the CasJobs jobID (integer).}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.executeQuery}}, \code{\link[SciServer]{CasJobs.getJobStatus}}, \code{\link[SciServer]{CasJobs.waitForJob}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
jobid <- CasJobs.submitJob("select 1 as foo","MyDB")
}
\keyword{SciServer}