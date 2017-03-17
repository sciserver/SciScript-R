\name{CasJobs.submitJob}
\alias{CasJobs.submitJob}
\title{
Submit asynchronous SQL query.
}
\description{
Submits an asynchronous SQL query to the CasJobs queue.
}
\usage{CasJobs.submitJob(sql, context="MyDB")}
\arguments{
  \item{sql}{sql query}
  \item{context}{database context}
}
\value{ Returns the CasJobs jobID (integer). Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the CasJobs API returns an error.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.executeQuery}}, \code{\link[SciServer]{CasJobs.getJobStatus}}, \code{\link[SciServer]{CasJobs.waitForJob}}, \code{\link[SciServer]{CasJobs.cancelJob}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\dontrun{jobid <- CasJobs.submitJob("select 1 as foo","MyDB")}
}
\keyword{SciServer}