\name{CasJobs.waitForJob}
\alias{CasJobs.waitForJob}
\title{
Wait for job status.
}
\description{
Queries the job status from casjobs every 2 seconds and
waits for the casjobs job to return a status of 3, 4, or 5.
}
\usage{CasJobs.waitForJob(jobid)}
\arguments{
  \item{jobid}{id of job.}
}
\value{Returns a list containing the job status and related metadata.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.submitJob}}, \code{\link[SciServer]{CasJobs.getJobStatus}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
CasJobs.waitForJob(CasJobs.submitJob("select 1"))
}
\keyword{SciServer}