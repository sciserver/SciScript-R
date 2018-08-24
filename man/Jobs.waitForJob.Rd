\name{Jobs.waitForJob}
\alias{Jobs.waitForJob}
\title{
Wait for job status.
}
\description{
Queries regularly the job status and waits until the job is completed.
}
\usage{Jobs.waitForJob(jobId, verbose=TRUE, pollTime = 5)}
\arguments{
  \item{jobId}{id of job (integer).}
  \item{verbose}{if TRUE, will print "wait" messages on the screen while the job is still running. If FALSE, will suppress the printing of messages on the screen.}
  \item{pollTime}{idle time interval (integer, in seconds) before querying again for the job status. Minimum value allowed is 5 seconds.}
}
\value{Returns a list object containing the job definition after the job is finished.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Jobs.submitShellCommandJob}},\code{\link[SciServer]{Jobs.getJobStatus}},\code{\link[SciServer]{Jobs.getDockerComputeDomains}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
Jobs.cancelJob(123)
}
\keyword{SciServer}