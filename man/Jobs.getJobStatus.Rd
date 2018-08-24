\name{Jobs.getJobStatus}
\alias{Jobs.getJobStatus}
\title{
Get job status
}
\description{
Gets a list with the job status as an integer value, together with its semantic meaning. The integer value is a power of 2, that is, 1:PENDING, 2:QUEUED, 4:ACCEPTED, 8:STARTED, 16:FINISHED, 32:SUCCESS, 64:ERROR and 128:CANCELED
}
\usage{Jobs.getJobDescription(jobId)}
\arguments{
  \item{jobId}{Id of job (integer)}
}
\value{list containing the integer value of the job status, as well as its semantic meaning.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Jobs.submitShellCommandJob}},\code{\link[SciServer]{Jobs.getJobStatus}},\code{\link[SciServer]{Jobs.getDockerComputeDomains}},\code{\link[SciServer]{Jobs.cancelJob}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
job = Jobs.getJobStatus(123);
}
\keyword{SciServer}