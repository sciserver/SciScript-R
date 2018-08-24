\name{Jobs.cancelJob}
\alias{Jobs.cancelJob}
\title{
Cancel Job
}
\description{
Cancels the execution of a job.
}
\usage{Jobs.cancelJob(jobID)}
\arguments{
  \item{jobID}{Id of the job (integer)}
}
\value{Returns TRUE if successful}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Jobs.submitNotebookJob}},\code{\link[SciServer]{Jobs.submitShellCommandJob}},\code{\link[SciServer]{Jobs.getJobStatus}},\code{\link[SciServer]{Jobs.getDockerComputeDomains}}
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