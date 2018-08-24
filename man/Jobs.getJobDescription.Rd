\name{Jobs.getJobDescription}
\alias{Jobs.getJobDescription}
\title{
Get job description
}
\description{
Gets the definition of a job.
}
\usage{Jobs.getJobDescription(jobId)}
\arguments{
  \item{jobId}{Id of job (integer)}
}
\value{dictionary containing the description or definition of the job.}
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
job = Jobs.getJobDescription(123);
}
\keyword{SciServer}