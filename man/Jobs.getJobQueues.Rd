\name{Jobs.getJobQueues}
\alias{Jobs.getJobQueues}
\title{
Get Jobs Queues
}
\description{
Gets information about queues of jobs submitted to all compute domains, including the ranking jobs that users have already submitted to the queue for execution, as well as the ranking that a new job would get if submitted to a queue.
}
\usage{Jobs.getJobQueues()}
\arguments{
  \item{format}{Format (string) of the returned object. If equal to "dataframe" (default setting), then the reult is a dataframe, else it is a list.}
}
\value{a dataframe or list containing job queues information.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Jobs.getJobsList}},\code{\link[SciServer]{Jobs.submitNotebookJob}},\code{\link[SciServer]{Jobs.submitShellCommandJob}},\code{\link[SciServer]{Jobs.getJobStatus}},\code{\link[SciServer]{Jobs.getDockerComputeDomains}},\code{\link[SciServer]{Jobs.cancelJob}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
jobs = Jobs.getJobQueues()
}
\keyword{SciServer}