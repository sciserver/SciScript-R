\name{Jobs.getDockerJobsListQuick}
\alias{Jobs.getDockerJobsListQuick}
\title{
Get Docker Jobs List
}
\description{
Quickly gets the list of Docker Jobs submitted by the user.
}
\usage{Jobs.getDockerJobsListQuick(top=10, open=NULL, start=NULL, end=NULL, labelReg=None)}
\arguments{
  \item{top}{top number of jobs (integer) returned. If top=NULL, then all jobs are returned.}
  \item{open}{Boolean variable. If set to TRUE, then only returns jobs that have not finished executing and wrapped up  (status <= FINISHED). If set to FALSE then only returnes jobs that are still running. If set to NULL, then returns both finished and unfinished jobs.}
\item{start}{The earliest date (inclusive) to search for jobs, in string format yyyy-MM-dd hh:mm:ss.SSS. If set to NULL, then there is no lower bound on date.}
\item{end}{The latest date (inclusive) to search for jobs, in string format yyyy-MM-dd hh:mm:ss.SSS. If set to NULL, then there is no upper bound on date.}
\item{labelReg}{A string occurring in the submitterDIDtype of jobs returned.}
}
\value{a list containing the definitions of submitted jobs.}
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
jobs = Jobs.getDockerJobsListQuick(top=2)
}
\keyword{SciServer}