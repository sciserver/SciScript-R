\name{Jobs.getDockerComputeDomains}
\alias{Jobs.getDockerComputeDomains}
\title{
Get Docker compute domains
}
\description{
Gets a list of all registered Docker compute domains that the user has access to.
}
\usage{Jobs.getDockerComputeDomains()}
\value{a list of objects, each one containing the definition of a Docker compute domain.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Jobs.submitShellCommandJob}}, \code{\link[SciServer]{Jobs.getJobStatus}}, \code{\link[SciServer]{Jobs.getRDBComputeDomains}}, \code{\link[SciServer]{Jobs.cancelJob}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
dockerComputeDomains = Jobs.getDockerComputeDomains()
}
\keyword{SciServer}