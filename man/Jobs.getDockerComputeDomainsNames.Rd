\name{Jobs.getDockerComputeDomainsNames}
\alias{Jobs.getDockerComputeDomainsNames}
\title{
Get docker compute domains names.
}
\description{
Returns the names of the docker compute domains available to the user.
}
\usage{Jobs.getDockerComputeDomainsNames(dockerComputeDomains=NULL)}
\arguments{
  \item{dockerComputeDomains}{a list of dockerComputeDomain objects, as returned by Jobs.getDockerComputeDomains(). If not set, then an extra internal call to Jobs.getDockerComputeDomains() is made.}
}
\value{a list of strings, each being the name of a docker compute domain available to the user.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Jobs.getDockerComputeDomains}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
dockerComputeDomainsNames = Jobs.getDockerComputeDomainsNames()
}
\keyword{SciServer}