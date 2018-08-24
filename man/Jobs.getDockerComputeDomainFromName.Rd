\name{Jobs.getDockerComputeDomainFromName}
\alias{Jobs.getDockerComputeDomainFromName}
\title{
Get a DockerComputeDomain
}
\description{
Returns the names of the docker compute domains available to the user.
}
\usage{Jobs.getDockerComputeDomainFromName(dockerComputeDomainName, dockerComputeDomains = NULL)}
\arguments{
  \item{dockerComputeDomainName}{name of the DockerComputeDomainName, as shown within the results of Jobs.getDockerComputeDomains()}
  \item{dockerComputeDomains}{a list of dockerComputeDomain objects (dictionaries), as returned by Jobs.getDockerComputeDomains(). If not set, then an internal call to Jobs.getDockerComputeDomains() is made.}  
}
\value{a DockerComputeDomain object (list) that defines a Docker compute domain. A list of these kind of objects available to the user is returned by the function Jobs.getDockerComputeDomains().}
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
dockerComputeDomain = Jobs.getDockerComputeDomainFromName("dockerComputeDomainAtJHU");
}
\keyword{SciServer}