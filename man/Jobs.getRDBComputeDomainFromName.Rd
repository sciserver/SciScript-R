\name{Jobs.getRDBComputeDomainFromName}
\alias{Jobs.getRDBComputeDomainFromName}
\title{
Get an RDBComputeDomain
}
\description{
Returns an RDBComputeDomain object, given its registered name.
}
\usage{Jobs.getDockerComputeDomainsNames(rdbComputeDomainName, rdbComputeDomains = NULL)}
\arguments{
  \item{rdbComputeDomainName}{name of the RDBComputeDomain, as shown within the results of Jobs.getRDBComputeDomainsNames() or Jobs.getRDBComputeDomains()}
  \item{rdbComputeDomains}{a list of RDBComputeDomain objects, as returned by Jobs.getRDBComputeDomains(). If not set, then an internal call to Jobs.getRDBComputeDomains() is made.}  
}
\value{an RDBComputeDomain object (list) that defines an RDB compute domain. A list of these kind of objects available to the user is returned by the function Jobs.getRDBComputeDomains().}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Jobs.getRDBComputeDomains}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
rdbComputeDomain = Jobs.getRDBComputeDomainFromName("rdbComputeDomainAtJHU");
}
\keyword{SciServer}