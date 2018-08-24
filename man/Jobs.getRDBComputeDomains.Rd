\name{Jobs.getRDBComputeDomains}
\alias{Jobs.getRDBComputeDomains}
\title{
Get database compute domains
}
\description{
Gets a list of all registered Relational Database (RDB) compute domains that the user has access to.
}
\usage{Jobs.getRDBComputeDomains()}
\value{a list containing the definition of RDB compute domains.}
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
rdbComputeDomains = Jobs.getRDBComputeDomains()
}
\keyword{SciServer}