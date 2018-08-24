\name{Jobs.getRDBComputeDomainsNames}
\alias{Jobs.getRDBComputeDomainsNames}
\title{
Get database domains names.
}
\description{
Returns the names of the RDB compute domains available to the user.
}
\usage{Jobs.getRDBComputeDomainsNames(rdbComputeDomains=NULL)}
\arguments{
  \item{rdbComputeDomains}{a list of rdbComputeDomain objects, as returned by Jobs.getRDBComputeDomains(). If not set, then an extra internal call to Jobs.getRDBComputeDomains() is made.}
}
\value{a list of strings, each being the name of a rdb compute domain available to the user.}
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
rdbComputeDomainsNames = Jobs.getRDBComputeDomainsNames()
}
\keyword{SciServer}