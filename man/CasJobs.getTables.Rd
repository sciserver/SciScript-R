\name{CasJobs.getTables}
\alias{CasJobs.getTables}
\title{
Get tables descriptions.
}
\description{
Gets the names, size and creation date of all tables in a database context that the user has access to.
}
\usage{CasJobs.getTables(context)}
\arguments{
  \item{context}{database context}
}
\value{ Returns a 'list', which includes the names, size and creation date of all tables. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the CasJobs API returns an error.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.getSchemaName}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
tables <- CasJobs.getTables(context="MyDB")
}
\keyword{SciServer}