\name{CasJobs.getSchemaName}
\alias{CasJobs.getSchemaName}
\title{
Get name of schema in MyScratch.
}
\description{
Returns the WebServiceID that identifies the schema for a user in MyScratch database with CasJobs.
}
\usage{CasJobs.getSchemaName()}

\value{ Returns the WebServiceID of the user as a character string. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the CasJobs API returns an error.}

\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.getTables}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\dontrun{wsid <- CasJobs.getSchemaName()}
}
\keyword{SciServer}