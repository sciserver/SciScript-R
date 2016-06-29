\name{CasJobs.getSchemaName}
\alias{CasJobs.getSchemaName}
\title{
Get name of schema in MyScratch.
}
\description{
Gets the WebServiceID that identifies the schema for a user in MyScratch database with CasJobs.
}
\usage{CasJobs.getSchemaName(token=NULL)}
\arguments{
  \item{token}{Sciserver's authentication token for the user}
}

\value{ WebServiceID of the user as a character string.}

\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.getTables}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
wsid <- CasJobs.getSchemaName()
}
\keyword{SciServer}