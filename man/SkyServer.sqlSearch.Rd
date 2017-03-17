\name{SkyServer.sqlSearch}
\alias{SkyServer.sqlSearch}
\title{
Execute SQL query.
}
\description{
Executes a SQL query to the SDSS database, and retrieves the result table as a dataframe. Maximum number of rows retrieved is set currently to 500,000.
}
\usage{SkyServer.sqlSearch(sql, dataRelease=NULL)}
\arguments{
  \item{sql}{a string containing the sql query}
  \item{dataRelease}{SDSS data release. E.g, 'DR13'. Default value already set in Config.DataRelease}
}
\value{ Returns the results table as a dataframe. Throws an exception if the HTTP request to the SkyServer API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.executeQuery}}, \code{\link[SciServer]{CasJobs.submitJob}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org}{http://skyserver.sdss.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
df = SkyServer.sqlSearch(sql="select 1")
}
\keyword{SciServer}
