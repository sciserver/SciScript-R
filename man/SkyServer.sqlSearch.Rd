\name{SkyServer.sqlSearch}
\alias{SkyServer.sqlSearch}
\title{
Execute SQL query.
}
\description{
Executes a SQL query to the SDSS database, and retrieves the result table as a dataframe.
}
\usage{SkyServer.sqlSearch(sql,limit,token)}
\arguments{
  \item{sql}{a string containing the sql query}
  \item{limit}{maximum number of rows in the result table}
  \item{token}{Sciserver's authentication token for the user} 
}
\value{ the returned object is of type 'dataframe'.}
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
table <- SkyServer.sqlSearch(sql="select 1")
}
\keyword{SciServer}
