\name{CasJobs.executeQuery}
\alias{CasJobs.executeQuery}
\title{
Synchronous SQL query.
}
\description{
Executes a synchronous SQL query in a CasJobs database context.
}
\usage{CasJobs.executeQuery(sql,context="MyDB")}
\arguments{
  \item{sql}{sql query}
  \item{context}{database context}
}

\value{ Returns the results table from the query as a dataframe. Throws an exception if the HTTP request to the CasJobs API returns an error}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.submitJob}}, \code{\link[SciServer]{CasJobs.getTables}}, \code{\link[SciServer]{SkyServer.sqlSearch}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
table <- CasJobs.executeQuery(sql="select 1 as foo, 2 as bar",context="MyDB")
}
\keyword{SciServer}