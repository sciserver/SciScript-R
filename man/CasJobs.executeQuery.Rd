\name{CasJobs.executeQuery}
\alias{CasJobs.executeQuery}
\title{
Synchronous SQL query.
}
\description{
Executes a synchronous SQL query in a CasJobs database context.
}
\usage{CasJobs.getTables(sql,context="MyDB",token=NULL)}
\arguments{
  \item{sql}{sql query}
  \item{context}{database context}
  \item{token}{Sciserver's authentication token for the user}
}

\value{ The results table from the query is retuned as a dataframe.}
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