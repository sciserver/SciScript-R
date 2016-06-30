\name{CasJobs.uploadDataFrameToTable}
\alias{CasJobs.uploadDataFrameToTable}
\title{
Upload dataframe into a DB table.
}
\description{

Uploads a dataframe object to a database table in CasJobs.

}
\usage{CasJobs.uploadDataFrameToTable(df, tableName, context="MyDB", token=NULL)}
\arguments{
  \item{df}{dataframe to be uploaded.}
  \item{tableName}{Name of table}
  \item{context}{Database context}
  \item{token}{Sciserver's authentication token for the user}
}
\value{Returns a response object with attributes such as status_code,headers,url.}

\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.uploadCSVToTable}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
response = CasJobs.uploadDataFrameToTable(CasJobs.executeQuery("select 1 as foo,2 as bar"), "MyNewTable")
}
\keyword{SciServer}