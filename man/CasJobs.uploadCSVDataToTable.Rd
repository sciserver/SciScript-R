\name{CasJobs.uploadCSVDataToTable}
\alias{CasJobs.uploadCSVDataToTable}
\title{
Upload CSV string table into a DB table.
}
\description{

Uploads a table in CSV string format into a CasJobs database table.

}
\usage{CasJobs.uploadCSVDataToTable(csv, tableName, context="MyDB")}
\arguments{
  \item{csv}{CSV string representing a table (string)}
  \item{tableName}{Name of table (string)}
  \item{context}{Database context (string)}
}
\value{Returns TRUE if the table was uploaded successfully. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the CSV file could not be found. Throws an exception if the HTTP request to the CasJobs API returns an error.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.uploadDataFrameToTable}}, \code{\link[SciServer]{CasJobs.uploadCSVFileToTable}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
response = CasJobs.uploadCSVDataToTable(csv="col1,col2\n1,2", tableName="MyNewTable", context="MyDB")
}
\keyword{SciServer}