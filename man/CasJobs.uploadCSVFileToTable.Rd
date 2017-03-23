\name{CasJobs.uploadCSVFileToTable}
\alias{CasJobs.uploadCSVFileToTable}
\title{
Upload CSV file into DB table.
}
\description{

Uploads a CSV file to a database table in CasJobs.

}
\usage{CasJobs.uploadCSVFileToTable(filePath, tableName, context="MyDB")}
\arguments{
  \item{filePath}{Path to CSV file (string)}
  \item{tableName}{Name of CasJobs table (string)}
  \item{context}{Database context (string)}
}
\value{Returns TRUE if the file was uploaded successfully. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the CSV file could not be found. Throws an exception if the HTTP request to the CasJobs API returns an error.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{CasJobs.uploadDataFrameToTable}}, \code{\link[SciServer]{CasJobs.uploadCSVDataToTable}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
response = CasJobs.uploadCSVFileToTable(filePath="/path/to/file.csv",tableName="MyNewTable", context="MyDB")
}
\keyword{SciServer}