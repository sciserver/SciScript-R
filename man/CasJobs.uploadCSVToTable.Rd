\name{CasJobs.uploadCSVToTable}
\alias{CasJobs.uploadCSVToTable}
\title{
Upload CSV file into DB table.
}
\description{

Uploads a CSV file to a database table in CasJobs.

}
\usage{CasJobs.uploadCSVToTable(csv, tableName, context="MyDB", token=NULL)}
\arguments{
  \item{csv}{Path to CSV file}
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
\code{\link[SciServer]{CasJobs.uploadDataFrameToTable}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org/casjobs}{http://skyserver.sdss.org/casjobs}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
response = CasJobs.uploadCSVToTable("/path/to/file.csv","MyNewTable")
}
\keyword{SciServer}