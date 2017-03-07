\name{SkyQuery.dropTable}
\alias{SkyQuery.dropTable}
\title{
Drop table
}
\description{
Drops (deletes) a table from the user database (more info in http://www.voservices.net/skyquery).
}
\usage{SkyQuery.dropTable(datasetName, tableName)}
\arguments{
  \item{datasetName}{name of dataset (string).}
  \item{tableName}{name of table (string) within dataset.}
}
\value{ Returns TRUE if the table was deleted successfully. Throws an exception if the user is not logged into SciServer (use Authentication.login for that purpose). Throws an exception if the HTTP request to the SkyQuery API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyQuery.listQueues}}, \code{\link[SciServer]{SkyQuery.listAllDatasets}}, \code{\link[SciServer]{SkyQuery.getDatasetInfo}}, \code{\link[SciServer]{SkyQuery.listDatasetTables}}, \code{\link[SciServer]{SkyQuery.getTableInfo}}, \code{\link[SciServer]{SkyQuery.listTableColumns}}, \code{\link[SciServer]{SkyQuery.getTable}}, \code{\link[SciServer]{SkyQuery.dropTable}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.voservices.net/skyquery}{http://www.voservices.net/skyquery}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
response = SkyQuery.dropTable("MyDB", "myTable")
}
\keyword{SciServer}
