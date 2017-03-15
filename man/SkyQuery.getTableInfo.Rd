\name{SkyQuery.getTableInfo}
\alias{SkyQuery.getTableInfo}
\title{
Get table info.
}
\description{
Returns info about a particular table belonging to a dataset (more info in http://www.voservices.net/skyquery).
}
\usage{SkyQuery.getTableInfo(tableName, datasetName="MyDB")}
\arguments{
  \item{tableName}{name of table (string) within dataset.}
  \item{datasetName}{name of dataset (string).}
}
\value{ Returns a list containing the table properties and associated info/metadata. Throws an exception if the user is not logged into SciServer (use Authentication.login for that purpose). Throws an exception if the HTTP request to the SkyQuery API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyQuery.listQueues}}, \code{\link[SciServer]{SkyQuery.listAllDatasets}}, \code{\link[SciServer]{SkyQuery.getDatasetInfo}}, \code{\link[SciServer]{SkyQuery.listDatasetTables}}, \code{\link[SciServer]{SkyQuery.getTableInfo}}, \code{\link[SciServer]{SkyQuery.listTableColumns}}, \code{\link[SciServer]{SkyQuery.getTable}}, \code{\link[SciServer]{SkyQuery.dropTable}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.voservices.net/skyquery}{http://www.voservices.net/skyquery}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
info = SkyQuery.getTableInfo("myTable", datasetName="MyDB")
}
\keyword{SciServer}
