\name{SkyQuery.uploadTable}
\alias{SkyQuery.uploadTable}
\title{
Upload table
}
\description{
Uploads a data table into a database (more info in http://www.voservices.net/skyquery).
}
\usage{SkyQuery.uploadTable(uploadData, tableName, datasetName="MyDB", format="csv")}
\arguments{
  \item{data}{data table, for now accepted in CSV string format.}
  \item{tableName}{name of table (string) within dataset.}
  \item{datasetName}{name of dataset or database context (string).}
  \item{format}{format of the 'data' parameter. Set to 'csv' for now.}
}
\value{ Returns TRUE if the table was uploaded successfully. Throws an exception if the user is not logged into SciServer (use Authentication.login for that purpose). Throws an exception if the HTTP request to the SkyQuery API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyQuery.listQueues}}, \code{\link[SciServer]{SkyQuery.listAllDatasets}}, \code{\link[SciServer]{SkyQuery.getDatasetInfo}}, \code{\link[SciServer]{SkyQuery.listDatasetTables}}, \code{\link[SciServer]{SkyQuery.getTableInfo}}, \code{\link[SciServer]{SkyQuery.listTableColumns}}, \code{\link[SciServer]{SkyQuery.getTable}}, \code{\link[SciServer]{SkyQuery.dropTable}, \code{\link[SciServer]{SkyQuery.uploadTable}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.voservices.net/skyquery}{http://www.voservices.net/skyquery}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
result = SkyQuery.uploadTable("Column1,Column2\n4.5,5.5\n", tableName="myTable", datasetName="MyDB", format="csv")
}
\keyword{SciServer}
