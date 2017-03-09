\name{SkyQuery.getDatasetInfo}
\alias{SkyQuery.getDatasetInfo}
\title{
Get dataset info.
}
\description{
Gets information related to a particular dataset (more info in http://www.voservices.net/skyquery).
}
\usage{SkyQuery.getDatasetInfo(datasetName)}
\arguments{
  \item{datasetName}{name of dataset (string).}
}
\value{ Returns a list containing the dataset information. Throws an exception if the user is not logged into SciServer (use Authentication.login for that purpose). Throws an exception if the HTTP request to the SkyQuery API returns an error.}
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
info = SkyQuery.getDatasetInfo("MyDB")
}
\keyword{SciServer}
