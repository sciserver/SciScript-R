\name{Jobs.submitRDBQueryJob}
\alias{Jobs.submitRDBQueryJob}
\title{
Submit SQL query job
}
\description{
Submits a sql query for execution (as an asynchronous job) inside a relational database (RDB) compute domain.
}
\usage{Jobs.submitNotebookJob(sqlQuery, rdbComputeDomain=NULL, databaseContextName = NULL, resultsName='queryResults', resultsFolderPath="", jobAlias = "")}
\arguments{
  \item{sqlQuery}{sql query (string)}
  \item{rdbComputeDomain}{object (list) that defines a relational database (RDB) compute domain. A list of these kind of objects available to the user is returned by the function Jobs.getRDBComputeDomains().}
  \item{databaseContextName}{database context name (string) on which the sql query is executed.}
  \item{resultsName}{name (string) of the table or file (without file type ending) that contains the query result. In case the sql query has multiple statements, should be set to a list of names (e.g., ['result1','result2']).}
  \item{dataVolumes}{a list with the names of data volumes that will be mounted to the docker Image. E.g.: dataVolumes=list(list(name='SDSS DAS')). A list of available data volumes can be found as the 'volumes' property in the dockerComputeDomain list. If dataVolumes=NULL, then all available data volumes are mounted.}
  \item{resultsFolderPath}{full path to results folder (string) where query output tables are written into. E.g.: "/home/idies/workspace/rootVOlume/username/userVolume/jobsFolder"" . If not set, then a default folder will be set automatically.}
  \item{jobAlias}{alias (string) of job, defined by the user.}
}  
\value{the job ID (integer)}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Jobs.submitNotebookJob}},\code{\link[SciServer]{Jobs.submitShellCommandJob}},\code{\link[SciServer]{Jobs.getJobStatus}},\code{\link[SciServer]{Jobs.getRDBComputeDomains}},\code{\link[SciServer]{Jobs.cancelJob}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
jobId = Jobs.submitRDBQueryJob("select 1", NULL, NULL, "myQueryResults", "myNewJob")
}
\keyword{SciServer}