\name{CasJobs.writeFitsFileFromQuery}
\alias{CasJobs.writeFitsFileFromQuery}
\title{
Write Fits file.
}
\description{

Executes a quick CasJobs query and writes the result to a local Fits file (http://www.stsci.edu/institute/software_hardware/pyfits).

}
\usage{CasJobs.writeFitsFileFromQuery(fileName, queryString, context="MyDB")}
\arguments{
  \item{df}{path to the local Fits file to be created (string).}
  \item{queryString}{sql query (string)}
  \item{context}{database context (string)}
}
\value{Returns True if the fits file was created successfully. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the CasJobs API returns an error.}

\author{
Manuchehr Taghizadeh-Popp \cr
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
response = CasJobs.writeFitsFileFromQuery(fileName="MyFile.fits", queryString="select 1 as foo,2 as bar"), context="MyDB")
}
\keyword{SciServer}