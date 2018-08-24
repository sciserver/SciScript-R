\name{Files.getRootVolumesInfo}
\alias{Files.getRootVolumesInfo}
\title{
Get RootVolumes info
}
\description{
Gets the names and descriptions of root volumes available to the user in a particular FileService.
}
\usage{Jobs.getRootVolumesInfo(fileService, verbose=TRUE)}
\arguments{
  \item{fileService}{name of fileService (string), or object (list) that defines a file service. A list of these kind of objects available to the user is returned by the function Files.getFileServices().}
  \item{verbose}{boolean parameter defining whether warnings will be printed (set to TRUE) or not (set to FALSE).}
}
\value{list of lists, each of which containing the name and description of a root volume.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Files.getFileServices}}, \code{\link[SciServer]{Files.getUserVolumesInfo}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://apps.sciserver.org/dashboard/files}{http://apps.sciserver.org/dashboard/files}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
fileServices = Files.getFileServices(); rootVolumesInfo = getRootVolumesInfo(fileServices[[1]])
}
\keyword{SciServer}