\name{Files.getUserVolumesInfo}
\alias{Files.getUserVolumesInfo}
\title{
Get UserVolumes info
}
\description{
Gets the names and definitions of the UserVolumes available in a particular FileService
}
\usage{Jobs.getUserVolumesInfo(fileService, rootVolumeName=NULL, verbose=TRUE)}
\arguments{
  \item{fileService}{name of fileService (string), or object (list) that defines a file service. A list of these kind of objects available to the user is returned by the function Files.getFileServices().}
  \item{rootVolumeName}{name of root Volume (string) for which the user volumes are fetched. If set to NULL, then user volumes in all root folders are fetched.}
  \item{verbose}{boolean parameter defining whether warnings will be printed (set to TRUE) or not (set to FALSE).}
}
\value{list of lists, each of which containing the name and description of a user volume that a user is able to access.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Files.getFileServices}}, \code{\link[SciServer]{Files.getRootVolumesInfo}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://apps.sciserver.org/dashboard/files}{http://apps.sciserver.org/dashboard/files}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
fileServices = Files.getFileServices(); userVolumesInfo = Files.getUserVolumesInfo(fileServices[[1]])
}
\keyword{SciServer}