\name{Files.splitPath}
\alias{Files.splitPath}
\title{
Split path into components
}
\description{
If the top volume in the 'path' is a rootVolume, then this function splits the path of the form rootVolume/userVolumeOwner/userVolume/relativePath/... and returns the vector c(rootVolume, userVolumeOwner, userVolume, relativePath, True). On the other hand,
if the top volume in the 'path' is a dataVolume, then this function splits the path of the form dataVolume/relativePath/... and returns the vector c(dataVolume, None, None, relativePath, False).
}
\usage{Jobs.splitPath(path, fileService)}
\arguments{
  \item{path}{file system path (string), starting from the root volume level. Example: rootVolume/userVolumeOwner/userVolume/relativePath...}
  \item{fileService}{name of fileService (string), or object (list) that defines a file service. A list of these kind of objects available to the user is returned by the function Files.getFileServices().}  
}
\value{a string vector containing the five components: (topVolume, userVolumeOwner, userVolume, relativePath, isTopVolumeARootVolume)}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Files.getFileServices}}, \code{\link[SciServer]{Files.getFileServiceFromName}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://apps.sciserver.org/dashboard/files}{http://apps.sciserver.org/dashboard/files}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
splitPath  = Files.splitPath("Storage/myUserName/persistent/myFolder/myFile")
}
\keyword{SciServer}