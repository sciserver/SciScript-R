\name{Files.splitPath}
\alias{Files.splitPath}
\title{
Split path into components
}
\description{
Splits a path of the form rootVolume/userVolumeOwner/userVolume/relativePath/... into its 4 components: rootVolume, userVolumeOwner, userVolume, and relativePath.
}
\usage{Jobs.splitPath(path)}
\arguments{
  \item{path}{file system path (string), starting from the root volume level. Example: rootVolume/userVolumeOwner/userVolume/relativePath...}
}
\value{a string vector containing the four components: (rootVolume, userVolumeOwner, userVolume, relativePath)}
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