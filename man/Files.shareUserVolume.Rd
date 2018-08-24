\name{Files.shareUserVolume}
\alias{Files.shareUserVolume}
\title{
Share a user volume
}
\description{
Shares a user volume with another user or group
}
\usage{Jobs.shareUserVolume(fileService, path, sharedWith, allowedActions, type="USER")}
\arguments{
  \item{fileService}{name of fileService (string), or object (dictionary) that defines a file service. A list of these kind of objects available to the user is returned by the function Files.getFileServices().}
  \item{path}{String defining the path (in the remote fileService) of the user volume to be shared, starting from the root volume level. Example: rootVolume/userVolumeOwner/userVolume.}
  \item{sharedWith}{name (string) of user or group that the user volume is shared with.}
  \item{allowedActions}{list of strings defining actions the user or group is allowed to do with respect to the shared user volume. E.g.: list("read"). Type of actions are "read", "write", "grant" (the user or group can also share the user volume with another user or group) and "delete" (ability to delete the user volume , use with care).}  
  \item{type}{type (string) of the entity defined by the "sharedWith" parameter. Can be set to "USER" or "GROUP".}
}
\value{void}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Files.getFileServices}}, \code{\link[SciServer]{Files.getFileServiceFromName}},
\code{\link[SciServer]{Files.move}}, \code{\link[SciServer]{Files.createDir}},
\code{\link[SciServer]{Files.upload}}, \code{\link[SciServer]{Files.download}}, \code{\link[SciServer]{Files.createUserVolume}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://apps.sciserver.org/dashboard/files}{http://apps.sciserver.org/dashboard/files}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
fileServices = Files.shareUserVolume(); Files.shareUserVolume(fileService[[1]],"rootVolume/userVolumeOwner/userVolume",sharedWith="userName", allowedActions=list("read"), type="USER")
}
\keyword{SciServer}