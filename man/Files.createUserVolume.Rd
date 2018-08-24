\name{Files.createUserVolume}
\alias{Files.createUserVolume}
\title{
Create a user volume.
}
\description{
Create a user volume.
}
\usage{Jobs.createUserVolume(fileService, path, quiet=TRUE)}
\arguments{
  \item{fileService}{name of fileService (string), or object (list) that defines a file service. A list of these kind of objects available to the user is returned by the function Files.getFileServices().}
  \item{path}{path (in the remote file service) to the user volume (string), starting from the root volume level. Example: rootVolume/userVolumeOwner/userVolume}
  \item{quiet}{if set to FALSE, will throw an error if the User Volume already exists. If TRUE, won't throw an error.}
}
\value{void}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Files.getFileServices}}, \code{\link[SciServer]{Files.getFileServiceFromName}},
\code{\link[SciServer]{Files.delete}}, \code{\link[SciServer]{Files.upload}},
\code{\link[SciServer]{Files.download}}, \code{\link[SciServer]{Files.dirList}}, \code{\link[SciServer]{Files.deleteUserVolume}}
}

\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://apps.sciserver.org/dashboard/files}{http://apps.sciserver.org/dashboard/files}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
fileServices = Files.getFileServices(); Files.createUserVolume(fileServices[[1]], "rootVolume/userVolumeOwner/userVolume");
}
\keyword{SciServer}