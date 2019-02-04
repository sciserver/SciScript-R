\name{Files.createDir}
\alias{Files.createDir}
\title{
Create a directory.
}
\description{
Create a directory or folder.
}
\usage{Jobs.createDir(fileService, path, quiet=TRUE)}
\arguments{
  \item{fileService}{name of fileService (string), or object (list) that defines a file service. A list of these kind of objects available to the user is returned by the function Files.getFileServices().}
  \item{path}{path (in the remote file service) to the directory (string), starting from the root volume level or data volume level. Examples: rootVolume/userVolumeOwner/userVolume/directory or dataVolume/directory}
    \item{quiet}{if set to FALSE, will throw an error if the directory already exists. If TRUE, won't throw an error.}
}
\value{void}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Files.getFileServices}}, \code{\link[SciServer]{Files.getFileServiceFromName}},
\code{\link[SciServer]{Files.delete}}, \code{\link[SciServer]{Files.upload}},
\code{\link[SciServer]{Files.download}}, \code{\link[SciServer]{Files.dirList}}, \code{\link[SciServer]{Files.createUserVolume}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://apps.sciserver.org/dashboard/files}{http://apps.sciserver.org/dashboard/files}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
fileServices = Files.getFileServices(); Files.createDir(fileServices[[1]], "rootVolume/userVolumeOwner/userVolume/newDirectory");
}
\keyword{SciServer}