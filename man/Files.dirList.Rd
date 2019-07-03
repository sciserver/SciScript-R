\name{Files.dirList}
\alias{Files.dirList}
\title{
List directory contents
}
\description{
Lists the contents of a directory.
}
\usage{Jobs.dirList(fileService, path, level=1, options="")}
\arguments{
  \item{fileService}{name of fileService (string), or object (list) that defines a file service. A list of these kind of objects available to the user is returned by the function Files.getFileServices().}
  \item{path}{String defining the path (in the remote file service) of the directory to be listed, starting from the root volume level or data volume level. Examples: rootVolume/userVolumeOwner/userVolume/directoryToBeListed or dataVolume/directoryToBeListed}
  \item{level}{amount (int) of listed directory levels that are below or at the same level to that of the relativePath.}
  \item{options}{string specifiying the file or directory patterns to match, based on the glob syntax}
}
\value{list containing the directory listing.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Files.getFileServices}}, \code{\link[SciServer]{Files.getFileServiceFromName}},
\code{\link[SciServer]{Files.delete}}, \code{\link[SciServer]{Files.createDir}},
\code{\link[SciServer]{Files.upload}}, \code{\link[SciServer]{Files.download}}, \code{\link[SciServer]{Files.createUserVolume}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://apps.sciserver.org/dashboard/files}{http://apps.sciserver.org/dashboard/files}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
fileServices = Files.getFileServices(); dirs = Files.dirList(fileService=fileServices[[1]], path="rootVolume/userVolumeOwner/userVolume/directoryToBeListed", level=4)
}
\keyword{SciServer}