\name{Files.delete}
\alias{Files.delete}
\title{
Delete file or folder
}
\description{
Deletes a directory or file in the File System.
}
\usage{Jobs.delete(fileService, path, quiet=TRUE)}
\arguments{
  \item{fileService}{name of fileService (string), or object (dictionary) that defines a file service. A list of these kind of objects available to the user is returned by the function Files.getFileServices().}
  \item{path}{String defining the path (in the remote fileService) of the file or directory to be deleted, starting from the root volume level or data volume level. Examples: rootVolume/userVolumeOwner/userVolume/fileToBeDeleted.txt or dataVolume/fileToBeDeleted.txt}
  \item{quiet}{If set to FALSE, it will throw an error if the file does not exist. If set to TRUE. it will not throw an error.}
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
fileServices = Files.getFileServices(); Files.delete(fileServices[[1]],"rootVolume/userVolumeOwner/userVolume/fileToBeDeleted.txt");
}
\keyword{SciServer}