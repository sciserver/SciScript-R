\name{Files.move}
\alias{Files.move}
\title{
Move or copy file or folder
}
\description{
Moves or copies a file or folder within remote FileService(s)
}
\usage{Jobs.move(fileService, path, destinationFileService, destinationPath, replaceExisting=TRUE, doCopy=TRUE)}
\arguments{
  \item{fileService}{name of fileService (string), or object (list) that defines a file service. A list of these kind of objects available to the user is returned by the function Files.getFileServices().}
  \item{path}{String defining the origin path (in the remote fileService) of the file or directory to be copied/moved, starting from the root volume level. Example: rootVolume/userVolumeOwner/userVolume/fileToBeMoved.txt.}
  \item{destinationFileService}{name of fileService (string), or object (list) that defines a destination file service (where the file is moved/copied into). A list of these kind of objects available to the user is returned by the function Files.getFileServices().}
  \item{destinationPath}{String defining the destination path (in the remote destinationFileService) of the file or directory to be copied/moved, starting from the root volume level. Example: rootVolume/userVolumeOwner/userVolume/recentlyMovedFile.txt.}
  \item{replaceExisting}{If set to FALSE, it will throw an error if the file already exists, If set to TRUE, it will not throw and eeror in that case.}
  \item{doCopy}{if set to TRUE, then it will copy the file or folder. If set to FALSE, then the file or folder will be moved.}
}
\value{void}
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
fileServices = Files.getFileServices(); Files.move(fileService[[1]],"rootVolume/userVolumeOwner/userVolume/fileToBeMoved.txt",fileService[[1]],"rootVolume2/userVolumeOwner/userVolume2/fileToBeCopied.txt")
}
\keyword{SciServer}