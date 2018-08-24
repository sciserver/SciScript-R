\name{Files.upload}
\alias{Files.upload}
\title{
Upload file or data
}
\description{
Uploads data or a local file into a path defined in the file system.
}
\usage{Jobs.upload(fileService, path, data=NULL, localFilePath=NULL, quiet=TRUE)}
\arguments{
  \item{fileService}{name of fileService (string), or object (list) that defines a file service. A list of these kind of objects available to the user is returned by the function Files.getFileServices().}
  \item{path}{path (in the remote file service) to the destination file (string), starting from the root volume level. Example: rootVolume/userVolumeOwner/userVolume/destinationFile.txt}
  \item{data}{string containing data to be uploaded, in case localFilePath is not set.}
  \item{localFilePath}{path to a local file to be uploaded (string)}
  \item{quiet}{If set to FALSE, it will throw an error if the file already exists. If set to TRUE. it will not throw an error.}
}
\value{void}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Files.getFileServices}}, \code{\link[SciServer]{Files.getFileServiceFromName}},
\code{\link[SciServer]{Files.delete}}, \code{\link[SciServer]{Files.createDir}},
\code{\link[SciServer]{Files.download}}, \code{\link[SciServer]{Files.dirList}}, \code{\link[SciServer]{Files.createUserVolume}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://apps.sciserver.org/dashboard/files}{http://apps.sciserver.org/dashboard/files}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
fileServices = Files.getFileServices(); Files.upload(fileServices[0], "myRootVolume/myUserVolume/persistent/myUploadedFile.txt", data="#ID,Column1,Column2\n1,4.5,5.5")
}
\keyword{SciServer}