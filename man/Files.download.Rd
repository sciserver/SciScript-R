\name{Files.download}
\alias{Files.download}
\title{
Download file or data
}
\description{
Downloads a file from the remote file system into the local file system, or returns the file content as an object in several formats.
}
\usage{Jobs.download(fileService, path, localFilePath=NULL, format="txt", quiet=TRUE)}
\arguments{
  \item{fileService}{name of fileService (string), or object (list) that defines a file service. A list of these kind of objects available to the user is returned by the function Files.getFileServices().}
  \item{path}{String defining the path (in the remote file service) of the file to be downloaded, starting from the root volume level. Example: rootVolume/userVolumeOwner/userVolume/fileToBeDownloaded.txt.}
  \item{localFilePath}{local destination path of the file to be downloaded. If set to NULL, then an object of format 'format' will be returned.}
  \item{format}{name (string) of the returned object's type (if localFilePath is not defined). This parameter can be "txt" , "raw" or "response". If the parameter 'localFilePath' is defined, then the 'format' parameter is not used and the file is downloaded to the local file system instead.}
  \item{quiet}{If set to FALSE, it will throw an error if the file does not exist. If set to TRUE. it will not throw an error.}
}
\value{If the 'localFilePath' parameter is defined, then it will return TRUE when the file is downloaded successfully in the local file system. If the 'localFilePath' is not defined, then the type of the returned object depends on the value of the 'format' parameter.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Files.getFileServices}}, \code{\link[SciServer]{Files.getFileServiceFromName}},
\code{\link[SciServer]{Files.delete}}, \code{\link[SciServer]{Files.createDir}},
\code{\link[SciServer]{Files.upload}}, \code{\link[SciServer]{Files.dirList}}, \code{\link[SciServer]{Files.createUserVolume}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://apps.sciserver.org/dashboard/files}{http://apps.sciserver.org/dashboard/files}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
fileServices = Files.getFileServices(); isDownloaded = Files.download(fileServices[[1]], path="rootVolume/userVolumeOwner/userVolume/fileToBeDownloaded.txt", localFilePath="/myUploadedFile.txt");
}
\keyword{SciServer}