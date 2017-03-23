\name{SciDrive.download}
\alias{SciDrive.download}
\title{
Download from SciDrive.
}
\description{
Downloads a file (directory) from SciDrive into the local file system, or returns the file conetent as an object in several formats.
}
\usage{SciDrive.download(path, format="text", localFilePath=NULL)}
\arguments{
  \item{path}{path of the file (or directory) in SciDrive.}
  \item{format}{Format of the returned R object that stores the data of the downloaded file. can take values of format="text" (the returned object is a string containing the text inside the file), format="raw" (raw binary format object is returned) and "response" (returned object is of type response in the httr library, i.e., the http response from the API call.). If the parameter 'localFilePath' is defined, then the 'format' parameter is not used and the file is downloaded to the local file system instead.}
  \item{localFilePath}{Local path of the file to be downloaded. If 'localFilePath' is defined, then the 'format' parameter is not used.}
}
\value{
If the 'localFilePath' parameter is defined, then it will return TRUE when the file is downloaded successfully in the local file system. If the 'localFilePath' is not defined, then the type of the returned object depends on the value of the 'format' parameter. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the SciDrive API returns an error.
}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SciDrive.upload}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.scidrive.org/scidrive}{http://www.scidrive.org/scidrive}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\donttest{
\dontrun{
csvString = SciDrive.download(path="path/to/SciDrive/file.csv", format="text")
}}
}
\keyword{SciServer}
