\name{SciDrive.upload}
\alias{SciDrive.upload}
\title{
Upload file or data.
}
\description{
Uploads data or a local file into a SciDrive directory.
}
\usage{SciDrive.upload(path, data="", localFilePath="")}
\arguments{
  \item{path}{desired file path in SciDrive (string).}
  \item{data}{data to be uploaded into SciDrive. If the 'localFilePath' parameter is set, then the local file will be uploaded instead.}
  \item{localFilePath}{ path to the local file to be uploaded (string).}
}
\value{Returns an object with the attributes of the uploaded file. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the SciDrive API returns an error.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SciDrive.createContainer}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.scidrive.org/scidrive}{http://www.scidrive.org/scidrive}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\donttest{
\dontrun{
response = SciDrive.upload("/SciDrive/path/to/file.csv", "/local/path/to/file.csv")
}}
}
\keyword{SciServer}
