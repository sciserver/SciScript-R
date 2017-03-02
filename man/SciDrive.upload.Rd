\name{SciDrive.upload}
\alias{SciDrive.upload}
\title{
Upload file.
}
\description{
Uploads a file into a directory in SciDrive.
}
\usage{SciDrive.upload(scidrivePath, localFile)}
\arguments{
  \item{scidrivePath}{path of the SciDrive directory where the file is being uploaded into.}
  \item{localFile}{local path of the file to be uploaded.}
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
