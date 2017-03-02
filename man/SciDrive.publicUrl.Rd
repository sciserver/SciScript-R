\name{SciDrive.publicUrl}
\alias{SciDrive.publicUrl}
\title{
Get public URL of file.
}
\description{
Gets the public URL of a file (or directory) in SciDrive.
}
\usage{SciDrive.publicUrl(scidrivePath)}
\arguments{
  \item{scidrivePath}{path of the file (or directory) in SciDrive.}
}
\value{
URL of a file in SciDrive (string). Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the SciDrive API returns an error.
}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
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
url = SciDrive.publicUrl("path/to/SciDrive/file.csv")
}}
}
\keyword{SciServer}
