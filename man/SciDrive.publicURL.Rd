\name{SciDrive.publicURL}
\alias{SciDrive.publicURL}
\title{
Get public URL of file.
}
\description{
Gets the public URL of a file or directory in SciDrive.
}
\usage{SciDrive.publicURL(path, token=NULL)}
\arguments{
  \item{path}{path of the file in SciDrive.}
  \item{token}{Sciserver's authentication token for the user}
}
\value{
URL of a file in SciDrive (string).
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
url = SciDrive.publicURL("path/to/SciDrive/file.csv")
}}
}
\keyword{SciServer}
