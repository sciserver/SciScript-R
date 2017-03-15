\name{SciDrive.delete}
\alias{SciDrive.delete}
\title{
Delete file or container.
}
\description{
Deletes a file or container (directory) in SciDrive.
}
\usage{SciDrive.delete(path)}
\arguments{
  \item{path}{path of the directory in SciDrive.}
}
\value{Returns TRUE if the file or container (directory) was deleted successfully. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the SciDrive API returns an error.}
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
  response = SciDrive.delete("path/to/SciDrive/file.csv")
}
\keyword{SciServer}
