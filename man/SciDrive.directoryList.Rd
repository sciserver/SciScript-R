\name{SciDrive.directoryList}
\alias{SciDrive.directoryList}
\title{
Get SciDrive directory content.
}
\description{
Gets the contents and metadata of a SciDrive directory (or file).
}
\usage{SciDrive.directoryList(path)}
\arguments{
  \item{path}{path of the directory (or file ) in SciDrive.}
}
\value{
List containing info and metadata of the directory (or file). Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the SciDrive API returns an error.
}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SciDrive.upload}}, \code{\link[SciServer]{SciDrive.download}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.scidrive.org/scidrive}{http://www.scidrive.org/scidrive}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\donttest{
\dontrun{
url = SciDrive.directoryList("path/to/SciDrive/directory")
}}
}
\keyword{SciServer}
