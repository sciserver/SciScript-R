\name{SciDrive.download}
\alias{SciDrive.download}
\title{
Download from SciDrive.
}
\description{
Downloads a file (directory) from SciDrive.
}
\usage{SciDrive.download(scidrivePath)}
\arguments{
  \item{scidrivePath}{path of the file (or directory) in SciDrive.}
}
\value{
Response object of type response (httr package), from which the file content can be extracted using the method httr::content. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the SciDrive API returns an error.
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
library(httr); response = SciDrive.download("path/to/SciDrive/file.csv"); file = content(response, as="text", encoding="UTF-8")
}}
}
\keyword{SciServer}
