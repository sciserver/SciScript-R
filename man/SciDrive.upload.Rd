\name{SciDrive.upload}
\alias{SciDrive.upload}
\title{
Upload file.
}
\description{
Uploads a file into a directory in SciDrive.
}
\usage{SciDrive.upload(path, data, token=NULL)}
\arguments{
  \item{path}{path of the directory in SciDrive.}
  \item{data}{path to file to be uploaded.}
  \item{token}{Sciserver's authentication token for the user}
}
\value{Returns a response object with attributes such as status_code,headers,url.}
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
response = SciDrive.upload("SciDrive/path/to/file.csv", "local/path/to/file.csv"))
}}
}
\keyword{SciServer}
