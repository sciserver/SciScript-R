\name{SciDrive.createContainer}
\alias{SciDrive.createContainer}
\title{
Create container.
}
\description{
Creates a container (directory) in SciDrive.
}
\usage{SciDrive.createContainer(path, token=NULL)}
\arguments{
  \item{path}{path of the directory in SciDrive.}
  \item{token}{Sciserver's authentication token for the user}
}
\value{Returns a response object with attributes such as status_code,headers,url.}
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
  response = SciDrive.createContainer("MyDirectory")
}
\keyword{SciServer}
