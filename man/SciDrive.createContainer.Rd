\name{SciDrive.createContainer}
\alias{SciDrive.createContainer}
\title{
Create container.
}
\description{
Creates a container (directory) in SciDrive.
}
\usage{SciDrive.createContainer(path)}
\arguments{
  \item{path}{path of the directory in SciDrive.}
}
\value{Returns TRUE if the container (directory) was created successfully. Throws an exception if the user is not logged into SciServer (use \code{\link[SciServer]{Authentication.login}} for that purpose). Throws an exception if the HTTP request to the SciDrive API returns an error.}
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
