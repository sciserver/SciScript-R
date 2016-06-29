\name{LoginPortal.getKeystoneUserWithToken}
\alias{LoginPortal.getKeystoneUserWithToken}
\title{
Get user info.
}
\description{
Returns the name/id for the user corresponding to the specified token
}
\usage{LoginPortal.getKeystoneUserWithToken(token)}
\arguments{
  \item{token}{Sciserver's authentication token for the user}
}
\value{Returns a list with name/id of the user.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{LoginPortal.getToken}}, \code{\link[SciServer]{LoginPortal.login}}, \code{\link[SciServer]{LoginPortal.setToken}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
user <- LoginPortal.getKeystoneUserWithToken(LoginPortal.getToken())
}
\keyword{SciServer}
