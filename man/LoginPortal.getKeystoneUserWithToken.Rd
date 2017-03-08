\name{LoginPortal.getKeystoneUserWithToken}
\alias{LoginPortal.getKeystoneUserWithToken}
\title{
Get user info (DEPRECATED).
}
\description{
DEPRECATED. Use \code{\link[SciServer]{Authentication.getKeystoneUserWithToken}} instead.

Returns the name/id for the user corresponding to the specified token. 
}
\usage{LoginPortal.getKeystoneUserWithToken(token)}
\arguments{
  \item{token}{Sciserver's LoginPortal token for the user}
}
\value{Returns a list with name/id of the user. Throws an exception if the HTTP request to the LoginPortal URL returns an error.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{LoginPortal.getKeystoneUserWithToken}}, \code{\link[SciServer]{LoginPortal.setToken}}, \code{\link[SciServer]{LoginPortal.login}}, \code{\link[SciServer]{LoginPortal.getToken}}, \code{\link[SciServer]{Authentication.getKeystoneUserWithToken}}, \code{\link[SciServer]{Authentication.login}}, \code{\link[SciServer]{Authentication.getToken}}, \code{\link[SciServer]{Authentication.setToken}}, \code{\link[SciServer]{Authentication.token}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://portal.sciserver.org/login-portal/}{http://portal.sciserver.org/login-portal/}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
user <- LoginPortal.getKeystoneUserWithToken(LoginPortal.getToken())
}
\keyword{SciServer}
