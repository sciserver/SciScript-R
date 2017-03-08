\name{LoginPortal.setToken}
\alias{LoginPortal.setToken}
\title{
Set token (DEPRECATED).
}
\description{
DEPRECATED. Use \code{\link[SciServer]{Authentication.setToken}} instead.

Sets the SciServer LoginPortal token of the user as the  Sys environmental variable, as well as in the variable \code{LoginPortal.token}.
}
\usage{LoginPortal.setToken(token)}
\arguments{
  \item{token}{Sciserver's LoginPortal token for the user}
}
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
\dontrun{
LoginPortal.setToken("MyToken")}
}
\keyword{SciServer}
