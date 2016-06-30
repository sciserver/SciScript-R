\name{LoginPortal.setToken}
\alias{LoginPortal.setToken}
\title{
Set token.
}
\description{
Sets the SciServer authentication token of the user as a Sys environmental variable.
}
\usage{LoginPortal.setToken(token)}
\arguments{
  \item{token}{Sciserver's authentication token for the user}
}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{LoginPortal.getKeystoneUserWithToken}}, \code{\link[SciServer]{LoginPortal.login}}, \code{\link[SciServer]{LoginPortal.getToken}}.
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
