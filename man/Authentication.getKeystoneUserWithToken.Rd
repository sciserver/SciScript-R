\name{Authentication.getKeystoneUserWithToken}
\alias{Authentication.getKeystoneUserWithToken}
\title{
Get user info.
}
\description{
Returns the name/id for the user corresponding to the specified token
}
\usage{Authentication.getKeystoneUserWithToken(token)}
\arguments{
  \item{token}{Sciserver's authentication token for the user}
}
\value{Returns a list with name/id of the user.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Authentication.getToken}}, \code{\link[SciServer]{Authentication.login}}, \code{\link[SciServer]{Authentication.setToken}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://portal.sciserver.org/login-portal/}{http://portal.sciserver.org/login-portal/}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
user <- Authentication.getKeystoneUserWithToken(Authentication.getToken())
}
\keyword{SciServer}
