\name{Authentication.setToken}
\alias{Authentication.setToken}
\title{
Set token.
}
\description{
Sets the SciServer authentication token of the user as a Sys environmental variable.
}
\usage{Authentication.setToken(token)}
\arguments{
  \item{token}{Sciserver's authentication token for the user}
}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Authentication.getKeystoneUserWithToken}}, \code{\link[SciServer]{Authentication.login}}, \code{\link[SciServer]{Authentication.getToken}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://portal.sciserver.org/login-portal/}{http://portal.sciserver.org/login-portal/}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\dontrun{
Authentication.setToken("MyToken")}
}
\keyword{SciServer}
