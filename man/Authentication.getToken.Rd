\name{Authentication.getToken}
\alias{Authentication.getToken}
\title{
Get user token.
}
\description{
Returns the SciServer authentication token of the user. First, will try to return the variable \code{\link[SciServer]{Authentication.token}}. If it is not set, \code{Authentication.getToken} will try to return the "sciservertoken" environmental variable. If this variable does not exist, will try to return the token stored in Config.KeystoneTokenFilePath. Will return a NULL if all previous steps fail.
}
\usage{Authentication.getToken()}

\value{Returns SciServer's authentication token (string).}

\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}

\seealso{
\code{\link[SciServer]{Authentication.getKeystoneUserWithToken}}, \code{\link[SciServer]{Authentication.login}}, \code{\link[SciServer]{Authentication.setToken}}, \code{\link[SciServer]{Authentication.token}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://portal.sciserver.org/login-portal/}{http://portal.sciserver.org/login-portal/}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\dontrun{token = Authentication.getToken()}
}
\keyword{SciServer}
