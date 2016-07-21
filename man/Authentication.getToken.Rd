\name{Authentication.getToken}
\alias{Authentication.getToken}
\title{
Get user token.
}
\description{
Returns the SciServer authentication token of the user.
}
\usage{Authentication.getToken()}

\value{Sciserver's authentication token (string).}

\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}

\seealso{
\code{\link[SciServer]{Authentication.getKeystoneUserWithToken}}, \code{\link[SciServer]{Authentication.login}}, \code{\link[SciServer]{Authentication.setToken}}.
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
