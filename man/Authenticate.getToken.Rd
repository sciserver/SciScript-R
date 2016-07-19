\name{Authenticate.getToken}
\alias{Authenticate.getToken}
\title{
Get user token.
}
\description{
Returns the SciServer authentication token of the user.
}
\usage{Authenticate.getToken()}

\value{Sciserver's authentication token (string).}

\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}

\seealso{
\code{\link[SciServer]{Authenticate.getKeystoneUserWithToken}}, \code{\link[SciServer]{Authenticate.login}}, \code{\link[SciServer]{Authenticate.setToken}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://portal.sciserver.org/login-portal/}{http://portal.sciserver.org/login-portal/}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\dontrun{token = Authenticate.getToken()}
}
\keyword{SciServer}
