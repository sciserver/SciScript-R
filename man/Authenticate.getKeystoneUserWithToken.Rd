\name{Authenticate.getKeystoneUserWithToken}
\alias{Authenticate.getKeystoneUserWithToken}
\title{
Get user info.
}
\description{
Returns the name/id for the user corresponding to the specified token
}
\usage{Authenticate.getKeystoneUserWithToken(token)}
\arguments{
  \item{token}{Sciserver's authentication token for the user}
}
\value{Returns a list with name/id of the user.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Authenticate.getToken}}, \code{\link[SciServer]{Authenticate.login}}, \code{\link[SciServer]{Authenticate.setToken}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://portal.sciserver.org/login-portal/}{http://portal.sciserver.org/login-portal/}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
user <- Authenticate.getKeystoneUserWithToken(Authenticate.getToken())
}
\keyword{SciServer}
