\name{Authenticate.setToken}
\alias{Authenticate.setToken}
\title{
Set token.
}
\description{
Sets the SciServer authentication token of the user as a Sys environmental variable.
}
\usage{Authenticate.setToken(token)}
\arguments{
  \item{token}{Sciserver's authentication token for the user}
}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Authenticate.getKeystoneUserWithToken}}, \code{\link[SciServer]{Authenticate.login}}, \code{\link[SciServer]{Authenticate.getToken}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://portal.sciserver.org/login-portal/}{http://portal.sciserver.org/login-portal/}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\dontrun{
Authenticate.setToken("MyToken")}
}
\keyword{SciServer}
