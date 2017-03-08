\name{LoginPortal.login}
\alias{LoginPortal.login}
\title{
User login (DEPRECATED).
}
\description{
DEPRECATED. Use \code{\link[SciServer]{Authentication.login}} instead.

Logs the user into SciServer and returns the LoginPortal token. This function is useful when SciScript-R library methods are executed outside the SciServer-Compute environment. In this case, the session LoginPortal token does not exist (and therefore can't be automatically recognized), so the user has to use \code{LoginPortal.login} in order to log into SciServer manually and get the LoginPortal token. \code{LoginPortal.login} also sets the token value in the Sys environment variable "sciservertoken" and as the variable \code{\link[SciServer]{LoginPortal.token}}.
}
\usage{LoginPortal.login(UserName, Password)}
\arguments{
  \item{UserName}{name of the user.}
  \item{Password}{password of the user.}
}
\value{Returns the LoginPortal token (string). Throws an exception if the HTTP request to the LoginPortal URL returns an error.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
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
\dontrun{LoginPortal.login("MyName","MyPassword") }
}
\keyword{SciServer}
