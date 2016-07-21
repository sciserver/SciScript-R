\name{Authentication.login}
\alias{Authentication.login}
\title{
User login.
}
\description{
Logs the user into SciServer and returns the authentication token, which is then also set as the Sys environment variable "sciservertoken".
}
\usage{Authentication.login(UserName, Password)}
\arguments{
  \item{UserName}{name of the user.}
  \item{Password}{password of the user.}
}
\value{Returns the authentication token (string).}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Authentication.getKeystoneUserWithToken}}, \code{\link[SciServer]{Authentication.getToken}}, \code{\link[SciServer]{Authentication.setToken}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://portal.sciserver.org/login-portal/}{http://portal.sciserver.org/login-portal/}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\dontrun{Authentication.login("MyName","MyPassword") }
}
\keyword{SciServer}
