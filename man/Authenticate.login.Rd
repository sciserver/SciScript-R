\name{Authenticate.login}
\alias{Authenticate.login}
\title{
User login.
}
\description{
Logs the user into SciServer and returns the authentication token, which is then also set as the Sys environment variable "sciservertoken".
}
\usage{Authenticate.login(UserName, Password)}
\arguments{
  \item{UserName}{name of the user.}
  \item{Password}{password of the user.}
}
\value{Returns the authentication token (string).}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Authenticate.getKeystoneUserWithToken}}, \code{\link[SciServer]{Authenticate.getToken}}, \code{\link[SciServer]{Authenticate.setToken}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://portal.sciserver.org/login-portal/}{http://portal.sciserver.org/login-portal/}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\dontrun{Authenticate.login("MyName","MyPassword") }
}
\keyword{SciServer}
