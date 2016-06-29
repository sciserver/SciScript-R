\name{LoginPortal.login}
\alias{LoginPortal.login}
\title{
User login.
}
\description{
Logs the user into SciServer and returns the authentication token, which is then also set as the Sys environment variable "sciservertoken".
}
\usage{LoginPortal.login(UserName, Password)}
\arguments{
  \item{UserName}{name of the user.}
  \item{Password}{password of the user.}
}
\value{Returns the authentication token (string).}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{LoginPortal.getKeystoneUserWithToken}}, \code{\link[SciServer]{LoginPortal.getToken}}, \code{\link[SciServer]{LoginPortal.setToken}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\dontrun{LoginPortal.login("MyName","MyPassword") }
}
\keyword{SciServer}
