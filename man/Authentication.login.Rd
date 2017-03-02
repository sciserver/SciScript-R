\name{Authentication.login}
\alias{Authentication.login}
\title{
User login.
}
\description{
Logs the user into SciServer and returns the authentication token. This function is useful when SciScript-R library methods are executed outside the SciServer-Compute environement. In this case, the session authenticatin token does not exists (and therefore can't be automatically recognized), so the user has to use \code{Authentication.login} in order to log into SciServer manually and get the authentication token. \code{Authentication.login} also sets the token value in the Sys environment variable "sciservertoken" and as the variable \code{\link[SciServer]{Authentication.token}}.
}
\usage{Authentication.login(UserName, Password)}
\arguments{
  \item{UserName}{name of the user.}
  \item{Password}{password of the user.}
}
\value{Returns the authentication token (string). Throws an exception if the HTTP request to the Authentication URL returns an error.}
\author{
Gerard Lemson, Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Authentication.getKeystoneUserWithToken}}, \code{\link[SciServer]{Authentication.getToken}}, \code{\link[SciServer]{Authentication.setToken}}, \code{\link[SciServer]{Authentication.token}}.
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
