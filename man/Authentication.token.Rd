\name{Authentication.token}
\alias{Authentication.token}
\title{
Stores the SciServer user authentication token.
}
\description{
String variable that stores the authentication token for the user's session. It has an initially a \code{NULL} value, and gets set directly by running \code{\link[SciServer]{Authentication.setToken}}, and indirectly by running \code{\link[SciServer]{Authentication.login}} or \code{\link[SciServer]{Authentication.getToken}}.
}
\usage{Authentication.token}
\value{Stores the authentication token (string). If never set, it stores a \code{NULL} value.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
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
\dontrun{myToken = Authentication.token }
}
\keyword{SciServer}
