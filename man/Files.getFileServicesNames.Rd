\name{Files.getFileServicesNames}
\alias{Files.getFileServicesNames}
\title{
Get FileServices Names
}
\description{
Returns the names and description of the fileServices available to the user.
}
\usage{Jobs.getFileServicesNames(fileServices=NULL, verbose=TRUE)}
\arguments{
  \item{fileServices}{a list of FileService objects, as returned by Files.getFileServices(). If not set, then an extra internal call to Jobs.getFileServices() is made.}
  \item{verbose}{boolean parameter defining whether warnings will be printed (set to TRUE) or not (set to FALSE).}
}
\value{a list of lists, each of which having the name and description of a file service available to the user.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Files.getFileServices}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://apps.sciserver.org/dashboard/files}{http://apps.sciserver.org/dashboard/files}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
fileServiceNames = Files.getFileServicesNames()
}
\keyword{SciServer}
