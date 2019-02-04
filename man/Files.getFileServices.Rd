\name{Files.getFileServices}
\alias{Files.getFileServices}
\title{
Get FileServices
}
\description{
Gets the definitions of file services that a user is able to access. A FileService represents a file system that contains root volumes accessible to the user for public/private data storage. Within each rootVolume, users can create sharable userVolumes for storing files.
}
\usage{Jobs.getFileServices(verbose=TRUE)}
\arguments{
  \item{verbose}{boolean parameter defining whether warnings will be printed (set to TRUE) or not (set to FALSE).}
}
\value{list that contains lists, each of which representing the description of a FileService that the user is able to access.}
\author{
Manuchehr Taghizadeh-Popp \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{Files.getFileServiceFromName}}
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://apps.sciserver.org/}{http://apps.sciserver.org}\cr
\href{http://apps.sciserver.org/dashboard/files}{http://apps.sciserver.org/dashboard/files}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
fileServices = Files.getFileServices()
}
\keyword{SciServer}