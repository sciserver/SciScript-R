\name{Files.getFileServiceFromName}
\alias{Files.getFileServiceFromName}
\title{
Get FileService from name
}
\description{
Returns a FileService object, given its registered name.
}
\usage{Jobs.getFileServiceFromName(fileServiceName, fileServices=NULL, verbose=TRUE)}
\arguments{
  \item{fileServiceName}{name of the FileService, as shown within the results of Files.getFileServices()}
  \item{fileServices}{a list of FileService objects (lists), as returned by Files.getFileServices(). If not set, then an extra internal call to Jobs.getFileServices() is made.}
  \item{verbose}{boolean parameter defining whether warnings will be printed (set to TRUE) or not (set to FALSE).}
}
\value{a FileService object (list) that defines a FileService. A list of these kind of objects available to the user is returned by the function Jobs.getFileServices(). If no fileService can be found, then returns NULL.}
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
fileService = Files.getFileServiceFromName('FileServiceAtJHU');
}
\keyword{SciServer}