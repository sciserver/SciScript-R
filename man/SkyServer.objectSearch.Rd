\name{SkyServer.objectSearch}
\alias{SkyServer.objectSearch}
\title{
Searches for an object in the SDSS catalog.
}
\description{
Searches for an object in the SDSS catalog and gets its properties and associated metadata. Search parameters include position in the sky and various identifiers included in SDSS catalog tables.
}
\usage{SkyServer.objectSearch(objId=NULL, specObjId=NULL, apogee_id=NULL, apstar_id=NULL, ra=NULL, dec=NULL, plate=NULL, mjd=NULL, fiber=NULL, run=NULL, rerun=NULL, camcol=NULL, field=NULL, obj=NULL, dataRelease=NULL)}

\arguments{
  \item{objId}{SDSS ObjId.}
  \item{specObjId}{SDSS SpecObjId.}
  \item{apogee_id}{ID identifying Apogee target object.}
  \item{apstar_id}{unique ID for combined apogee star spectrum.}
  \item{ra}{Right Ascension of the object.}
  \item{dec}{Declination of the object.}
  \item{plate}{SDSS plate number.}
  \item{mjd}{Modified Julian Date of observation.}
  \item{fiber}{SDSS fiber number.}
  \item{run}{SDSS run number.}
  \item{rerun}{SDSS rerun number.}
  \item{camcol}{SDSS camera column.}
  \item{field}{SDSS field number.}
  \item{obj}{The object id within a field.}
  \item{dataRelease}{SDSS data release. E.g, 'DR13'}
}
\value{ Returns an object containing various tables with the objects properties and associated metadata. Throws an exception if the HTTP request to the SkyServer API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyServer.sqlSearch}}, \code{\link[SciServer]{SkyServer.rectangularSearch}}, \code{\link[SciServer]{SkyServer.radialSearch}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org}{http://skyserver.sdss.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
object <- SkyServer.objectSearch(ra=258.25, dec=64.05)
}
\keyword{SciServer}
