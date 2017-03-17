\name{SkyServer.radialSearch}
\alias{SkyServer.radialSearch}
\title{
Run a radial search on the sky.
}
\description{
Runs a query in the SDSS database that searches for all objects within a certain radius from a point in the sky, and retrieves the result table as a dataframe. Maximum number of rows retrieved is set currently to 500,000.
}
\usage{SkyServer.radialSearch(ra, dec, radius=1, coordType="equatorial", whichPhotometry="optical", limit="0", dataRelease=NULL)}
\arguments{
  \item{ra}{Right Ascension of the point in the sky.}
  \item{dec}{Declination of the point in the sky.}
  \item{radius}{Search radius around the (ra,dec) coordinate in the sky. Measured in arcminutes.}
  \item{coordType}{Type of celestial coordinate system. Can be set to "equatorial" or "galactic".}
  \item{whichPhotometry}{Type of retrieved data. Can be set to "optical" or "infrared".}
  \item{limit}{maximum number of rows in the result table (string). If set to "0", then the function will return all rows.}
  \item{dataRelease}{SDSS data release. E.g, 'DR13'}  
}
\value{ Returns the results table as a dataframe. Throws an exception if the HTTP request to the SkyServer API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyServer.sqlSearch}}, \code{\link[SciServer]{SkyServer.rectangularSearch}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org}{http://skyserver.sdss.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
table = SkyServer.radialSearch(ra=258.25, dec=64.05, radius=3)
}
\keyword{SciServer}
