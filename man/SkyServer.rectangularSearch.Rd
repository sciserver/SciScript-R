\name{SkyServer.rectangularSearch}
\alias{SkyServer.rectangularSearch}
\title{
Run a rectangular search on the sky.
}
\description{
Runs a query in the SDSS database that searches for all objects within a certain rectangular box defined on the the sky, and retrieves the result table as a dataframe. Maximum number of rows retrieved is set currently to 500,000.
}
\usage{SkyServer.rectangularSearch(min_ra, max_ra, min_dec, max_dec, coordType="equatorial", whichPhotometry="optical", limit="0", dataRelease=NULL)}
\arguments{
  \item{min_ra}{Minimum value of Right Ascension coordinate that defines the box boundaries on the sky.}
  \item{max_ra}{Maximum value of Right Ascension coordinate that defines the box boundaries on the sky.}
  \item{min_dec}{Minimum value of Declination coordinate that defines the box boundaries on the sky.}
  \item{max_dec}{Maximum value of Declination coordinate that defines the box boundaries on the sky.}
  \item{coordType}{Type of celestial coordinate system. Can be set to "equatorial" or "galactic".}
  \item{whichPhotometry}{Type of retrieved data. Can be set to "optical" or "infrared".}
  \item{limit}{Maximum number of rows in the result table (string). If set to "0", then the function will return all rows.}
  \item{dataRelease}{SDSS data release. E.g, 'DR13'}  
}
\value{ Returns the results table as a dataframe. Throws an exception if the HTTP request to the SkyServer API returns an error.}
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\seealso{
\code{\link[SciServer]{SkyServer.sqlSearch}}, \code{\link[SciServer]{SkyServer.radialSearch}}.
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org}{http://skyserver.sdss.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
table = SkyServer.rectangularSearch(min_ra=258.2, max_ra=258.3, min_dec=64,max_dec=64.1)
}
\keyword{SciServer}
