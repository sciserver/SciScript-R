\name{SkyServer.rectangularSearch}
\alias{SkyServer.rectangularSearch}
\title{
Run a rectangular search on the sky.
}
\description{
Runs a query in the SDSS database that searches for all objects within a certain rectangular box defined on the the sky, and retrieves the result table as a dataframe.
}
\usage{SkyServer.rectangularSearch(min_ra, max_ra, min_dec, max_dec, coordType="equatorial", whichPhotometry="optical", limit="10", token=NULL)}
\arguments{
  \item{min_ra}{Minimum value of Right Ascension coordinate that defines the box boundaries on the sky.}
  \item{max_ra}{Maximum value of Right Ascension coordinate that defines the box boundaries on the sky.}
  \item{min_dec}{Minimum value of Declination coordinate that defines the box boundaries on the sky.}
  \item{max_dec}{Maximum value of Declination coordinate that defines the box boundaries on the sky.}
  \item{coordType}{Type of celestial coordinate system. Can be set to "equatorial" or "galactic".}
  \item{whichPhotometry}{Type of retrieved data. Can be set to "optical" or "infrared".}
  \item{limit}{maximum number of rows in the result table (string). If set to "0", then the function will return all rows.}
  \item{token}{Sciserver's authentication token for the user} 
}
\value{ the returned object is of type 'dataframe'.}
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
table <- SkyServer.rectangularSearch(min_ra=258.2, max_ra=258.3, min_dec=64,max_dec=64.1)
}
\keyword{SciServer}
