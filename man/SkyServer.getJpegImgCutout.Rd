\name{SkyServer.getJpegImgCutout}
\alias{SkyServer.getJpegImgCutout}
\title{
Get a sky image cutout from SDSS.
}
\description{
Gets a rectangular image cutout from a region of the sky in SDSS, centered at (\code{ra},\code{dec}).
}
\usage{SkyServer.getJpegImgCutout(ra, dec, scale=0.7, width=512, height=512, token=NULL)}
\arguments{
  \item{ra}{Right Ascention of the image's center.}
  \item{dec}{Declination of the image's center.}
  \item{scale}{scale of the image, mesured in \code{[arcsec/pix]}}
  \item{width}{Width of the image, mesured in \code{[pix]}}
  \item{height}{Height of the image, mesured in \code{[pix]}}
  \item{token}{Sciserver's authentication token for the user} 
}
\value{ the returned object is an array of doubles, of dimensions  [1:\code{width},1:\code{height},1:3].  }
\author{
Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://skyserver.sdss.org}{http://skyserver.sdss.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\examples{
\dontrun{
img <- SkyServer.getJpegImgCutout(ra=197.614455642896, dec=18.438168853724, width=512, height=512, scale=0.4)
plot(0:1, 0:1, type = "n")
rasterImage(img, 0, 0, 1, 1)
}
}
\keyword{SciServer}
