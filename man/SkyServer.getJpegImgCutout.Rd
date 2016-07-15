\name{SkyServer.getJpegImgCutout}
\alias{SkyServer.getJpegImgCutout}
\title{
Get a sky image cutout from SDSS.
}
\description{
Gets a rectangular image cutout from a region of the sky in SDSS, centered at (\code{ra},\code{dec}).
}
\usage{SkyServer.getJpegImgCutout(ra, dec, scale=0.7, width=512, height=512, opt="", query="", token = NULL)}
\arguments{
  \item{ra}{Right Ascension of the image's center.}
  \item{dec}{Declination of the image's center.}
  \item{scale}{Scale of the image, measured in \code{[arcsec/pix]}}
  \item{width}{Width of the image, measured in \code{[pix]}}
  \item{height}{Height of the image, measured in \code{[pix]}}
  \item{opt}{Optional drawing options, expressed as concatenation of letters (string). The letters options are\cr 
              "G": Grid. Draw a N-S E-W grid through the center \cr
              "L": Label. Draw the name, scale, ra, and dec on image. \cr
              "P 	PhotoObj. Draw a small cicle around each primary photoObj. \cr
              "S: SpecObj. Draw a small square around each specObj. \cr
              "O": Outline. Draw the outline of each photoObj. \cr
              "B": Bounding Box. Draw the bounding box of each photoObj. \cr
              "F": Fields. Draw the outline of each field. \cr
              "M": Masks. Draw the outline of each mask considered to be important. \cr
              "Q": Plates. Draw the outline of each plate. \cr
              "I": Invert. Invert the image (B on W). \cr
  (see \href{http://skyserver.sdss.org/dr12/en/tools/chart/chartinfo.aspx}{http://skyserver.sdss.org/dr12/en/tools/chart/chartinfo.aspx})}
  \item{query}{Optional string. Marks with inverted triangles on the image the position of user defined objects. The (RA,Dec) coordinates of these object can be given by three means: \cr 
  1) query is a SQL command of format \code{"SELECT Id, RA, Dec, FROM Table"}.\cr
  2) query is list of objects. A header with RA and DEC columns must be included. Columns must be separated by tabs, spaces, commas or semicolons. The list may contain as many columns as wished. \cr
  3) aquery is a string following the pattern: ObjType Band (low_mag, high_mag).\cr
  ObjType: 	S | G | P  marks Stars, Galaxies or PhotoPrimary objects.\cr
  Band: 	U | G | R | I | Z | A 	restricts marks to objects with Band BETWEEN low_mag AND high_mag Band 'A' will mark all objects within the specified magnitude range in any band (ORs composition).\cr
  Examples:\cr
  S \cr
	S R (0.0, 23.5) \cr
	G A (20, 30) \cr
  (see \href{http://skyserver.sdss.org/dr12/en/tools/chart/chartinfo.aspx}{http://skyserver.sdss.org/dr12/en/tools/chart/chartinfo.aspx})\cr
  }
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
img <- SkyServer.getJpegImgCutout(ra=197.614455642896, dec=18.438168853724, width=512, height=512, scale=0.4, opt="OG", query="SELECT TOP 100 p.objID, p.ra, p.dec, p.r FROM fGetObjFromRectEq(197.6,18.4,197.7,18.5) n, PhotoPrimary p WHERE n.objID=p.objID")
plot(0:1, 0:1, type = "n")
rasterImage(img, 0, 0, 1, 1)
}
}
\keyword{SciServer}
