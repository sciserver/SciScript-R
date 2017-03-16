\name{SciServer}
\alias{SciServer}
\docType{package}
\title{
Access to SciServer tools.
}
\description{
This package provides functions for quick access of SciServer web services and tools. \cr
SciServer (\href{www.sciserver.org}{www.sciserver.org}) provides a new online framework for data-intensive scientifc computing in the cloud, where the motto is to bring the computation close where the data is stored, and allow seamless access and sharing of big data sets within the scientific community.
}

\details{
Some SciServer tools you can access with this package:


\href{http://skyserver.sdss.org/casjobs/}{CasJobs}: Database storage and querying. \cr
     You can have access big databases, as well as save your data tables in your own database called 'MyDB'. 
     The user can run synchronous or asynchronous SQL queries and get the result back as an R data-frame (using \code{\link[SciServer]{CasJobs.executeQuery}} or \code{\link[SciServer]{CasJobs.submitJob}}, respectively). Uploading of CSV files or R data-frames into a database table can be done using \code{\link[SciServer]{CasJobs.uploadCSVToTable}} and \code{\link[SciServer]{CasJobs.uploadDataFrameToTable}}, respectively.\cr

\href{http://portal.sciserver.org/login-portal/}{Login Portal}: Single sign-on portal to all SciServer applications. \cr
      Although some tools accept anonymous access, you can use  \code{\link[SciServer]{Authentication.login}} to login and access your own data and environment (after registering in the \href{http://portal.sciserver.org/login-portal/}{Login Portal}).    \cr

\href{http://scidrive.org/scidrive/}{SciDrive}: Drag-and-drop file storage and sharing.\cr
      You can create directories in SciDrive using \code{\link[SciServer]{SciDrive.createContainer}}, upload a file to SciDrive using \code{\link[SciServer]{SciDrive.upload}}, and share its URL with your collaborators by using \code{\link[SciServer]{SciDrive.publicUrl}}. \cr

\href{http://skyserver.sdss.org/}{SkyServer}: Access to the \href{http://www.sdss.org/}{SDSS} astronomical survey. \cr
      You can query the SDSS database using \code{\link[SciServer]{SkyServer.sqlSearch}}, get cutout images from the sky using \code{\link[SciServer]{SkyServer.getJpegImgCutout}}.  \cr




\tabular{ll}{
Package: \tab SciServer\cr
Type: \tab Package\cr
Version: \tab 1.10.0\cr
}}
\author{
Manuchehr Taghizadeh-Popp, Gerard Lemson \cr
Maintainer: Manuchehr Taghizadeh-Popp <mtaghiza@jhu.edu>
}
\references{
\href{http://www.sciserver.org}{http://www.sciserver.org}\cr
\href{http://www.github.com/sciserver/SciScript-R}{http://www.github.com/sciserver/SciScript-R}
}
\keyword{ package }
\seealso{ }
