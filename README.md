# SciScript-R

## R libraries for Jupyter Notebooks

This R package provides functions for quick access of [SciServer](http://www.sciserver.org) APIs (web services) and tools.
[SciServer](http://www.sciserver.org) provides a new online framework for data-intensive scientifc computing in the cloud,
where the motto is to bring the computation close where the data is stored, and allow seamless access and sharing of big data sets within the scientific community.

Some SciServer tools you can access with this package:

 * [Login Portal](http://portal.sciserver.org): Single sign-on portal to all SciServer applications.

 * [CasJobs](http://skyserver.sdss.org/CasJobs): Database storage and querying.

 * [SciDrive](http://www.scidrive.org/): Drag-and-drop file storage and sharing.

 * [SkyServer](http://skyserver.sdss.org/): Access to the SDSS astronomical survey.

 * [SkyQuery](http://www.voservices.net/skyquery): Cross-match of astronomical source catalogs.

Maintainer: Manuchehr Taghizadeh-Popp.

Authors: Gerard Lemson, Manuchehr Taghizadeh-Popp.


## 1) Cloning the code locally:
    
1.1.- Run `git clone http://github.com/sciserver/SciScript-R.git`

## 2) Setting configuration parameters:

2.1.- Open `./R/Config.py` and edit the API URLs and parameters to match those of the SciServer tools and installation, according to the instructions and descriptions found therein.

## 3) Installation:

There are 2 possibilities: automatic or manual installation.

### a) Automatic Installation and Update:

3.a.1.- Run `Rscript ShowSciServerTags.R` in order to see the version tags that label each SciServer release containing new SciScript code.

3.b.2.- Run `Rscript Install.R tag`, where `tag` is the version of the SciServer release containing the SciScript version you want to install or update to (see previous step). If `tag` is not specified, then the latest version will be installed.

### b) Manual Installation.

3.b.1.- To check the code before building the R-package, run
    `R CMD check SciScript-R`

3.b.2.- To build and compress the R-package, run
    `R CMD build SciScript-R`
      
3.b.3.- To install the R-package, run
    `R CMD INSTALL SciServer_VERSION.tar.gz`, 
    where `SciServer_VERSION.tar.gz` was created in step 2. The string `VERSION` is the SciServer release tag, which is written automatically in the .gz file name, and you can find it in the DESCRIPTION file.

## 4) Creating HTML documentation:

4.1.- Run `Rscript CreateHtmlDocs.R dir`, where `dir` is the directory where the html files will be created. If `dir` is not specified, then the html files will be created in `./html/`

## 5) Unit Tests:

5.1.- Open `./Tests/RunUnitTests.R` and edit the `Authentication_loginName` and `Authentication_loginPassword` parameters in order to run the Tests under the credentials of a (test) user.

5.2.- Run `Rscript RunUnitTests.R` while in the `./Tests` directory in order to run the unit tests for the SciScript-R modules. Be sure all that all tests end with an `done successfully` status.

## 6) Examples.

6.1.- In the directory `./Examples` you can find python scripts or Jupyter notebooks that will run sample code using SciScript-R modules and methods.
