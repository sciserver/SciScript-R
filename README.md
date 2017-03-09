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

### Cloning the code locally:
    git clone http://github.com/sciserver/SciScript-R.git

### Manual Installation Process:

1.- To check the code before building the R-package, run
    `R CMD check SciScript-R`

2.- To build and compress the R-package, run
    `R CMD build SciScript-R`
      
3.- To install the R-package, run
    `R CMD INSTALL SciServer_VERSION.tar.gz`, 
    where `SciServer_VERSION.tar.gz` was created in step 2. The string `VERSION` is the SciServer release tag, which is written automatically in the .gz file name, and you can find it in the DESCRIPTION file.


### Automatic Installation process:
  
1.- Run `Rscript ShowSciServerTags.R` in order to see the version tags that label each SciServer release containing new SciScript code.

2.- Run `Rscript Install.R tag`, where `tag` is the version of the SciServer release containing the SciScript version you want to install or update to (see previous step). If `tag` is not specified, then the latest version will be installed.


### Creating HTML documentation:

1.- Run `Rscript CreateHtmlDocs.R dir`, where `dir` is the directory where the html files will be created. If `dir` is not specified, then the html files will be created in `./html/`


