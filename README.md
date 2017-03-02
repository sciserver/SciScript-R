# SciScript-R

## R libraries for SciScript, Jupyter Notebooks


### Cloning the code locally:
    `git clone http://github.com/sciserver/SciScript-R.git`

### Manual Installation Process:

1.- To check the code before building the R-package, run
    `R CMD check SciScript-R`

2.- To build and compress the R-package, run
    `R CMD build SciScript-R`
      
3.- To install the R-package, run
    `R CMD INSTALL SciServer_VERSION.tar.gz`, 
    where `SciServer_VERSION.tar.gz` was created in step 2. The string `VERSION` is the SciServer release tag, which is written automatically in the .gz file name, and you can find it in the DESCRIPTION file.


### Automatic Installation process:
  
1.- Run `Rscript ShowScuiserverTags.R` in order to see the version tags that label each SciServer release containing new SciScript code.

2.- Run `Rscript Update.R tag`, where `tag` is the version of the SciServer release containing the SciScript version you want to install or update to (see previous step). If `tag` is not specified, then the latest version will be installed.


### Creating HTML documentation:

1.- Run `Rscript CreateHtmlDocs.R dir`, where `dir` is the directory where the html files will be created. If `dir` is not specified, then the html files will be created in `./html/`


