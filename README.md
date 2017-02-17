# SciScript-R
R libraries for SciScript, Jupyter Notebooks

1.- To check the code before building the R-package, run
      R CMD check SciScript-R

2.- To build and compress the R-package, run
      R CMD build SciScript-R
      
3.- To install the R-package, run
      R CMD INSTALL SciServer_VERSION.tar.gz  
    where the stringVERSION is the SciServer release tag. It's written automatically in the .gz file name, and you can find it in the           DESCRIPTION file.


To create HTML documentation, just run 'CreateHtmlDocs("SciServer")' within R, where the function 'CreateHtmlDocs' is defined in file CreateHtmlDocs.R.txt
