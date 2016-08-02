# SciScript-R
R libraries for SciScript, Jupyter Notebooks

1.- To check the code before building the R-package, run
      R CMD check SciScript-R

2.- To build and compress the R-package, run
      R CMD build SciScript-R
      
3.- To install the R-package, run
      R CMD INSTALL SciScript_1.0.tar.gz


To create HTML documentation, just run 'CreateHtmlDocs("SciServer")' within R, where the function 'CreateHtmlDocs'  is defined by 

CreateHtmlDocs  = function(packageName) {
    workingDirectory = getwd()
    helpDirectory = system.file('html', package = packageName)
    setwd(helpDirectory)
    links = tools::findHTMLlinks()
    helpDoc = tools:::fetchRdDB(file.path(find.package(packageName),'help', packageName))
    docNames = names(helpDoc)
    for (doc in docNames) 
    {
        tools::Rd2HTML(helpDoc[[doc]], paste(doc, 'html', sep = '.'), package = packageName, Links = links, no_links = is.null(links))
    }
    setwd(workingDirectory)
    message("HTML cocuments created in ", helpDirectory)
}
