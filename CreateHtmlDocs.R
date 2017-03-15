#!/usr/bin/env Rscript
CreateHtmlDocs  = function(packageName, helpDirectory = NULL) {
    workingDirectory = getwd()
    if(is.null(helpDirectory) || helpDirectory == ""){
      system("mkdir htmlDocs")
      helpDirectory = "./htmlDocs/" #system.file('htmlDocs', package = packageName)
    }else{
      system(paste("mkdir -p ",helpDirectory,sep=""))
    }
    setwd(helpDirectory)
    htmldir = getwd()
    
    links = tools::findHTMLlinks()
    helpDoc = tools:::fetchRdDB(file.path(find.package(packageName),'help', packageName))
    docNames = names(helpDoc)
    for (doc in docNames) 
    {
        tools::Rd2HTML(helpDoc[[doc]], paste(doc, 'html', sep = '.'), package = packageName, Links = links, no_links = is.null(links))
    }
    
    setwd(workingDirectory)
    
    message("SciServer HTML documents created in ", htmldir)
}


commandLineArguments = commandArgs(trailingOnly=TRUE)

if (length(commandLineArguments) == 0) {
  CreateHtmlDocs("SciServer")
}else{
  dir = commandLineArguments[1]
  CreateHtmlDocs("SciServer", dir)
}
