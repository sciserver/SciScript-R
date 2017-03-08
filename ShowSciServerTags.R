#!/usr/bin/env Rscript

cat("\n---1) Updating local Git repository...\n\n")
system("git tag -d $(git tag)") #deletes local tags
system("git fetch --all") #fetches all remotes into local repo, including tags.
system("git checkout master")

cat("\n--2) Listing available SciServer version tags:\n\n")
tags = system('git tag --list "*sciserver*"', intern=TRUE)
if(length(tags)==0){
  cat("No SciServer Tags available.\n\n")
}else{
  system('git tag --list "*sciserver*"')
}
cat("\n*** Refer to http://www.sciserver.org/support/updates for particular release tag details.\n\n")