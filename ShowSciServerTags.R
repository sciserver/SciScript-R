#!/usr/bin/env Rscript
cat("\n---1) Updating local Git repository...\n\n")
system("git tag -d $(git tag)") #deletes local tags
system("git fetch -a -t -v") #fetches all remotes into local repo, including tags.
system("git reset --hard origin/master") #resets the local master branch to what was just fetched.

cat("\n--2) Listing available SciServer version Tags:\n\n")
tags = system("git tag", intern=TRUE)
if(length(tags)==0){
  cat("No SciServer Tags available.\n\n")
}else{
  system("git tag")
}