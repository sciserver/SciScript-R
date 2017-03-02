#!/usr/bin/env Rscript
commandLineArguments = commandArgs(trailingOnly=TRUE)

cat("\n---1) Updating local Git repository...\n\n")
system("git tag -d $(git tag)") #deletes local tags
system("git fetch --all") #fetches all remotes into local repo, including tags.
system("git checkout master")
system("git reset --hard origin/master") #resets the local master branch to what was just fetched.
system("git clean -df") #removes all untracked files
if (length(commandLineArguments) == 0) {

  cat("\n---2) Checking out latest SciScript code from local master branch...\n\n")
  system("git checkout master")

}else{

  sciserverTag = commandLineArguments[1]
  cat(paste("\n---2) Checking out latest SciScript code tagged as \"",sciserverTag,"\"...\n\n",sep=""))
  system(paste("git checkout tags/",sciserverTag,sep=""))
}

setwd("../")

cat(paste("\n---3) Building the SciServer package...\n\n"))
system("R CMD build SciScript-R")

packages = system("ls -t SciServer*.tar.gz",intern=TRUE) #this lists all packages from different versions that might have already been built.

cat(paste("\n---4) Installing the SciServer package from ",packages[1],"...\n\n",sep=""))
system(paste("R CMD INSTALL ",packages[1],sep=""))