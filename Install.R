#!/usr/bin/env Rscript
commandLineArguments = commandArgs(trailingOnly=TRUE)

cat("\n---1) Updating local Git repository...\n\n")
system("git tag -d $(git tag)") #deletes local tags
system("git fetch --all") #fetches all remotes into local repo, including tags.
system("git checkout master")
system("git reset --hard origin/master") #resets the local master branch to what was just fetched.
system("git clean -df") #removes all untracked files

system("cp -f Install.R ../Install_IntermediateCopy5551234.R") #copies the install file one level up, so that if the commit checked out in step 2) does not have it, then we can copy it back in there.

if (length(commandLineArguments) == 0) {

  cat("\n---2) Checking out latest SciScript code from local master branch...\n\n")
  system("git checkout master")

}else{

  sciserverTag = commandLineArguments[1]
  cat(paste("\n---2) Checking out latest SciScript code tagged as \"",sciserverTag,"\"...\n\n",sep=""))
  system(paste("git checkout tags/",sciserverTag,sep=""))
}

hasInstallFile = system('ls Install.R', intern=TRUE)
if(length(hasInstallFile) > 0){
  os.system("rm -f ../Install_IntermediateCopy5551234.R") #removes the copy of the install file one level up
else{
  os.system("mv -f ../Install_IntermediateCopy5551234.R ./Install.R") #copies the install file back from one level up
}

setwd("../")

cat(paste("\n---3) Building the SciServer package...\n\n"))
system("R CMD build SciScript-R")

packages = system("ls -t SciServer*.tar.gz",intern=TRUE) #this lists all packages from different versions that might have already been built.

cat(paste("\n---4) Installing the SciServer package from ",packages[1],"...\n\n",sep=""))
system(paste("R CMD INSTALL ",packages[1],sep=""))