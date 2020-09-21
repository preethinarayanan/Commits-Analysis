setwd(here::here())

library(dplyr)
library(magrittr)

source("libraries.R")


my_list = 'C:/Users/pnarayanan/Documents/projects/bitbucket'

files <-list.files(path="C:/Users/pnarayanan/Documents/projects/bitbucket", full.names=TRUE, recursive=FALSE)

repo_name = files



git_summary <- function(repo_name){
  
  #setwd(repo_name)
  
  # system("git pull")
  term_out <- system('git log --pretty=format:"%cn %h on %ci')
  
  
  #write.table(text = term_out,con = "path/to/central/file/location.txt")
}


git_summary("bitbucket-api")
 git_summary(files)

my_list = 'C:/Users/pnarayanan/Documents/projects/bitbucket'

files <-list.files(path="C:/Users/pnarayanan/Documents/projects/bitbucket", full.names=TRUE, recursive=FALSE)

#repo_name = files

for (i in files){
  a = git_summary(i)
  print(a)
}

