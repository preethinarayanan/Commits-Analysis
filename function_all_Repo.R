getwd()

setwd(here::here())

library(dplyr)
library(magrittr)

source("libraries.R")

#' Title
#'
#' @param repo_name 
#'
#' @return
#' @export
#'
#' @examples
#' 
#' 
#' 

git_summary <- function(repo_name){
  
  #setwd(repo_name)
  
  # system("git pull")
  term_out <- system('git log --pretty=format:"%cn committed %h on %cd')
  
  
  #write.table(text = term_out,con = "path/to/central/file/location.txt")
}


git_summary('bitbucket-api')

git_log <- function(){
  #for (i in seq_along(files)) {
  gitlog <- purrr::map(c("%cn", "%h", "%cd", "%s"), function(i) paste0('git log --pretty=format:', i)) %>%
    purrr::map(system, intern = T)
  tibble::tibble(commiter = gitlog[[1]],
                 commit_hash = gitlog[[2]],
                 commit_date = gitlog[[3]] %>% stringr::str_replace_all(., pattern = " -0800", ""),
                 commit_message = gitlog[[4]]) %>%
  
    tidyr::separate(., commit_date, sep = "[:space:]+", into = c("weekday", "month", "day_of_month", "time", "year"))
} 


for (i in seq_along(files)) {
  #term_out <- system('git log --pretty=format:"%cn committed %h on %cd')
  
  #a <- purrr::map(c("%cn", "%h", "%cd", "%s"), paste0('git log --pretty=format:', i))
  df = print(git_log())
  
  #a = as.character(df)
  #writeLines(df, 'C:/Users/pnarayanan/Documents/projects/bitbucket/bits.txt')
  
}



my_list = 'C:/Users/pnarayanan/Documents/projects/bitbucket'

for(i in my_list){
  print(git_summary(i))
}


my_plot_function <- function(filepath){
  
  readLines("path/to/central/file/location.txt")
  
  # do 
  # fancy
  # plotting
  # here
  
}

files <- list.files(path="C:/Users/pnarayanan/Documents/projects/bitbucket", full.names=TRUE, recursive=FALSE)


setwd('C:/Users/pnarayanan/Documents/projects/bitbucket/project-sigma')
  

for (i in (files)) {
  term_out <- system('git log --pretty=format:"%cn %h on %ci')
  
  #a <- purrr::map(c("%cn", "%h", "%cd", "%s"), paste0('git log --pretty=format:', i))
  #df = print(term_out)
  
  #a = as.character(df)
  writeLines(a, 'C:/Users/pnarayanan/Documents/projects/bitbucket/bits.txt')
  
}

  
library(tidyverse)
f_summary <- 
 files %>% 
  map(git_summary(files))


for(i in files){
  print(git_summary(i))
}

i=3
#git_summary(files)


system('git log --format=%cd')


system('git shortlog -s -n --all --no-merges')

system("git --no-pager show -s --format='%an <%ae>' COMMIT")

system("git shortlog -snd --all")

for (i in (files)) {
  print(files)
}

