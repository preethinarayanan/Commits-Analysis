#loading libraries

library(dplyr)
library(magrittr)
library(ggplot2)
library(flexdashboard)
library(highcharter)
library(viridis) 
library(tidyverse)
library(plotly)
library(magrittr)
library(DT)
library(lubridate)
library(zoo)

files <-list.files(path="C:/Users/pnarayanan/Documents/projects/bitbucket", full.names=TRUE, recursive=FALSE)


my_repos_list <- files[c(1:45)]


#get git log

git_log <- function(n){
  gitlog <- purrr::map(c("%cn", "%h", "%ci"), function(i) paste0('git log --all --pretty=format:', i)) %>%
    purrr::map(system, intern = T)
  tibble::tibble(commiter = gitlog[[1]],
                 commit_hash = gitlog[[2]],
                 commit_date = gitlog[[3]] %>% stringr::str_replace_all(., pattern = " -0800", ""))  
  
} 

# git log for all repos in the directory

print_list <- list()

all_repo <- function(){
  
  print_list <- list()
  
  for (i in my_repos_list){
    
    setwd(i)
    
    tibble1 <- git_log()
    
    tibble1$repo <- i
    
    print_list[[i]] <- tibble1
    
   
  }
  
}


for (i in my_repos_list){
  
  setwd(i)
  
  tibble1 <- git_log()
  
  tibble1$repo <- i
  
  print_list[[i]] <- tibble1
  
  
}




for (i in my_repos_list){
  
  setwd(i)
  
  tibble1 <- git_log()
  
  tibble1$repo <- i
  
  print_list[[i]] <- tibble1
  
  
}


# data manipulation

output2 <- data.table::rbindlist(print_list)

a <- output2


a$repo <- sub('.*/', '', a$repo)


a$commit_date <- lubridate::as_date(a$commit_date)


test <- a %>% select(commiter, commit_hash, commit_date, repo) %>% group_by(commit_date,commiter) %>% tally() %>% mutate(commit_no = n) %>% distinct()


P <- c("Preethi Narayanan", "PREETHI NARAYANAN")

test1_p <- test %>% filter(commiter %in% P)

J <- c("Jason Perone")

test2_j <- test %>% filter(commiter %in% J)

E <- c("Eric Lessert", "e_lessert", "elessert760")

test3_e <- test %>% filter(commiter %in% E)

M <- c("Matthew Johns", "matthewjohns")

test4_m <- test %>% filter(commiter %in% M)

K <- c("Kristine Dinh", "kristine", "dinhkristine", "dinhth15")

test5_k <- test %>% filter(commiter %in% K)

L <- c("Â–LauraHuang_AGIA", "Laura Huang", "LauraHuang_AGIA")

test6_l <- test %>% filter(commiter %in% L)

N <- c("Nicole Moeller")

test7_n <- test %>% filter(commiter %in% N)


x <- c("Commit Date", "Number of Commits per day", 
       "Commiter")
y <- sprintf("{point.%s}", c("commit_date", "commit_no", "commiter"))

tltip <- tooltip_table(x,y)


highchart(type='stock') %>%
  hc_add_series(test1_p, hcaes(x = commit_date, y = commit_no), type= 'line', color = 'red', name = "Preethi Narayanan") %>%
  hc_add_series(test2_j, hcaes(x = commit_date, y = commit_no), type = "line", color= 'black', name = "Jason Perone") %>% 
  hc_add_series(test3_e, hcaes(x = commit_date, y = commit_no), type = "line", color= 'blue', name = "Eric Lessert") %>% 
  hc_add_series(test4_m, hcaes(x = commit_date, y = commit_no), type = "line", color= 'green', name = "Matthew Johns") %>% 
  hc_add_series(test5_k, hcaes(x = commit_date, y = commit_no), type = "line", color= 'pink', name = "Kristine Dinh") %>% 
  hc_add_series(test6_l, hcaes(x = commit_date, y = commit_no), type = "line", color= 'orange', name = "Laura Huang") %>% 
  hc_add_series(test7_n, hcaes(x = commit_date, y = commit_no), type = "line", color= 'purple', name = "Nicole Moeller") %>% 
  hc_yAxis(title = list(text = "Count of Commits")) %>%
  hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %d}'), title = list(text = "Commit Date")) %>%
  hc_legend(enabled = TRUE) %>% 
  hc_tooltip(useHTML = TRUE, pointFormat = tltip, split = FALSE) 
