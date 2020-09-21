library(dplyr)
library(magrittr)
library(ggplot2)
#library(flexdashboard)
library(highcharter)
library(viridis) 
library(tidyverse)
#library(rjson)
library(plotly)
#library(quivR)
library(magrittr)
#library(crosstalk)
#library(DT)
library(lubridate)
library(zoo)

#getwd()
files <-list.files(path="C:/Users/pnarayanan/Documents/projects/bitbucket", full.names=TRUE, recursive=FALSE)

#my_repos_list <- files[c(5,6,7,9,16,17, 18,26,29,30,31,33,36)]
#my_repos_list <- files[c(5,6,7)]
my_repos_list <- files[c(1:40)]


git_log <- function(n){
  #for (i in seq_along(files)) {
  gitlog <- purrr::map(c("%cn", "%h", "%ci"), function(i) paste0('git log --all --pretty=format:', i)) %>%
    purrr::map(system, intern = T)
  tibble::tibble(commiter = gitlog[[1]],
                 commit_hash = gitlog[[2]],
                 commit_date = gitlog[[3]] %>% stringr::str_replace_all(., pattern = " -0800", ""))  
  
} 

git_log()
all_repo <- function(){
  
  print_list <- list()
  
  for (i in my_repos_list){
    
    setwd(i)
    
    # print(git_log())
    
    tibble1 <- git_log()
    
    tibble1$repo <- i
    
    print_list[[i]] <- tibble1
    
    #print_list$repo <- i
    #saveRDS(git_log(i),file = paste(git_log(i),'.RDS'),compress=TRUE)
    
    #saveRDS(print_list[i], "C:/Users/pnarayanan/Documents/projects/bitbucket/git.RDS")
    
  }
  
}


# print_list <- list()

# for (i in my_repos_list){
#   
#   setwd(i)
#   
#   # print(git_log())
#   
#   tibble1 <- git_log()
#   
#   tibble1$repo <- i
#   
#   print_list[[i]] <- tibble1
#   
#   #print_list$repo <- i
#   #saveRDS(git_log(i),file = paste(git_log(i),'.RDS'),compress=TRUE)
#   
#   #saveRDS(print_list[i], "C:/Users/pnarayanan/Documents/projects/bitbucket/git.RDS")
#   
# }

tibble1 <- git_log()

tibble1$repo <- i

print_list[[i]] <- tibble1


#print_list3 <- all_repo()

#output <- data.table::rbindlist(print_list3)

output2 <- data.table::rbindlist(print_list)

a <- output2


a$repo <- sub('.*/', '', a$repo)

a$repo

#saveRDS(a, "C:/Users/pnarayanan/Documents/projects/bitbucket/gitlog.RDS")


#a <- readRDS("C:/Users/pnarayanan/Documents/projects/bitbucket/gitlog.RDS")

a$commit_date <- lubridate::as_date(a$commit_date)

#a$commit_hash <- as.integer(a$commit_hash)

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
  hc_tooltip(useHTML = TRUE, pointFormat = tltip, split = FALSE) # %>% 
#hc_add_theme(custom_theme)




#highchart() %>%
 # hc_add_series(test1, hcaes(x = commit_date, y = commit_no), type= 'line', color = 'red', name = "Commiter:Preethi Narayanan") %>%
  #hc_add_series(test2, hcaes(x = commit_date, y = commit_no), type = "line", color= 'black', name = "Commiter:Kristine Dinh") %>% 
  #hc_add_series(test3, hcaes(x = commit_date, y = commit_no), type = "line", color= 'blue', name = "Commiter:Jason Perone") %>% 
  #hc_add_series(test4, hcaes(x = commit_date, y = commit_no), type = "line", color= 'green', name = "Commiter:Eric Lessert") %>% 
  #hc_add_series(test5, hcaes(x = commit_date, y = commit_no), type = "line", color= 'yellow', name = "Commiter:Matthew Johns") %>% 
  #hc_yAxis(title = list(text = "Count of Commits")) %>%
  #hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %d}'), title = list(text = "Commit Date")) %>%
  #hc_legend(enabled = TRUE) %>% 
  #hc_tooltip(useHTML = TRUE, pointFormat = tltip, split = FALSE) # %>% 
#hc_add_theme(custom_theme)


ggplot(data = test) + 
 geom_line(mapping = aes(x = commit_date, y = commit_no, color = commiter)) +
  # geom_col() +
  theme_minimal()



plot(test$commit_date, test$commit_no, col = 'green',type = 'l')

### Plot 4 - Commit log Per day

# highchart(type='stock') %>%
#   hc_add_series(test1, hcaes(x = commit_date, y = commit_no), type= 'line', color = 'red', name = "Preethi Narayanan") %>%
#   hc_add_series(test2, hcaes(x = commit_date, y = commit_no), type = "line", color= 'black', name = "PREETHI NARAYANAN") %>% 
#   hc_add_series(test3, hcaes(x = commit_date, y = commit_no), type = "line", color= 'blue', name = "Jason Perone") %>% 
#   hc_add_series(test4, hcaes(x = commit_date, y = commit_no), type = "line", color= 'green', name = "Eric Lessert") %>% 
#   hc_add_series(test5, hcaes(x = commit_date, y = commit_no), type = "line", color= 'yellow', name = "e_lessert") %>% 
#   hc_add_series(test5, hcaes(x = commit_date, y = commit_no), type = "line", color= 'yellow', name = "elessert760") %>% 
#   hc_add_series(test5, hcaes(x = commit_date, y = commit_no), type = "line", color= 'yellow', name = "Matthew Johns") %>% 
#   hc_add_series(test5, hcaes(x = commit_date, y = commit_no), type = "line", color= 'yellow', name = "matthewjohns") %>% 
#   hc_add_series(test1, hcaes(x = commit_date, y = commit_no), type= 'line', color = 'red', name = "kristine") %>%
#   hc_add_series(test2, hcaes(x = commit_date, y = commit_no), type = "line", color= 'black', name = "Kristine Dinh") %>% 
#   hc_add_series(test3, hcaes(x = commit_date, y = commit_no), type = "line", color= 'blue', name = "dinhkristine") %>% 
#   hc_add_series(test4, hcaes(x = commit_date, y = commit_no), type = "line", color= 'green', name = "dinhth15") %>% 
#   hc_add_series(test5, hcaes(x = commit_date, y = commit_no), type = "line", color= 'yellow', name = "Â–LauraHuang_AGIA") %>% 
#   hc_add_series(test2, hcaes(x = commit_date, y = commit_no), type = "line", color= 'black', name = "Laura Huang") %>% 
#   hc_add_series(test3, hcaes(x = commit_date, y = commit_no), type = "line", color= 'blue', name = "LauraHuang_AGIA") %>% 
#   hc_add_series(test3, hcaes(x = commit_date, y = commit_no), type = "line", color= 'blue', name = "Nicole Moeller") %>%
#   
#   hc_yAxis(title = list(text = "Count of Commits")) %>%
#   hc_xAxis(type = 'datetime', labels = list(format = '{value:%b %d}'), title = list(text = "Commit Date")) %>%
#   hc_legend(enabled = TRUE) %>% 
#   hc_tooltip(useHTML = TRUE, pointFormat = tltip, split = FALSE) # %>% 
# #hc_add_theme(custom_theme)
# 

