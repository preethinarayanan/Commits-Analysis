library(dplyr)
library(magrittr)

files <-list.files(path="C:/Users/pnarayanan/Documents/projects/bitbucket", full.names=TRUE, recursive=FALSE)

my_repos_list <- files[c(5,6,7,9,16,17, 18,26,29,30,31,33,36)]
#my_repos_list <- files[c(5,6,7)]
my <- files[c(1:40)]



# for (i in my_repos_list){
#   
#   setwd(i)
#   
#   system('git log --pretty=format:"%cn %h on %ci') 
#   
# }



git_log <- function(n){
  #for (i in seq_along(files)) {
  gitlog <- purrr::map(c("%cn", "%h", "%ci"), function(i) paste0('git log --pretty=format:', i)) %>%
    purrr::map(system, intern = T)
  tibble::tibble(commiter = gitlog[[1]],
                 commit_hash = gitlog[[2]],
                 commit_date = gitlog[[3]] %>% stringr::str_replace_all(., pattern = " -0800", ""))  
                 #commit_message = gitlog[[4]]) %>%
  
    #tidyr::separate(., commit_date, sep = "[:space:]+", into = c("weekday", "month", "day_of_month", "time", "year"))
} 

#my <- files[c(1:3)]


#git_log()

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
  
 # saveRDS(git_log(my_repos_list), "C:/Users/pnarayanan/Documents/projects/bitbucket/git.RDS")
  
 }
  
}


print_list3 <- all_repo()

output <- data.table::rbindlist(print_list3)

output2 <- data.table::rbindlist(print_list)

a <- output2

# library(stringi)
# a$repo <- stri_sub(a$repo,4,-1)
# a$repo
# 
# 
# library(stringr)
# word(a$repo,3,sep = "\\/")
# 
# 
# sub(".*\\/.", "", a$repo)
# 
# strsplit(a$repo, "[/]")[[1]]

a$repo <- sub('.*/', '', a$repo)

a$repo



#View(test)

saveRDS(git_log(my_repos_list), "C:/Users/pnarayanan/Documents/projects/bitbucket/git.RDS")


saveRDS(a, "C:/Users/pnarayanan/Documents/projects/bitbucket/gitlog.RDS")


#a <- readRDS("C:/Users/pnarayanan/Documents/projects/bitbucket/gitlog.RDS/gitlog.RDS")

a$commit_date <- lubridate::as_date(a$commit_date)

#a$commit_hash <- as.integer(a$commit_hash)

test <- a %>% select(commiter, commit_hash, commit_date, repo) %>% group_by(commit_date) %>% tally() %>% mutate(commit_no = n) %>% distinct()


ggplot(data=test, aes(x=commit_date, y=commit_no, group=commiter, colour=commiter)) +
  geom_line() +
  geom_point()


test2 <- a %>% select(commiter, commit_hash, commit_date, repo) %>% group_by(commiter, commit_date) %>% mutate(commit_no = length(commit_hash))  %>% distinct()


test3 <- test2 %>% select(commiter, commit_date, commit_no, repo) %>% distinct()


ggplot(data = test3) + 
  geom_point(mapping = aes(x = commit_date, y = commit_no, color = commiter)) 



test4 <- test3 %>% filter(commit_date >= 2020-1-1)


test2$commit_date <- as.Date(test2$commit_date,
                              format = "%m/%d/%y")

class(test3$commit_date)

ggplot(data = test4) + 
  geom_point(mapping = aes(x = commit_date, y = commit_no,  color = commiter )) 

ggplot(data = test3) + 
  geom_point(mapping = aes(x = commit_date, y = commit_no, group = repo, color = commiter )) 


i = plot(test3$commit_date, test3$commit_no, type = "l",)

i = ggplot(test3, aes(commit_date, commit_no))

p = i + geom_line(aes(colour = factor(commiter)))

print(p)



ggplot(data=test3, aes(x=commit_date, y=commit_no, group=commiter, colour=commiter)) +
  geom_line() +
  geom_point()



library(ggplot2)

i = plot(test2$commit_date, test2$commit_no, type = "l",)

i = ggplot(test2, aes(commit_date, commit_no))

p = i + geom_line(aes(colour = factor(commiter)))

print(p)


base_plot <- ggplot(data = test2) +
  geom_line(aes(x = commit_date, y = commit_no), 
            #group = commiter,
            alpha = 0.6,
            size = 0.6) +
  labs(x = "Commit Date", 
       y = "No of commits in a datr",
       title = "Base Plot") +
  theme_minimal()

#ase_plot +
 # scale_x_date(limits = as.Date(c("2020-01-01","2020-12-01"))) +
  #ggtitle("limits = as.Date(c(\"2020-01-01\",\"2020-12-01\"))")

ggplot(data=test2, aes(x=commit_date, y=commit_no, group=commiter, colour=commiter)) +
  geom_line() +
  geom_point()


#daterange=c(as.POSIXlt(min(test2$commit_date)),as.POSIXlt(max(test2$commit_date))) 


ggplot(data = test2) + 
  geom_point(mapping = aes(x = commit_date, y = commit_no, color = commiter )) 
 # geom_smooth(mapping = aes(x = commit_date, y = commit_no, linetype = repo))


ggplot(data = test2) + 
  geom_point(mapping = aes(x = commit_date, y = commit_no, group = commiter, color = repo)) 


meltDF$variable=as.numeric(levels(meltDF$variable))[meltDF$variable]

git_log_plot <- function(git_log_df) {
  #git_log_df$weekday %<>% factor(., levels = c("Fri", "Thu", "Wed", "Tue", "Mon", "Sat", "Sun"))
  #git_log_df$month %<>% factor(., levels = month.abb)
  git_log_df %>%
    #dplyr::mutate(time_24_hrs = substr(time, 1, 2)) %>%
    #dplyr::mutate(weekday = weekday) %>%
    dplyr::group_by(commiter, commit_date) %>%
    dplyr::tally() %>%
    dplyr::rename(commit_tally = n) %>%
    ggplot2::ggplot() +
    #ggplot2::facet_grid(commiter ~ .) +
    ggplot2::geom_tile(ggplot2::aes(commit_date, commit_tally, color = commiter)) +
    #ggplot2::scale_fill_distiller(palette = "Greens", direction = -1) +
    #ggplot2::theme_minimal() +
    #ggplot2::theme(legend.position = "none", legend.title = ggplot2::element_blank()) +
    ggplot2::labs(y = "commiter", x = "commit_date", legend = "")
}
git_log() %>% git_log_plot()


git_log()

git_log_plot(test3)

# git_log_df
# 
# git_log_plot <- function(git_log_df) {
#   #git_log_df$weekday %<>% factor(., levels = c("Fri", "Thu", "Wed", "Tue", "Mon", "Sat", "Sun"))
#   #git_log_df$month %<>% factor(., levels = month.abb)
#   git_log_df %>%
#     
#     group_by_(commiter, date)
#     #dplyr::mutate(time_24_hrs = substr(time, 1, 2)) %>%
#     #dplyr::mutate(weekday = weekday) %>%
#     #dplyr::group_by(commiter,date) %>%
#     dplyr::tally() %>%
#     dplyr::rename(commit_tally = n) %>%
#     ggplot2::ggplot() +
#     ggplot2::facet_grid(commiter ~ .) +
#     #ggplot2::geom_tile(ggplot2::aes(time_24_hrs,  fill = commit_tally)) +
#     #ggplot2::scale_fill_distiller(palette = "Greens", direction = -1) +
#     ggplot2::theme_minimal() +
#     ggplot2::theme(legend.position = "none", legend.title = ggplot2::element_blank()) +
#     ggplot2::labs(y = "n", x = "Commiter", legend = "")
# }
# git_log() %>% git_log_plot()
