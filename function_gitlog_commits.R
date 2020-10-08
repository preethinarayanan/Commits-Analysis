git_log <- function(){
  gitlog <- purrr::map(c("%cn", "%h", "%cd", "%s"), function(i) paste0('git log --pretty=format:', i)) %>%
    purrr::map(system, intern = T)
  tibble::tibble(commiter = gitlog[[1]],
                 commit_hash = gitlog[[2]],
                 commit_date = gitlog[[3]] %>% stringr::str_replace_all(., pattern = " -0800", ""),
                 commit_message = gitlog[[4]]) %>%
    tidyr::separate(., commit_date, sep = "[:space:]+", into = c("weekday", "month", "day_of_month", "time", "year"))
}