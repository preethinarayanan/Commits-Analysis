#' git_log_plot
#'
#' @param git_log_df output of git_log() function
#'
#' @return ggplot2
#' @export
#'
#' @examples
#' git_log() %>% git_log_plot()
#' 
#' 
#' 


#Eric function

source("libraries.R")
       
git_log <- function(){
  gitlog <- purrr::map(c("%cn", "%h", "%cd", "%s"), function(i) paste0('git log --pretty=format:', i)) %>%
    purrr::map(system, intern = T)
  tibble::tibble(commiter = gitlog[[1]],
                 commit_hash = gitlog[[2]],
                 commit_date = gitlog[[3]] %>% stringr::str_replace_all(., pattern = " -0800", ""),
                 commit_message = gitlog[[4]]) %>%
    tidyr::separate(., commit_date, sep = "[:space:]+", into = c("weekday", "month", "day_of_month", "time", "year"))
}
git_log_plot <- function(git_log_df) {
  git_log_df$weekday %<>% factor(., levels = c("Fri", "Thu", "Wed", "Tue", "Mon", "Sat", "Sun"))
  git_log_df$month %<>% factor(., levels = month.abb)
  git_log_df %>%
    dplyr::mutate(time_24_hrs = substr(time, 1, 2)) %>%
    dplyr::mutate(weekday = weekday) %>%
    dplyr::group_by(commiter, weekday, year, time_24_hrs, year) %>%
    dplyr::tally() %>%
    dplyr::rename(commit_tally = n) %>%
    ggplot2::ggplot() +
    ggplot2::facet_grid(commiter ~ .) +
    ggplot2::geom_tile(ggplot2::aes(time_24_hrs, weekday, fill = commit_tally)) +
    ggplot2::scale_fill_distiller(palette = "Greens", direction = -1) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none", legend.title = ggplot2::element_blank()) +
    ggplot2::labs(y = "Weekday", x = "Hour", legend = "")
}
git_log() %>% git_log_plot()
