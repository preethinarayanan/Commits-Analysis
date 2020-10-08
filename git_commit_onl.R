source("libraries.R")

system(glue('git log'))

log_format_options <- c(datetime = "cd", commit = "h", parents = "p", author = "an", subject = "s")
option_delim <- "\t"
log_format   <- glue("%{log_format_options}") %>% collapse(option_delim)
log_options  <- glue('--pretty=format:"{log_format}" --date=format:"%Y-%m-%d %H:%M:%S"')
log_cmd      <- glue('git log {log_options}')
log_cmd
system(glue('{log_cmd}'))

history_logs <- system(log_cmd, intern = TRUE) %>% 
  str_split_fixed(option_delim, length(log_format_options)) %>% 
  as_tibble() %>% 
  setNames(names(log_format_options))

history_logs <- history_logs %>% 
  mutate(parents = str_split(parents, " "))

history_logs <- history_logs %>% mutate(branch = NA_integer_)

# # Create a boolean vector to represent free columns (1000 should be plenty!)
# free_col <- rep(TRUE, 1000)
# 
# for (i in seq_len(nrow(history_logs) - 1)) { # - 1 to ignore root
#   # Check current branch col and assign open col if NA
#   branch <- history_logs$branch[i]
#   
#   if (is.na(branch)) {
#     branch <- which.max(free_col)
#     free_col[branch] <- FALSE
#     history_logs$branch[i] <- branch
#   }
#   
#   # Go through parents
#   parents <- history_logs$parents[[i]]
#   
#   for (p in parents) {
#     parent_col <- history_logs$branch[history_logs$commit == p]
#     
#     # If col is missing, assign it to same branch (if first parent) or new
#     # branch (if other)
#     if (is.na(parent_col)) {
#       parent_col <- if_else(p == parents[1], branch, which.max(free_col))
#       
#       # If NOT missing this means a split has occurred. Assign parent the lowest
#       # and re-open both cols (parent closed at the end)
#     } else {
#       free_col[c(branch, parent_col)] <- TRUE
#       parent_col <- min(branch, parent_col)
#       
#     }
#     
#     # Close parent col and assign
#     free_col[parent_col] <- FALSE
#     history_logs$branch[history_logs$commit == p] <- parent_col
#   }
# }

history_logs %>% 
  group_by(date) %>%
  count(author, sort = TRUE)

history_logs <- history_logs %>% 
  mutate(author = case_when(
    str_detect(tolower(author), "hadley") ~ "Hadley Wickham",
    str_detect(tolower(author), "kohske takahashi") ~ "Kohske Takahashi",
    TRUE ~ str_to_title(author)
  ))
#Now, again, authors by commit frequency:
  history_logs %>% 
  count(author) %>% 
  arrange(desc(n))
  
  
  history_logs %>% 
  count(author) %>% 
  top_n(10, n) %>% 
  mutate(author = fct_reorder(author, n)) %>% 
  ggplot(aes(author, n)) +
  geom_col(aes(fill = n), show.legend = FALSE) +
  coord_flip() +
  theme_minimal() +
  ggtitle("ggplot2 authors with most commits") +
  labs(x = NULL, y = "Number of commits", caption = "Post by @drsimonj")
