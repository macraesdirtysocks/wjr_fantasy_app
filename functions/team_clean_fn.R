team_clean_fn <- function(x){
  
  df_names <- c("jersey_num", "player", "player_info", "gp", "g", "a", "pts", "pim", "sog", "x", "gwg", "ppg", "shg")
  
  x_clean <- 
    x[seq(from = 1, to = 67, by = 3),] %>% 
    janitor::clean_names() %>% 
    select(-c(x, x_3:x_12)) %>% 
    set_names(df_names)
  
  
  info <- 
    str_split(can_clean[1, 3], "\n\n\n")[[1]][2:9] %>% 
    str_replace_all("\n", " ") %>% 
    str_trim(side = "both") %>% 
    tibble(info = .) %>% 
    separate(col = info, into = c("info", "value"), sep = ": ") %>% 
    pivot_wider(id_cols = info, names_from = info, values_from = value)
  
  
}