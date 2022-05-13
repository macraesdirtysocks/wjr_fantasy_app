scoring_ws <- read_html("https://www.iihf.com/en/events/2021/wm20/skaters/scoringleaders")



#### skater stats ####

skater_stats <- scoring_ws %>%
  html_nodes(css = ".s-stats") %>% 
  # html_node("tr") %>%
  html_node("table") %>% 
  html_table() %>% 
  bind_rows() %>% 
  slice(4:n()) %>% 
  bind_cols(player = scoring_ws %>% 
          html_nodes(".s-container") %>% 
          html_nodes(".s-name") %>% 
          html_text(),
        flag = scoring_ws %>% 
          html_nodes(".s-flag") %>% 
          html_attr("style") %>%
          .[4:length(.)] %>% 
          str_extract_between("\\(", "\\)")
          ) %>% 
  as_tibble() %>%
  relocate(player, .before = gp) %>% 
  mutate(across(.cols = c(player, flag), as.character)) %>% 
  rowwise() %>% 
  mutate(stp = sum(ppg,shg)) %>% 
  ungroup() %>% 
  separate(player,sep = " ", into = c("last_name", "first_name")) %>% 
  mutate(last_name = to_title_case(last_name, sep_out = "")) %>% 
  unite(player, first_name, last_name, sep = " ") %>% 
  relocate(stp, .before = "ppg") %>% 
  inner_join(national_rosters[,c(1, 6)], by = "player") %>% 
  write_csv("skater_stats.csv")



fantasy_skater_stats <- skater_stats %>% 
  inner_join(draft_order[, c(5,8)], by = c("player" = "selections")) %>%
  group_by(logo) %>% 
  summarise(across(.cols = c(3:7),  ~sum(.x, na.rm = TRUE))) %>% 
  ungroup()

fantasy_goalie_stats <- goalie_stats %>% 
  inner_join(draft_order[, c(8,5)], by = c("player" = "selections")) %>% 
  select("w", "sv", "so", "logo") %>% 
  mutate(sv = 0.1 * sv)


fantasy_standings <- left_join(fantasy_skater_stats, fantasy_goalie_stats, by = "logo") %>% 
  mutate(across(.cols = everything(), ~ replace_na(.x, 0))) %>% 
  rowwise() %>% 
  mutate(f_pts = sum(g, a, gwg, ppg, shg, w, sv, so)) %>% 
  ungroup() %>% 
  write_csv("fantasy_standings.csv")
