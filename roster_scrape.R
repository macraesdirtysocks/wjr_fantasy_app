roster_ws <- read_html("https://www.nhl.com/news/2021-world-junior-championship-rosters/c-319801144")


wjr_rosters <- roster_ws %>% html_node(xpath = '//*[@id="content-wrap"]/div[3]/div[2]/div/article/div[4]/div[2]') %>% 
  html_nodes("p") %>% 
  html_text()

wjr_rosters <- wjr_rosters[10:315] %>% 
  as_tibble()


#### austria ####
roster_aut <- as_tibble(wjr_rosters[c(2:4, 6:14, 16:30), ]) %>% 
  separate(value, sep = ",", into = c("player", "team", "league")) %>% 
  add_column(position = c(rep("G", 3), rep("D", 9),rep("F", 15))) %>% 
  mutate(team = str_trim(team, side = "both"),
         league = str_trim(league, side = "both"),
         draft_eligible = str_extract(league, "2+[0-9]+"),
         draft_eligible = case_when(is.na(draft_eligible) == TRUE ~ "Drafted",
                                    TRUE ~ draft_eligible),
         league = str_remove_all(league, "[:blank:]\\(.*\\)"),
         league = if_else(is.na(league == TRUE), "NHL", league),
         nation = "austria"
         )

#### canada ####
roster_can <- as_tibble(wjr_rosters[c(34:36, 38:45, 47:60), ]) %>% 
  separate(value, sep = ",", into = c("player", "team", "league")) %>% 
  add_column(position = c(rep("G", 3), rep("D", 8),rep("F", 14))) %>% 
  mutate(team = str_trim(team, side = "both"),
         league = str_trim(league, side = "both"),
         draft_eligible = str_extract(league, "2+[0-9]+"),
         draft_eligible = case_when(is.na(draft_eligible) == TRUE ~ "Drafted", 
                                    TRUE ~ draft_eligible),
         league = str_remove_all(league, "[:blank:]\\(.*\\)"),
         league = if_else(is.na(league == TRUE), "NHL", league),
         nation = "canada"
         )

#### czech ####
roster_cze <- as_tibble(wjr_rosters[c(63:65, 67:75, 77:92), ]) %>% 
  separate(value, sep = ",", into = c("player", "team", "league")) %>% 
  add_column(position = c("G", "G", "G", 
                          "D","D","D","D","D","D","D","D","D",
                          "F","F","F","F","F","F","F","F","F","F","F","F","F","F","F","F")
  ) %>% 
  mutate(team = str_trim(team, side = "both"),
         league = str_trim(league, side = "both"),
         draft_eligible = str_extract(league, "2+[0-9]+"),
         draft_eligible = case_when(is.na(draft_eligible) == TRUE ~ "Drafted", 
                                    TRUE ~ draft_eligible),
         league = str_remove_all(league, "[:blank:]\\(.*\\)"),
         league = if_else(is.na(league == TRUE), "NHL", league),
         nation = "czech_republic"
  )

#### finland ####
roster_fin <- as_tibble(wjr_rosters[c(96:98, 100:107, 109:122), ]) %>% 
  separate(value, sep = ",", into = c("player", "team", "league")) %>% 
  add_column(position = c("G", "G", "G", 
                          "D","D","D","D","D","D","D","D",
                          "F","F","F","F","F","F","F","F","F","F","F","F","F","F")
  ) %>% 
  mutate(team = str_trim(team, side = "both"),
         league = str_trim(league, side = "both"),
         draft_eligible = str_extract(league, "2+[0-9]+"),
         draft_eligible = case_when(is.na(draft_eligible) == TRUE ~ "Drafted", 
                                    TRUE ~ draft_eligible),
         league = str_remove_all(league, "[:blank:]\\(.*\\)"),
         league = if_else(is.na(league == TRUE), "NHL", league),
         nation = "finland"
  )

#### germany ####
roster_ger <- as_tibble(wjr_rosters[c(125:127, 129:136, 138:151), ]) %>% 
  separate(value, sep = ",", into = c("player", "team", "league")) %>% 
  add_column(position = c("G", "G", "G", 
                          "D","D","D","D","D","D","D","D",
                          "F","F","F","F","F","F","F","F","F","F","F","F","F","F")
  ) %>% 
  mutate(team = str_trim(team, side = "both"),
         league = str_trim(league, side = "both"),
         draft_eligible = str_extract(league, "2+[0-9]+"),
         draft_eligible = case_when(is.na(draft_eligible) == TRUE ~ "Drafted", 
                                    TRUE ~ draft_eligible),
         league = str_remove_all(league, "[:blank:]\\(.*\\)"),
         league = if_else(is.na(league == TRUE), "NHL", league),
         nation = "germany"
         
  )

#### russia ####
roster_rus <- as_tibble(wjr_rosters[c(154:156, 158:165, 167:180), ]) %>% 
  separate(value, sep = ",", into = c("player", "team", "league")) %>% 
  add_column(position = c(rep("G", 3), rep("D", 8),rep("F", 14))
  ) %>% 
  mutate(team = str_trim(team, side = "both"),
         league = str_trim(league, side = "both"),
         draft_eligible = str_extract(league, "2+[0-9]+"),
         draft_eligible = case_when(is.na(draft_eligible) == TRUE ~ "Drafted", 
                                    TRUE ~ draft_eligible),
         league = str_remove_all(league, "[:blank:]\\(.*\\)"),
         league = if_else(is.na(league == TRUE), "NHL", league),
         nation = "russia"
         
  )

#### slovakia ####
roster_svk <- as_tibble(wjr_rosters[c(183:186, 188:197, 199:214), ]) %>% 
  separate(value, sep = ",", into = c("player", "team", "league")) %>% 
  add_column(position = c(rep("G", 4), rep("D", 10),rep("F", 16))
  ) %>% 
  mutate(team = str_trim(team, side = "both"),
         league = str_trim(league, side = "both"),
         draft_eligible = str_extract(league, "2+[0-9]+"),
         draft_eligible = case_when(is.na(draft_eligible) == TRUE ~ "Drafted", 
                                    TRUE ~ draft_eligible),
         league = str_remove_all(league, "[:blank:]\\(.*\\)"),
         league = if_else(is.na(league == TRUE), "NHL", league),
         nation = "slovakia"
  )

#### sweden ####
roster_swe <- as_tibble(wjr_rosters[c(218:220, 222:229, 231:244), ]) %>% 
  separate(value, sep = ",", into = c("player", "team", "league")) %>% 
  add_column(position = c(rep("G", 3), rep("D", 8),rep("F", 14))
  ) %>% 
  mutate(team = str_trim(team, side = "both"),
         league = str_trim(league, side = "both"),
         draft_eligible = str_extract(league, "2+[0-9]+"),
         draft_eligible = case_when(is.na(draft_eligible) == TRUE ~ "Drafted", 
                                    TRUE ~ draft_eligible),
         league = str_remove_all(league, "[:blank:]\\(.*\\)"),
         league = if_else(is.na(league == TRUE), "NHL", league),
         nation = "sweden"
         
  )

#### switzerland ####
roster_sui <- as_tibble(wjr_rosters[c(247:249, 251:260, 262:276), ]) %>% 
  separate(value, sep = ",", into = c("player", "team", "league")) %>% 
  add_column(position = c(rep("G", 3), rep("D", 10),rep("F", 15))
  ) %>% 
  mutate(team = str_trim(team, side = "both"),
         league = str_trim(league, side = "both"),
         draft_eligible = str_extract(league, "2+[0-9]+"),
         draft_eligible = case_when(is.na(draft_eligible) == TRUE ~ "Drafted", 
                                    TRUE ~ draft_eligible),
         league = str_remove_all(league, "[:blank:]\\(.*\\)"),
         league = if_else(is.na(league == TRUE), "NHL", league),
         nation = "switzerland"
  )

#### united_states ####
roster_usa <- as_tibble(wjr_rosters[c(280:282, 284:292, 294:306), ]) %>% 
  separate(value, sep = ",", into = c("player", "team", "league")) %>% 
  add_column(position = c(rep("G", 3), rep("D", 9),rep("F", 13))
  ) %>% 
  mutate(team = str_trim(team, side = "both"),
         league = str_trim(league, side = "both"),
         draft_eligible = str_extract(league, "2+[0-9]+"),
         draft_eligible = case_when(is.na(draft_eligible) == TRUE ~ "Drafted", 
                                    TRUE ~ draft_eligible),
         league = str_remove_all(league, "[:blank:]\\(.*\\)"),
         league = if_else(is.na(league == TRUE), "NHL", league),
         nation = "usa"
  )


national_rosters <- bind_rows(roster_aut, 
                              roster_can,
                              roster_cze, 
                              roster_fin,
                              roster_ger,
                              roster_rus,
                              roster_sui,
                              roster_svk,
                              roster_swe,
                              roster_usa) %>%
  mutate(pos_fac = case_when(position == "F" ~ "1",
                             position == "D" ~ "2",
                             position == "G" ~ "3",
                             TRUE ~ position)) %>% 
  write_csv("national_rosters.csv")
