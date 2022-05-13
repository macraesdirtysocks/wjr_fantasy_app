schedule <- read_csv("schedule.csv", col_types = cols(time = col_character()))

schedule <- schedule %>% 
  mutate(home_team = str_replace_all(home_team, "USA", "US"),
         away_team = str_replace_all(away_team, "USA", "US")) %>% 
  write_csv("schedule.csv")

playing_today <- inner_join(schedule_long, draft_order[, c(5, 4, 9)], by = c("nation")) %>% 
  write_csv("playing_today.csv")

home <- schedule %>% 
  select(date, "nation" = home_team)

away <- schedule %>% 
  select(date, "nation" = away_team)

schedule_long <- bind_rows(home, away) %>% 
  mutate(
    nation = str_replace_all(nation, "SUI", "switzerland"),
    nation = str_replace_all(nation, "GER", "germany"),
    nation = str_replace_all(nation, "RUS", "russia"),
    nation = str_replace_all(nation, "FIN", "finland"),
    nation = str_replace_all(nation, "AUT", "austria"),
    nation = str_replace_all(nation, "CZE", "czech_republic"),
    nation = str_replace_all(nation, "CAN", "canada"),
    nation = str_replace_all(nation, "SVK", "slovakia"),
    nation = str_replace_all(nation, "SWE", "sweden"),
    nation = str_replace_all(nation, "USA", "usa"),
    date = as.Date(date)
  )
