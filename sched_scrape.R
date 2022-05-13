pacman::p_load(rvest, magrittr, snakecase, janitor, polite)


#### SCAPRE WJR SCHEDULE ###

sched_ws <- read_html("https://www.iihf.com/en/events/2021/wm20/schedule") ####URL OF WEBSITE ####

sched <- sched_ws %>% 
  html_nodes(".b-card-schedule.s-card-container.visible")


schedule <- bind_cols(
  phase = sched %>% html_attr("data-phase"),
  date = sched %>% html_attr("data-date"),
  time = sched %>% html_node(".s-time") %>% html_text(),
  home_team = sched %>% html_attr("data-hometeam"),
  away_team = sched %>% html_attr("data-guestteam")
) %>%
  mutate(
    phase = case_when(
      phase == "PreliminaryRound" ~ "Round Robin",
      phase == "BronzeMedalGame" ~ "Bronze",
      phase == "GoldMedalGame" ~ "Championship",
      TRUE ~ phase
    ),
    home_team = str_replace_all(home_team, "USA", "US"),
    away_team = str_replace_all(away_team, "USA", "US"),
    date = ymd(date),
    time = as.character(time)
  ) %>%
  write_csv("schedule.csv")



flag <- sched_ws %>% 
  html_nodes(".s-flag") %>% 
  html_attr("style") %>% 
  unique() %>% 
  na.omit() %>% 
  str_extract_between(pattern1 = "\\(", pattern2 = "\\)")

ls("package:dplyr")

