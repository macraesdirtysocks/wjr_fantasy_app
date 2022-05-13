library(pacman)
p_load(rvest, ratelimitr)

forward_links <- c("https://www.quanthockey.com/wjc-u20/en/teams/team-austria-forwards-2021-wjc-u20-stats.html",
           "https://www.quanthockey.com/wjc-u20/en/teams/team-canada-forwards-2021-wjc-u20-stats.html",
           "https://www.quanthockey.com/wjc-u20/en/teams/team-czech-republic-forwards-2021-wjc-u20-stats.html",
           "https://www.quanthockey.com/wjc-u20/en/teams/team-finland-forwards-2021-wjc-u20-stats.html",
           "https://www.quanthockey.com/wjc-u20/en/teams/team-germany-forwards-2021-wjc-u20-stats.html",
           "https://www.quanthockey.com/wjc-u20/en/teams/team-russia-forwards-2021-wjc-u20-stats.html",
           "https://www.quanthockey.com/wjc-u20/en/teams/team-slovakia-forwards-2021-wjc-u20-stats.html",
           "https://www.quanthockey.com/wjc-u20/en/teams/team-sweden-forwards-2021-wjc-u20-stats.html",
           "https://www.quanthockey.com/wjc-u20/en/teams/team-switzerland-forwards-2021-wjc-u20-stats.html",
           "https://www.quanthockey.com/wjc-u20/en/teams/team-usa-forwards-2021-wjc-u20-stats.html"
           )

d_links <- str_replace_all(forward_links, "forwards", "defensemen")

goalie_links <- str_replace_all(forward_links, "forwards", "goalies")

countries <- sort(unique(draft_order$nation))

#### scrape function ###
wjr_data <- function(input){
    
  page <- read_html(input)
  
  page %>%  
    html_node("table") %>% 
    html_table()
  
}

#### limit rate of scrape ####

wjr_data_limited <- limit_rate(wjr_data, rate(n = 5, period = 1))

#### MAP GET_PICKS FUNTION OVER LIST LINKS ####

#### F ####

forward_data_raw <- map_df(forward_links, wjr_data_limited, .id = "id")


forward_data <- forward_data_raw %>% 
  select(player = Name, position = Pos, g = G, a = A, , gwg = GWG, ppg = PPG, shg = SHG, nation = id) %>% 
  mutate(
    across(.cols = everything(), ~ replace_na(.x, 0)),
    player = iconv(player, to = "utf8", sub = "xxx", mark = TRUE, toRaw = FALSE),
    position = "F",
    nation = case_when(
      nation == 1 ~ "austria",
      nation == 2 ~ "canada",
      nation == 3 ~ "czech_republic",
      nation == 4 ~ "finland",
      nation == 5 ~ "germany",
      nation == 6 ~ "russia",
      nation == 7 ~ "slovakia",
      nation == 8 ~ "sweden",
      nation == 9 ~ "switzerland",
      nation == 10 ~ "usa",
      TRUE ~ nation
    ), 
    player = case_when(
      player == "Mathias Bxxxhm" ~ "Mathias Bohm",
      player == "Michal Teplxxx" ~ "Michal Teply",
      player == "Aku Rxxxty" ~ "Aku Raty",
      player == "Tim Stxxxtzle" ~ "Tim Stutzle",
      player == "Simon Holmstrxxxm" ~ "Simon Holmstrom ",
      player == "Elvis Schlxxxpfer " ~ "Elvis Schlapfer ",
      player == "Vasili Podkolzin" ~ "Vasily Podkolzin",
      TRUE ~ player
    ),
    player = str_replace_all(player, "ö", "o"),
    pos_fac = "1"
  )

#### D ####

defence_data_raw <- map_df(d_links, wjr_data_limited, .id = "id")


defence_data <- defence_data_raw %>% 
  select(player = Name, position = Pos, g = G, a = A, , gwg = GWG, ppg = PPG, shg = SHG, nation = id) %>% 
  mutate(
    across(.cols = everything(), ~ replace_na(.x, 0)),
    player = iconv(player, to = "utf8", sub = "xxx", mark = TRUE, toRaw = FALSE),
    position = "D",
    nation = case_when(
      nation == 1 ~ "austria",
      nation == 2 ~ "canada",
      nation == 3 ~ "czech_republic",
      nation == 4 ~ "finland",
      nation == 5 ~ "germany",
      nation == 6 ~ "russia",
      nation == 7 ~ "slovakia",
      nation == 8 ~ "sweden",
      nation == 9 ~ "switzerland",
      nation == 10 ~ "usa",
      TRUE ~ nation
    ), 
    player = case_when(
      player == "Jirxxx Suhrada" ~ "Jiri Suhrada",
      player == "Topi Niemelxxx" ~ "Topi Niemela",
      player == "Maximilian Glxxxtzl" ~ "Maximilian Glotzl",
      player == "Victor Sxxxderström" ~ "Victor Soderstrom",
      TRUE ~ player
    ),
    player = str_replace_all(player, "ö", "o"),
    pos_fac = "2"
  )


#### read goalie sheeet ####

googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/1EaxWSYaYHfVV-4IIJtG09XrmFrNx9mSnPS-pVTiFIeE/edit?usp=sharing",
  sheet = "goalie_stats"
) %>%
  write_csv("goalie_stats.csv")

#### bind forward and defence into tibble ####

skater_stats <- bind_rows(forward_data, defence_data) %>% 
  as_tibble() %>%
  write_csv("skater_stats.csv")
  
goalie_stats <- read_csv("goalie_stats.csv") %>% 
  inner_join(national_rosters[,c(1,6)], by = "player")




fantasy_skater_stats <- skater_stats %>% 
  inner_join(draft_order[, c(5,8)], by = c("player" = "selections")) %>%
  group_by(logo) %>% 
  summarise(across(.cols = c(3:7),  ~sum(.x, na.rm = TRUE))) %>% 
  ungroup()

fantasy_goalie_stats <- goalie_stats %>% 
  inner_join(draft_order[, c(8,5)], by = c("player" = "selections")) %>% 
  select("w", "sv", "so", "logo") %>% 
  mutate(sv = 0.1 * sv)


left_join(fantasy_skater_stats, fantasy_goalie_stats, by = "logo") %>%
  mutate(across(.cols = everything(), ~ replace_na(.x, 0))) %>%
  rowwise() %>%
  mutate(f_pts = sum(g, a, gwg, ppg, shg, w, sv, so)) %>%
  ungroup() %>%
  write_csv("fantasy_standings.csv")

top_10_scorers <- bind_rows(
  goalie_stats %>% 
    mutate(f_sv = 0.1 * sv) %>% 
    rowwise() %>% 
    mutate(f_pts = sum(c(w, so, f_sv))) %>% 
    ungroup() %>% 
    select(player, nation, f_pts),
  skater_stats %>%
    rowwise() %>% 
    mutate(f_pts = sum(c_across(3:7))) %>% 
    ungroup() %>% 
    select(player, nation, f_pts)
) %>% 
  arrange(desc(f_pts)) %>% 
  slice_head(n = 10) %>% 
  write_csv("top_10.csv")

rm(fantasy_goalie_stats, fantasy_skater_stats, fantasy_standings, 
   playing_today, goalie_stats, goalie_drop_down, skater_stats,
   national_rosters, draft_order)
