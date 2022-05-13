managers <- tibble(
  name = c("Chad", "Jeff", "Tyson", "Oleary", "Clayton", "Darrin", "Wasyl", "Kelsey"),
  logo = c("cozens_eddie", "relitz_relish", "dernic_dynasty", "craigs_button", "mikado_magic", "campbells_soup", "double_dions", "yorkton_g_spots")
)


draft_order <- tibble(
  round = rep(1:6, each = 8), 
  pick = rep(seq(1:8), times = 6),
  selections = drafted_players,
  owner = c(managers$name, rev(managers$name), managers$name, rev(managers$name), managers$name, rev(managers$name))
  ) %>% 
  inner_join(managers, by = c("owner" = "name")) %>% 
  mutate(team = to_upper_camel_case(logo, sep_in = "_", sep_out = " "),
         team = str_replace_all(team, "Yorkton G Spots", "Yorkton G-Spots")) %>% 
  relocate(team, .before = selections) %>% 
  inner_join(national_rosters[,c(1,4,6)], by = c("selections" = "player")) %>% 
  relocate(position, .after = selections) %>% 
  mutate(pos_fac = case_when(position == "F" ~ "1", 
                             position == "D" ~ "2",
                             position == "G" ~ "3",
                             TRUE ~ position),
         pos_fac = as.numeric(pos_fac)) %>% 
  rownames_to_column(var = "overall") %>% 
  write_csv("draft.csv")
                             





drafted_players <- c(
  "Dawson Mercer",
  "Quinton Byfield",
  "Dylan Cozens",
  "Trevor Zegras",
  "Cole Caufield",
  "Tim Stutzle",
  "Yaroslav Askarov",
  "Dylan Holloway",
  "Spencer Knight",
  "Connor McMichael",
  "Lucas Raymond",
  "Hugo Alnefelt",
  "Marco Rossi",
  "Matthew Beniers",
  "Philip Broberg",
  "Cam York",
  "Alexander Holtz",
  "Aku Raty",
  "Anton Lundell",
  "Jamie Drysdale",
  "Alex Newhook",
  "Vasily Podkolzin",
  "Alex Turcotte",
  "Shakir Mukhamadullin",
  "Arvid Costmar",
  "Tobias Bjornfot",
  "Taylor Gauthier",
  "Rodion Amirov",
  "Lukas Parik",
  "Victor Soderstrom",
  "Samuel Hlavaj",
  "Mikhail Abramov",
  "Joel Blomqvist",
  "Simon Knak",
  "Arno Tiefensee",
  "Egor Afanasyev",
  "Ville Heinola",
  "Jake Sanderson",
  "Roni Hirvonen",
  "Henri Nikkanen",
  "Jack Quinn",
  "Peyton Krebs",
  "Bobby Brink",
  "Arthur Kaliyev",
  "Cole Perfetti",
  "Bowen Byram",
  "Matthew Boldy",
  "Connor Zary"
)
  
  
home <- schedule %>% 
  filter(phase == "Round Robin") %>% 
  select(date, "nation" = home_team)
  
away <- schedule %>% 
  filter(phase == "Round Robin") %>% 
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

  

playing_today <- inner_join(schedule_long, draft_order[, c(5, 4, 9)], by = c("nation")) %>% 
  write_csv("playing_today.csv")


rsconnect::setAccountInfo(name=Sys.getenv("rsconnect_name"),
                          token=Sys.getenv("rsconnect_token"),
                          secret=Sys.getenv("rsconnect_secret"
                          )

library(rsconnect)
rsconnect::deployApp('~wjr_app/app.R')


schedule <- schedule %>% 
  mutate(home_team = str_replace_all(home_team, "USA", "US"),
         away_team = str_replace_all(away_team, "USA", "US")) %>% 
  write_csv("schedule.csv")

