managers <- tibble(
  name = c("Chad", "Jeff", "Tyson", "Oleary", "Clayton", "Darrin", "Wasyl", "Kelsey"),
  logo = c("cozens_eddie", "relitz_relish", "dernic_dynasty", "craigs_button", "mikado_magic", "campbells_soup", "double_dions", "yorkton_g_spots")
)

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
  "Yegor Afanasyev",
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



logo = colDef(
  name = "Logo", 
  cell = function(value) {
    div(
      class = "team",
      img(class = "flag", alt = paste(to_title_case(value), "logo"), src = sprintf("flags/%s.svg", value)),
      div(class = "team-name", to_title_case(value))
    )
  }
)

  
  


rsconnect::setAccountInfo(name=Sys.getenv("rsconnect_name"),
                          token=Sys.getenv("rsconnect_token"),
                          secret=Sys.getenv("rsconnect_secret"
                          )

library(rsconnect)
rsconnect::deployApp('~wjr_app/app.R')




