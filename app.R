library(shiny)
library(shinydashboard)
library(tidyverse)
library(lubridate)
library(reactable)
library(stringr.plus)
library(janitor)
library(snakecase)
library(googlesheets4)
library(formattable)


national_rosters <- read_csv("national_rosters.csv")
draft_order <- read_csv("draft.csv")
schedule <- read_csv("schedule.csv", col_types = cols(time = col_character()))
skater_stats <- read_csv("skater_stats.csv")
schedule_long <- read_csv("schedule_long.csv")
playing_today <- read_csv("playing_today.csv")
fantasy_standings <- read_csv("fantasy_standings.csv")
top_10_scorers <- read_csv("top_10.csv")

goalie_stats <- read_csv("goalie_stats.csv") %>% 
  inner_join(national_rosters[,c(1,6)], by = "player")

skater_drop_down <- right_join(skater_stats,
                               draft_order[, c(5, 8)], by = c("player" = "selections")) %>%
  filter(pos_fac != 3) %>%
  mutate(across(.cols = everything(), ~ replace_na(.x, 0)))


goalie_drop_down <- right_join(goalie_stats[, c(1:5)],
                               draft_order[, c(5, 8, 9, 10)], by = c("player" = "selections")) %>%
  filter(pos_fac == 3) %>%
  mutate(across(.cols = everything(), ~ replace_na(.x, 0)),
         sv = 0.1 * sv)

ui <- dashboardPage(
  
  dashboardHeader(title = "Hasbeen WJR Pool", titleWidth = 200),
  
  ### SIDEBAR ####
  dashboardSidebar(width = 200,
    sidebarMenu(
      menuItem("Main", tabName = "main_sched", icon = icon("home")),
      
      # menuItem("Fantasy", tabName = "fantasy", icon = icon("home")),
      
      menuItem("Teams", tabName = "teams_tab", icon = icon("user-circle")),
      
      menuItem("Stats", tabName = "stats_tab", icon = icon("file-contract"),
               menuSubItem(text = "Forwards", tabName = "forward_stats"),
               menuSubItem(text = "Goalies", tabName = "goalie_stats")
               ),
      
      menuItem("Draft", tabName = "draft_tab", icon = icon("list-ol")),

      menuItem("Schedule", tabName = "full_schedule", icon = icon("calendar-alt")),
      
      menuItem("Rosters", tabName = "rosters", icon = icon("address-book"),
               menuSubItem("Austria", tabName = "roster_aut"),
               menuSubItem("Canada", tabName = "roster_can"),
               menuSubItem("Czech Republic", tabName = "roster_cze"),
               menuSubItem("Finland", tabName = "roster_fin"),
               menuSubItem("Germany", tabName = "roster_ger"),
               menuSubItem("Russia", tabName = "roster_rus"),
               menuSubItem("Slovakia", tabName = "roster_svk"),
               menuSubItem("Sweden", tabName = "roster_swe"),
               menuSubItem("Switzerland", tabName = "roster_sui"),
               menuSubItem("USA", tabName = "roster_usa")
               )
      )
  ),
  
  #### BODY ####
  dashboardBody(
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    tabItems(
      
      #### main_body ###
      tabItem(tabName = "main_sched",
              fluidRow(
                box(width = 6,
                  h3("Fantasy Standings", align = "center", style = "font-weight:700"),
                  tabName = "fantasy", reactableOutput("main_panel_fantasy_stats")),
                box(
                  width = 6,
                  footer = "***All Game Times MST***",
                  h3("Todays Games", align = "center", style = "font-weight:700"),
                  reactableOutput(outputId = "main_panel_sched"))
                ),
              fluidRow(
                box(width = 6, 
                    h3("Top 10 fantasy Scorers", align = "center", style = "font-weight:700"),
                    reactableOutput("main_panel_top_10")),
                box(
                  width = 6,
                  h3("Todays Players", align = "center", style = "font-weight:700"),
                  reactableOutput("main_panel_players")
                )
              )),  
     
      #### stats_body ####
      
      tabItem(tabName = "forward_stats", 
              fluidRow(
                box(width = 6, 
                    reactableOutput("skater_stats")))),
      tabItem(tabName = "goalie_stats",
              fluidRow(
                box(width = 6,
                    reactableOutput("goalie_stats")))),
      
      
      
      
      #### draft_body ####
      tabItem(tabName = "draft_tab",
              fluidRow(
                box(title = "Round 1", reactableOutput(outputId = "draft_1"), width = 12)),
              fluidRow(
                box(title = "Round 2", reactableOutput(outputId = "draft_2"), width = 6),
                box(title = "Round 3", reactableOutput(outputId = "draft_3"), width = 6)),
              fluidRow(
                box(title = "Round 4", reactableOutput(outputId = "draft_4"), width = 6),
                box(title = "Round 5", reactableOutput(outputId = "draft_5"), width = 6)),
              fluidRow(
                box(title = "Flex", reactableOutput(outputId = "draft_6"), width = 6),
                box(title = "Draft Stats", formattableOutput(outputId = "draft_stats"), width = 6))
              ),
              
      
      
      #### teams body ####
      tabItem(tabName = "teams_tab",
              fluidRow(
                box(
                  h4(
                    tags$div("Double Dions",
                             style = "font-size:24px;font-weight:700;padding-bottom:15px;"),
                    tags$img(src = "manager_logo/double_dions.jpg",
                             style = "border-radius:70%;max-width:100%;height:100px;border:4pxsolid#ddd;",
                             class = "center")
                  ),
                  align = "center",
                  width = 6,
                  reactableOutput("double_dions")
                ),
                box(
                  h4(
                    tags$div("Dernic Dynasty",
                             style = "font-size:24px;font-weight:700;padding-bottom:15px;"),
                    tags$img(src = "manager_logo/dernic_dynasty.jpg",
                             style = "border-radius:70%;max-width:100%;height:100px;border:4pxsolid#ddd;")
                  ),
                  align = "center",
                  width = 6,
                  reactableOutput("dernic_dynasty")
                )
              ),
              
              fluidRow(
                box(
                  h4(
                    tags$div("Campbells Soup",
                             style = "font-size:24px;font-weight:700;padding-bottom:15px;"),
                    tags$img(src = "manager_logo/campbells_soup.jpg",
                             style = "border-radius:70%;max-width:100%;height:100px;border:4pxsolid#ddd;")
                  ),
                  align = "center",
                  width = 6,
                  reactableOutput("campbells_soup")
                ),
                box(
                  h4(
                    tags$div("Cozens Eddie",
                             style = "font-size:24px;font-weight:700;padding-bottom:15px;"),
                    tags$img(src = "manager_logo/cozens_eddie.jpg",
                             style = "border-radius:70%;max-width:100%;height:100px;border:4pxsolid#ddd;")
                  ),
                  align = "center",
                  width = 6,
                  reactableOutput("cozens_eddie")
                )
              ),
              
              fluidRow(
                box(
                  h4(
                    tags$div("Craigs Button",
                             style = "font-size:24px;font-weight:700;padding-bottom:15px;"),
                    tags$img(src = "manager_logo/craigs_button.jpg",
                             style = "border-radius:70%;max-width:100%;height:100px;border:4pxsolid#ddd;")
                  ),
                  align = "center",
                  width = 6,
                  reactableOutput("craigs_button")
                ),
                
                box(
                  h4(
                    tags$div("Mikado Magic",
                             style = "font-size:24px;font-weight:700;padding-bottom:15px;"),
                    tags$img(src = "manager_logo/mikado_magic.jpg",
                             style = "border-radius:70%;max-width:100%;height:100px;border:4pxsolid#ddd;")
                    ),
                    align = "center",
                    width = 6,
                    reactableOutput("mikado_magic")
                  )
                ),
                
                fluidRow(
                  box(
                    h4(
                      tags$div("Relitz Relish",
                               style = "font-size:24px;font-weight:700;padding-bottom:15px;"),
                      tags$img(src = "manager_logo/relitz_relish.jpg",
                               style = "border-radius:70%;max-width:100%;height:100px;border:4pxsolid#ddd;")
                    ),
                    align = "center",
                    width = 6,
                    reactableOutput("relitz_relish")
                  ),
                  
                  box(
                    h4(
                      tags$div("Yorkton G-Spots",
                               style = "font-size:24px;font-weight:700;padding-bottom:15px;"),
                      tags$img(src = "manager_logo/yorkton_g_spots.jpg",
                               style = "border-radius:70%;max-width:100%;height:100px;border:4pxsolid#ddd;")
                    ),
                    align = "center",
                    width = 6,
                    reactableOutput("yorkton_g_spots")
                  )
                )
              ),
              
    
    
      #### schedule_body ####
      tabItem(tabName = "full_schedule",
              reactableOutput(outputId = "full_schedule")),
      
    
      #### national_rosters_body ####
      tabItem(tabName = "roster_can",
             tags$h2(
               tags$img(src = "team_jerseys/canada.png", alt = "Canada", height = 100),
               style = "text-align:center"),
             reactableOutput(outputId = "canada")),
      tabItem(tabName = "roster_aut",
              tags$h2(
                tags$img(src = "team_jerseys/austria.png", alt = "Austria", height = 100),
                style = "text-align:center"),
              reactableOutput(outputId = "austria")),
      tabItem(tabName = "roster_cze",
              tags$h2(
                tags$img(src = "team_jerseys/czech.png", alt = "Czech", height = 100),
                style = "text-align:center"),
              reactableOutput(outputId = "czech")),
      tabItem(tabName = "roster_fin",
              tags$h2(
                tags$img(src = "team_jerseys/finland.png", alt = "Finland", height = 100),
                style = "text-align:center"),
              reactableOutput(outputId = "finland")),
      tabItem(tabName = "roster_ger",
              tags$h2(
                tags$img(src = "team_jerseys/germany.png", alt = "Germany", height = 100),
                style = "text-align:center"),
              reactableOutput(outputId = "germany")),
      tabItem(tabName = "roster_rus",
              tags$h2(
                tags$img(src = "team_jerseys/russia.png", alt = "Russia", height = 100),
                style = "text-align:center"),
              reactableOutput(outputId = "russia")),
      tabItem(tabName = "roster_svk",
              tags$h2(
                tags$img(src = "team_jerseys/slovakia.png", alt = "Slovakia", height = 100),
                style = "text-align:center"),
              reactableOutput(outputId = "slovakia")),
      tabItem(tabName = "roster_swe",
              tags$h2(
                tags$img(src = "team_jerseys/sweden.png", alt = "Sweden", height = 100),
                style = "text-align:center"),
              reactableOutput(outputId = "sweden")),
      tabItem(tabName = "roster_sui",
              tags$h2(
                tags$img(src = "team_jerseys/swiss.png", alt = "Swiss", height = 100),
                style = "text-align:center"),
              reactableOutput(outputId = "switzerland")),
      tabItem(tabName = "roster_usa",
              tags$h2(
                tags$img(src = "team_jerseys/usa.png", alt = "USA", height = 100),
                style = "text-align:center"),
              reactableOutput(outputId = "usa"))
    )
  ))





### SERVER ####
server <- function(input, output) { 
  
  
  #### main panel ####
  
  
  #### schedule ####
  output$main_panel_sched <- renderReactable({
    reactable(schedule[schedule$date == Sys.Date(), c(1, 3:5)],
              columns = list(
                phase = colDef(
                  name = "Phase",
                  minWidth = 75,
                  align = "left",
                  style = "font-weight:700"
                  ),
                
                time = colDef(
                  name = "Time",
                  minWidth = 75, 
                  align = "left",
                  class = "team-name"
                  ),
                
                home_team = colDef(
                  name = "Home Team",
                  minWidth = 100, 
                  align = "left",
                  cell = function(value, index) {
                    div(
                      class = "team",
                      img(class = "flag", 
                          alt = paste(value, "flag"), 
                          src = sprintf("flags/%s.svg", value)),
                      div(class = "team-name", value)
                    )
                  }
                ),
                
                away_team = colDef(
                  name = "Away Team",
                  minWidth = 100, 
                  align = "left", 
                  cell = function(value) {
                    div(
                      class = "team",
                      img(class = "flag", alt = paste(value, "flag"), src = sprintf("flags/%s.svg", value)),
                      div(class = "team-name", value)
                    )
                  }
                )
              ))
  })
  
  output$main_panel_fantasy_stats <- renderReactable({
    reactable(
      fantasy_standings[, c(1, 10)],
      rownames = FALSE,
      defaultSorted = list("f_pts" = "desc"),
      showSortIcon = FALSE,
      striped = TRUE,
      defaultColDef = colDef(headerStyle = "font-size:20px;font-weight:650"),
      columns = list(
        logo = colDef(
          name = "Team",
          minWidth = 175,
          cell = function(value) {
            div(
              class = "team",
              img(
                style = "border-radius:50%;max-width:100%;height:40px;border:1pxsolid#ddd;padding-right:5px",
                alt = paste(to_title_case(value), "team"),
                src = sprintf("manager_logo/%s.jpg", value)
              ),
              div(
                class = "team-name",
                if_else(
                  value == "yorkton_g_spots",
                  "Yorkton G-Spots",
                  to_title_case(value)
                )
              )
            )
          },
          details = function(index){
            goalies <-
              filter(goalie_drop_down, logo == fantasy_standings$logo[index]) %>%
              mutate(F_Pts = rowSums(.[3:5]))
            players <-
              filter(skater_drop_down, logo == fantasy_standings$logo[index]) %>%
              mutate(F_Pts = rowSums(.[3:7]))
            htmltools::div(
              style = "padding: 16px",
              
              reactable(
                players[,c(1:7,11)],
                rownames = FALSE,
                defaultExpanded = FALSE,
                fullWidth = FALSE,
                bordered = TRUE,
                compact = TRUE,
                striped = TRUE,
                columns = list(
                  player = colDef(name = "Player", minWidth = 150),
                  
                  position = colDef(name = "Pos", minWidth = 40),
                  
                  g = colDef(name = "G", minWidth = 40),
                  
                  a = colDef(name = "A", minWidth = 40),
                  
                  gwg = colDef(name = "GWG", minWidth = 50),
                  
                  ppg = colDef(name = "PPG", minWidth = 50),
                  
                  shg = colDef(name = "SHG", minWidth = 50),
                  
                  F_Pts = colDef(
                    name = "F_Pts",
                    minWidth = 50,
                    footer = function(values) {
                      sprintf("%.f", sum(values))
                    }
                  )
                )
              ),
              reactable(
                goalies[,c(1:5,9)],
                rownames = FALSE,
                defaultExpanded = FALSE,
                fullWidth = FALSE,
                bordered = TRUE,
                compact = TRUE,
                striped = TRUE,
                columns = list(
                  player = colDef(name = "Player", minWidth = 150),
                  gp = colDef(name = "GP", minWidth = 50),
                  w = colDef(name = "W", minWidth = 50),
                  sv = colDef(name = "SV", minWidth = 50),
                  so = colDef(name = "SO", minWidth = 50),
                  F_Pts = colDef(name = "F_Pts", minWidth = 50)
                  )
                ),
                outlined = TRUE
              )
              
          }
          
            ),
        f_pts = colDef(name = "F_Pts", class = "team-name")
        )
      )
  })
  
  output$fantasy_stats <- renderReactable({
    reactable(
      fantasy_standings,
      rownames = FALSE,
      striped = TRUE,
      defaultSorted = list("f_pts" = "desc", "g" = "desc"),
      showSortIcon = FALSE,
      defaultColDef = colDef(headerStyle = "font-size:18px;font-weight:650"),
      columns = list(
        
        logo = colDef(
          name = "Team",
          width = 225, 
          align = "left",
          cell = function(value) {
            div(
              class = "team",
              img(
                style = "border-radius:50%;max-width:100%;height:30px;border:1pxsolid#ddd;padding-right:5px",
                alt = paste(to_title_case(value), "team"),
                src = sprintf("manager_logo/%s.jpg", value)
              ),
              div(
                class = "team-name",
                if_else(
                  value == "yorkton_g_spots",
                  "Yorkton G-Spots",
                  to_title_case(value)
                )
              )
            )
          },
          details = function(index){
           players <- filter(test, logo == fantasy_standings$logo[index])
           reactable(
             players[, c(1:6)],
             rownames = FALSE,
             defaultExpanded = FALSE,
             fullWidth = FALSE,
             bordered = TRUE,
             compact = TRUE,
             striped = TRUE,
             columns = list(
               player = colDef(name = "Player", minWidth = 150),
               
               g = colDef(name = "G", minWidth = 50),
               
               a = colDef(name = "A", minWidth = 50),
               
               gwg = colDef(name = "GWG", minWidth = 50),
               
               ppg = colDef(name = "PPG", minWidth = 50),
               
               shg = colDef(name = "SHG", minWidth = 50)
             )
           )
           
          }
        ), 
        
        g = colDef(
          name = "G",
          minWidth = 65,
          align = "left"
        ),
        
        a = colDef(
          name = "A",
          minWidth = 65,
          align = "left"
        ),
         
        gwg = colDef(
          name = "GWG",
          minWidth = 65,
          align = "left"
        ),
        
        ppg = colDef(
          name = "PPG",
          minWidth = 65,
          align = "left"
        ),
        
        shg = colDef(
          name = "SHG",
          minWidth = 65,
          align = "left"
        ),
        
        w = colDef(
          name = "W",
          minWidth = 65,
          align = "left"
        ),
        
        sv = colDef(
          name = "SVS",
          minWidth = 65,
          align = "left"
        ),
        
        so = colDef(
          name = "SO",
          minWidth = 65,
          align = "left"
        ),
        
        f_pts = colDef(
          name = "F_Pts",
          minWidth = 65,
          align = "center"
        )
      )
    )})
  
  output$main_panel_top_10 <- renderReactable({
    reactable(
      top_10_scorers,
        columns = list(
          player = colDef(
            name = "Player",
            minWidth = 250,
            align = "left",
            cell = function(value, index) {
              flag <- top_10_scorers[index, 2]
              div(
                class = "team",
                img(class = "flag", 
                    alt = paste(to_screaming_snake_case(value), "flag"), 
                    src = sprintf("flags/%s.svg", flag)),
                div(class = "team-name",
                      to_title_case(value)
                    ))}),
          
          nation = colDef(show = FALSE),
          
          f_pts = colDef(
            name = "F_Pts",
            minWidth = 75,
            align = "center", 
            class = "team-name"
          ))
        )})
  
  output$skater_stats <- renderReactable({
    reactable(
      skater_stats %>% 
        rowwise() %>% 
        mutate(f_pts = sum(c_across(3:7))) %>% 
        ungroup(),
      filterable = TRUE,
      defaultSorted = list("g" = "desc"),
      showSortIcon = FALSE,
      rownames = FALSE,
      sortable = TRUE,
      defaultPageSize = 25,
      striped = TRUE,
      columns = list(
        
        nation = colDef(show = FALSE),
        
        player = colDef(
          name = "Player",
          minWidth = 250,
          align = "left",
          cell = function(value, index) {
            flag <- skater_stats[index, 8]
            div(
              class = "team",
              img(class = "flag", 
                  alt = paste(to_screaming_snake_case(value), "flag"), 
                  src = sprintf("flags/%s.svg", flag)),
              div(class = "team-name",
                  if_else(
                    value == "yorkton_g_spots",
                    "Yorkton G-Spots",
                    to_title_case(value)
                  )))}), 
          
        
        g = colDef(
          name = "G",
          minWidth = 65,
          align = "left"
        ),
        
        a = colDef(
          name = "A",
          minWidth = 65,
          align = "left"
        ),
        
        gwg = colDef(
          name = "GWG",
          minWidth = 65,
          align = "left"
        ),
        
        ppg = colDef(
          name = "PPG",
          minWidth = 65,
          align = "left"
        ),
        
        shg = colDef(
          name = "SHG",
          minWidth = 65,
          align = "left"
        ),
        
        f_pts = colDef(
          name = "F_Pts",
          minWidth = 65,
          align = "left"
        ),
        
        position = colDef(show = FALSE),
        
        pos_fac = colDef(show = FALSE),
        
        nation = colDef(show = FALSE)
      )
      )})
  
  output$goalie_stats <- renderReactable({
    reactable(
      goalie_stats %>% 
        mutate(f_sv = 0.1 * sv) %>% 
        rowwise() %>% 
        mutate(f_pts = sum(c(w, so, f_sv))) %>% 
        ungroup(),
      filterable = TRUE,
      defaultSorted = list(sv = "desc", w = "desc"),
      rownames = FALSE,
      sortable = TRUE,
      defaultPageSize = 30,
      striped = TRUE,
      columns = list(
        
        nation = colDef(show = FALSE),
        
        player = colDef(
          name = "Player",
          minWidth = 270,
          align = "left",
          cell = function(value, index) {
            flag <- goalie_stats[index, 6]
            div(
              class = "team",
              img(class = "flag", 
                  alt = paste(to_screaming_snake_case(value), "flag"), 
                  src = sprintf("flags/%s.svg", flag)),
              div(class = "team-name",
                  if_else(
                    value == "yorkton_g_spots",
                    "Yorkton G-Spots",
                    to_title_case(value)
                  )))}), 
        
        gp = colDef(
          name = "GP",
          minWidth = 45,
          align = "left"
        ),
        
        w = colDef(
          name = "W",
          minWidth = 45,
          align = "left"
        ),
        
        sv = colDef(
          name = "SV",
          minWidth = 45,
          align = "left"
          ),
        
        so = colDef(
          name = "SHO",
          minWidth = 50,
          align = "left"
        ),
        
        f_sv = colDef(
          show = FALSE
        ),
        
        f_pts = colDef(
          name = "F_Pts",
          minWidth = 65,
          align = "left"
        ),
        
        nation = colDef(show = FALSE)
      )
      
    )})
  
  output$main_panel_players <- renderReactable({
    reactable(playing_today[playing_today$date == Sys.Date(), c(2:4)],
              rownames = FALSE,
              striped = TRUE,
              groupBy = "team",
              defaultSorted = list("team" = "asc"),
              defaultExpanded = TRUE,
              compact = TRUE,
              columns = list(
                
                nation = colDef(
                  name = "",
                  minWidth = 50,
                  align = "right",
                  cell = function(value) {
                    div(
                      class = "team",
                      img(class = "flag", 
                          alt = paste(to_screaming_snake_case(value), "flag"), 
                          src = sprintf("flags/%s.svg", value)))}),
                
                team = colDef(
                  name = "Team",
                  minWidth = 100
                  ),
                
                selections = colDef(
                  name = "Player",
                  minWidth = 80
                )
              
              )
              
              )})
  
  #### draft ###
  output$draft_1 <- renderReactable({
    reactable(draft_order[draft_order$round == 1, c(1,9, 5,6,8)],
              rownames = FALSE,
              compact = TRUE,
              minRows = 8,
              striped = TRUE,
              columns = list(
                
                overall = colDef(name = "Pick",
                                 minWidth = 50,
                                 align = "left"),
                
                selections = colDef(name = "Player",
                                    minWidth = 110,
                                    align = "left"
                ),
                
                position = colDef(name = "Position",
                                  minWidth = 75,
                                  align = "center"),
                logo = colDef(
                  name = "Team",
                  minWidth = 70,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "logo"),
                        src = sprintf("manager_logo/%s.jpg", value),
                        style = "border-radius:50%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                ),
                
                nation = colDef(
                  name = "",
                  minWidth = 50,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "flag"),
                        src = sprintf("flags/%s.svg", value),
                        # style = "border-radius:60%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                )
                
              )
    )})
  
  output$draft_2 <- renderReactable({
    reactable(draft_order[draft_order$round == 2, c(1,9, 5,6,8)],
              rownames = FALSE,
              compact = TRUE,
              minRows = 8,
              striped = TRUE,
              columns = list(
                
                overall = colDef(name = "Pick",
                                 minWidth = 50,
                                 align = "left"),
                
                selections = colDef(name = "Player",
                                    minWidth = 110,
                                    align = "left"
                ),
                
                position = colDef(name = "Position",
                                  minWidth = 75,
                                  align = "center"),
                logo = colDef(
                  name = "Team",
                  minWidth = 70,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "logo"),
                        src = sprintf("manager_logo/%s.jpg", value),
                        style = "border-radius:50%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                ),
                
                nation = colDef(
                  name = "",
                  minWidth = 50,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "flag"),
                        src = sprintf("flags/%s.svg", value),
                        # style = "border-radius:60%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                )
                
              )
    )})
  
  output$draft_3 <- renderReactable({
    reactable(draft_order[draft_order$round == 3, c(1,9, 5,6,8)],
              rownames = FALSE,
              compact = TRUE,
              minRows = 8,
              striped = TRUE,
              columns = list(
                
                overall = colDef(name = "Pick",
                                 minWidth = 50,
                                 align = "left"),
                
                selections = colDef(name = "Player",
                                    minWidth = 110,
                                    align = "left"
                ),
                
                position = colDef(name = "Position",
                                  minWidth = 75,
                                  align = "center"),
                logo = colDef(
                  name = "Team",
                  minWidth = 70,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "logo"),
                        src = sprintf("manager_logo/%s.jpg", value),
                        style = "border-radius:50%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                ),
                
                nation = colDef(
                  name = "",
                  minWidth = 50,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "flag"),
                        src = sprintf("flags/%s.svg", value),
                        # style = "border-radius:60%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                )
                
              )
    )})
  
  output$draft_4 <- renderReactable({
    reactable(draft_order[draft_order$round == 4, c(1,9, 5,6,8)],
              rownames = FALSE,
              compact = TRUE,
              minRows = 8,
              striped = TRUE,
              columns = list(
                
                overall = colDef(name = "Pick",
                                 minWidth = 50,
                                 align = "left"),
                
                selections = colDef(name = "Player",
                                    minWidth = 110,
                                    align = "left"
                ),
                
                position = colDef(name = "Position",
                                  minWidth = 75,
                                  align = "center"),
                logo = colDef(
                  name = "Team",
                  minWidth = 70,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "logo"),
                        src = sprintf("manager_logo/%s.jpg", value),
                        style = "border-radius:50%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                ),
                
                nation = colDef(
                  name = "",
                  minWidth = 50,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "flag"),
                        src = sprintf("flags/%s.svg", value),
                        # style = "border-radius:60%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                )
                
              )
    )})
  
  output$draft_5 <- renderReactable({
    reactable(draft_order[draft_order$round == 5, c(1,9, 5,6,8)],
              rownames = FALSE,
              compact = TRUE,
              minRows = 8,
              striped = TRUE,
              columns = list(
                
                overall = colDef(name = "Pick",
                                 minWidth = 50,
                                 align = "left"),
                
                selections = colDef(name = "Player",
                                    minWidth = 110,
                                    align = "left"
                ),
                
                position = colDef(name = "Position",
                                  minWidth = 75,
                                  align = "center"),
                logo = colDef(
                  name = "Team",
                  minWidth = 70,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "logo"),
                        src = sprintf("manager_logo/%s.jpg", value),
                        style = "border-radius:50%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                ),
                
                nation = colDef(
                  name = "",
                  minWidth = 50,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "flag"),
                        src = sprintf("flags/%s.svg", value),
                        # style = "border-radius:60%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                )
                
              )
    )})
  
  output$draft_6 <- renderReactable({
    reactable(draft_order[draft_order$round == 6, c(1,9, 5,6,8)],
              rownames = FALSE,
              compact = TRUE,
              minRows = 8,
              striped = TRUE,
              columns = list(
                
                overall = colDef(name = "Pick",
                                 minWidth = 50,
                                 align = "left"),
                
                selections = colDef(name = "Player",
                                    minWidth = 110,
                                    align = "left"
                ),
                
                position = colDef(name = "Position",
                                  minWidth = 75,
                                  align = "center"),
                logo = colDef(
                  name = "Team",
                  minWidth = 70,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "logo"),
                        src = sprintf("manager_logo/%s.jpg", value),
                        style = "border-radius:50%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                ),
                
                nation = colDef(
                  name = "",
                  minWidth = 50,
                  cell = function(value) {
                    div(
                      class = "team",
                      img(
                        class = "flag",
                        alt = paste(value, "flag"),
                        src = sprintf("flags/%s.svg", value),
                        # style = "border-radius:60%;max-width:100%;height:30px;border:1pxsolid#ddd;",
                      )
                    )
                  }
                )
                
              )
    )})
 
  output$draft_stats <- renderFormattable({
    formattable(
      draft_order %>% 
        group_by(nation, position) %>%
        tally(n = "count") %>%
        ungroup() %>%
        arrange(desc(count)) %>%
        # slice_head(n = 10) %>%
        mutate(nation = if_else(nation == "usa", "USA", to_sentence_case(nation)),
               pct = count/sum(count)) %>% 
        slice_head(n = 10) %>%
        rename_with(to_sentence_case),
      align = c("l", "l", "c", "r"),
      list(Count = color_bar("#0cd424"),
           Pct = formatter("span", x ~ percent(x),
                           style = ~ style(color = "black", "font-weight" = "bold")))
      # columns = list(
      #   nation = colDef(name = "Nation"),
      #   position = colDef(name = "Position"),
      #   count = colDef(name = "Count")
      # )
      )
    })
  
  #### war pigs ####
  output$double_dions <- renderReactable({
    reactable(draft_order[draft_order$team == "Double Dions", c(9, 5,6, 10)], 
              rownames = FALSE,
              defaultSorted = "pos_fac",
              defaultSortOrder = "asc",
              
              columns = list(
                
                selections = colDef(
                  name = "Player",
                  minWidth = 100, 
                  align = "left",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                position = colDef(
                  name = "Pos",
                  minWidth = 50, 
                  align = "center",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                pos_fac = colDef(show = FALSE),
                
                nation = colDef(
                  name = "", 
                  minWidth = 25,
                  align = "right",
                  cell = function(value) {
                    div(
                      class = "team",
                      img(class = "flag", alt = paste(value, "flag"), src = sprintf("flags/%s.svg", value))
                    )
                  }
                )
              )
    )
  })
  
  #### dernic ####
  output$dernic_dynasty <- renderReactable({
    reactable(draft_order[draft_order$team == "Dernic Dynasty", c(9, 5,6, 10)], 
              rownames = FALSE,
              defaultSorted = "pos_fac",
              defaultSortOrder = "asc",
              
              columns = list(
                
                selections = colDef(
                  name = "Player",
                  minWidth = 100, 
                  align = "left",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                position = colDef(
                  name = "Pos",
                  minWidth = 50, 
                  align = "center",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                pos_fac = colDef(show = FALSE),
                
                nation = colDef(
                  name = "", 
                  minWidth = 25,
                  align = "right",
                  cell = function(value) {
                    div(
                      class = "team",
                      img(class = "flag", alt = paste(value, "flag"), src = sprintf("flags/%s.svg", value))
                    )
                  }
                )
              )
    )
  })
  
  #### yorkton ####
  output$yorkton_g_spots <- renderReactable({
    reactable(draft_order[draft_order$team == "Yorkton G-Spots", c(9, 5,6, 10)], 
              rownames = FALSE,
              defaultSorted = "pos_fac",
              defaultSortOrder = "asc",
              
              columns = list(
                
                selections = colDef(
                  name = "Player",
                  minWidth = 100, 
                  align = "left",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                position = colDef(
                  name = "Pos",
                  minWidth = 50, 
                  align = "center",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                pos_fac = colDef(show = FALSE),
                
                nation = colDef(
                  name = "", 
                  minWidth = 25,
                  align = "right",
                  cell = function(value) {
                    div(
                      class = "team",
                      img(class = "flag", alt = paste(value, "flag"), src = sprintf("flags/%s.svg", value))
                    )
                  }
                )
              )
    )
  })
  
  #### relitz relish ####
  output$relitz_relish <- renderReactable({
    reactable(draft_order[draft_order$team == "Relitz Relish", c(9, 5,6, 10)], 
              rownames = FALSE,
              defaultSorted = "pos_fac",
              defaultSortOrder = "asc",
              
              columns = list(
                
                selections = colDef(
                  name = "Player",
                  minWidth = 100, 
                  align = "left",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                position = colDef(
                  name = "Pos",
                  minWidth = 50, 
                  align = "center",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                pos_fac = colDef(show = FALSE),
                
                nation = colDef(
                  name = "", 
                  minWidth = 25,
                  align = "right",
                  cell = function(value) {
                    div(
                      class = "team",
                      img(class = "flag", alt = paste(value, "flag"), src = sprintf("flags/%s.svg", value))
                    )
                  }
                )
              )
    )
  })
  
  #### campbells soup ####
  output$campbells_soup <- renderReactable({
    reactable(draft_order[draft_order$team == "Campbells Soup", c(9, 5,6, 10)], 
              rownames = FALSE,
              defaultSorted = "pos_fac",
              defaultSortOrder = "asc",
              
              columns = list(
                
                selections = colDef(
                  name = "Player",
                  minWidth = 100, 
                  align = "left",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                position = colDef(
                  name = "Pos",
                  minWidth = 50, 
                  align = "center",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                pos_fac = colDef(show = FALSE),
                
                nation = colDef(
                  name = "", 
                  minWidth = 25,
                  align = "right",
                  cell = function(value) {
                    div(
                      class = "team",
                      img(class = "flag", alt = paste(value, "flag"), src = sprintf("flags/%s.svg", value))
                    )
                  }
                )
              )
    )
  })
  
  #### mikado ####
  output$mikado_magic <- renderReactable({
    reactable(draft_order[draft_order$team == "Mikado Magic", c(9, 5,6, 10)], 
              rownames = FALSE,
              defaultSorted = "pos_fac",
              defaultSortOrder = "asc",
              
              columns = list(
                
                selections = colDef(
                  name = "Player",
                  minWidth = 100, 
                  align = "left",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                position = colDef(
                  name = "Pos",
                  minWidth = 50, 
                  align = "center",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                pos_fac = colDef(show = FALSE),
                
                nation = colDef(
                  name = "", 
                  minWidth = 25,
                  align = "right",
                  cell = function(value) {
                    div(
                      class = "team",
                      img(class = "flag", alt = paste(value, "flag"), src = sprintf("flags/%s.svg", value))
                    )
                  }
                )
              )
    )
  })
  
  #### macrae ####
  output$craigs_button <- renderReactable({
    reactable(draft_order[draft_order$team == "Craigs Button", c(9, 5,6, 10)], 
              rownames = FALSE,
              defaultSorted = "pos_fac",
              defaultSortOrder = "asc",
              
              columns = list(
                
                selections = colDef(
                  name = "Player",
                  minWidth = 100, 
                  align = "left",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                position = colDef(
                  name = "Pos",
                  minWidth = 50, 
                  align = "center",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                pos_fac = colDef(show = FALSE),
                
                nation = colDef(
                  name = "", 
                  minWidth = 25,
                  align = "right",
                  cell = function(value) {
                    div(
                      class = "team",
                      img(class = "flag", alt = paste(value, "flag"), src = sprintf("flags/%s.svg", value))
                    )
                  }
                )
              )
    )
  })
  
  #### top chelf ####
  output$cozens_eddie <- renderReactable({
    reactable(draft_order[draft_order$team == "Cozens Eddie", c(9, 5,6, 10)], 
              rownames = FALSE,
              defaultSorted = "pos_fac",
              defaultSortOrder = "asc",
              
              columns = list(
                
                selections = colDef(
                  name = "Player",
                  minWidth = 100, 
                  align = "left",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                position = colDef(
                  name = "Pos",
                  minWidth = 50, 
                  align = "center",
                  style = "font-size:21px;font-weight: 500;",
                  headerStyle = "font-size:24px;font-weight: 700;"
                ),
                
                pos_fac = colDef(show = FALSE),
                
                nation = colDef(
                  name = "", 
                  minWidth = 25,
                  align = "right",
                  cell = function(value) {
                    div(
                      class = "team",
                      img(class = "flag", alt = paste(value, "flag"), src = sprintf("flags/%s.svg", value))
                    )
                  }
                )
              )
    )
  })

  #### Matches ####
  output$full_schedule <- renderReactable({
    reactable(schedule,
              # groupBy = c("date"),
              defaultPageSize = 30,
              rownames = FALSE,
              columns = list(
                phase = colDef(name = "Phase",
                               minWidth = 100, 
                               align = "left"),
                
                date = colDef(name = "Date",
                              minWidth = 100,
                              align = "left",
                              format = colFormat(date = TRUE)
                              ),
                
                time = colDef(name = "Time",
                              minWidth = 50,
                              align = "left"
                              ),
                
                home_team = colDef(name = "Home",
                              minWidth = 60,
                              align = "left",
                              cell = function(value) {
                                div(
                                  class = "team",
                                  img(class = "flag", alt = paste(value, "flag"), src = sprintf("flags/%s.svg", value))
                                  # div(class = "team-name", value)
                                )
                              }
                              ),
                
                away_team = colDef(name = "Away",
                              minWidth = 60,
                              align = "left",
                              cell = function(value) {
                                div(
                                  class = "team",
                                  img(class = "flag", alt = paste(value, "flag"), src = sprintf("flags/%s.svg", value))
                                  # div(class = "team-name", value)
                                )
                              }
                              )
                              
                )
                
                )
  })
  


 
#### country rosters ####
  
output$canada <- renderReactable({
  reactable(national_rosters[national_rosters$nation == "canada", ],
            defaultPageSize = 30, 
            compact = TRUE,
            sortable = FALSE,
            striped = TRUE,
            
            columns = list(
              position = colDef(
                name = "Pos"
              ),
              player = colDef(
                name = "Player",
                minWidth = 150
              ),
              team = colDef(
                name = "Team",
                minWidth = 150
              ),
              league = colDef(
                name = "League",
                minWidth = 150
              ),
              draft_eligible = colDef(
                name = "Draft Yr"
              ),
              nation = colDef(
                show = FALSE
              ),
              
              pos_fac = colDef(
                show = FALSE
                )
            )
            
            
            )})

output$austria <- renderReactable({
  reactable(national_rosters[national_rosters$nation == "austria", ],
            defaultPageSize = 30, 
            compact = TRUE,
            sortable = FALSE,
            striped = TRUE,
            
            columns = list(
              position = colDef(
                name = "Pos"
              ),
              player = colDef(
                name = "Player",
                minWidth = 150
              ),
              team = colDef(
                name = "Team",
                minWidth = 150
              ),
              league = colDef(
                name = "League",
                minWidth = 150
              ),
              draft_eligible = colDef(
                name = "Draft Yr"
              ),
              nation = colDef(
                show = FALSE
              ),
              
              pos_fac = colDef(
                show = FALSE
              )
            )
            
            
  )})

output$czech <- renderReactable({
  reactable(national_rosters[national_rosters$nation == "czech_republic", ],
            defaultPageSize = 30, 
            compact = TRUE,
            sortable = FALSE,
            striped = TRUE,
            
            columns = list(
              position = colDef(
                name = "Pos"
              ),
              player = colDef(
                name = "Player",
                minWidth = 150
              ),
              team = colDef(
                name = "Team",
                minWidth = 150
              ),
              league = colDef(
                name = "League",
                minWidth = 150
              ),
              draft_eligible = colDef(
                name = "Draft Yr"
              ),
              nation = colDef(
                show = FALSE
              ),
              
              pos_fac = colDef(
                show = FALSE
              )
            )
            
            
  )})

output$finland <- renderReactable({
  reactable(national_rosters[national_rosters$nation == "finland", ],
            defaultPageSize = 30, 
            compact = TRUE,
            sortable = FALSE,
            striped = TRUE,
            
            columns = list(
              position = colDef(
                name = "Pos"
              ),
              player = colDef(
                name = "Player",
                minWidth = 150
              ),
              team = colDef(
                name = "Team",
                minWidth = 150
              ),
              league = colDef(
                name = "League",
                minWidth = 150
              ),
              draft_eligible = colDef(
                name = "Draft Yr"
              ),
              nation = colDef(
                show = FALSE
              ),
              
              pos_fac = colDef(
                show = FALSE
              )
            )
  )})

output$germany <- renderReactable({
  reactable(national_rosters[national_rosters$nation == "germany", ],
            defaultPageSize = 30, 
            compact = TRUE,
            sortable = FALSE,
            striped = TRUE,
            
            columns = list(
              position = colDef(
                name = "Pos"
              ),
              player = colDef(
                name = "Player",
                minWidth = 150
              ),
              team = colDef(
                name = "Team",
                minWidth = 150
              ),
              league = colDef(
                name = "League",
                minWidth = 150
              ),
              draft_eligible = colDef(
                name = "Draft Yr"
              ),
              nation = colDef(
                show = FALSE
              ),
              
              pos_fac = colDef(
                show = FALSE
              )
            )
  )})

output$russia <- renderReactable({
  reactable(national_rosters[national_rosters$nation == "russia", ],
            defaultPageSize = 30, 
            compact = TRUE,
            sortable = FALSE,
            striped = TRUE,
            
            columns = list(
              position = colDef(
                name = "Pos"
              ),
              player = colDef(
                name = "Player",
                minWidth = 150
              ),
              team = colDef(
                name = "Team",
                minWidth = 150
              ),
              league = colDef(
                name = "League",
                minWidth = 150
              ),
              draft_eligible = colDef(
                name = "Draft Yr"
              ),
              nation = colDef(
                show = FALSE
              ),
              
              pos_fac = colDef(
                show = FALSE
              )
            )
  )})

output$slovakia <- renderReactable({
  reactable(national_rosters[national_rosters$nation == "slovakia", ],
            defaultPageSize = 30, 
            compact = TRUE,
            sortable = FALSE,
            striped = TRUE,
            
            columns = list(
              position = colDef(
                name = "Pos"
              ),
              player = colDef(
                name = "Player",
                minWidth = 150
              ),
              team = colDef(
                name = "Team",
                minWidth = 150
              ),
              league = colDef(
                name = "League",
                minWidth = 150
              ),
              draft_eligible = colDef(
                name = "Draft Yr"
              ),
              nation = colDef(
                show = FALSE
              ),
              
              pos_fac = colDef(
                show = FALSE
              )
            )
  )})

output$sweden <- renderReactable({
  reactable(national_rosters[national_rosters$nation == "sweden", ],
            defaultPageSize = 30, 
            compact = TRUE,
            sortable = FALSE,
            striped = TRUE,
            
            columns = list(
              position = colDef(
                name = "Pos"
              ),
              player = colDef(
                name = "Player",
                minWidth = 150
              ),
              team = colDef(
                name = "Team",
                minWidth = 150
              ),
              league = colDef(
                name = "League",
                minWidth = 150
              ),
              draft_eligible = colDef(
                name = "Draft Yr"
              ),
              nation = colDef(
                show = FALSE
              ),
              
              pos_fac = colDef(
                show = FALSE
              )
            )
  )})

output$switzerland <- renderReactable({
  reactable(national_rosters[national_rosters$nation == "switzerland", ],
            defaultPageSize = 30, 
            compact = TRUE,
            sortable = FALSE,
            striped = TRUE,
            
            columns = list(
              position = colDef(
                name = "Pos"
              ),
              player = colDef(
                name = "Player",
                minWidth = 150
              ),
              team = colDef(
                name = "Team",
                minWidth = 150
              ),
              league = colDef(
                name = "League",
                minWidth = 150
              ),
              draft_eligible = colDef(
                name = "Draft Yr"
              ),
              nation = colDef(
                show = FALSE
              ),
              
              pos_fac = colDef(
                show = FALSE
              )
            )
  )})

output$usa <- renderReactable({
  reactable(national_rosters[national_rosters$nation == "usa", ],
            defaultPageSize = 30, 
            compact = TRUE,
            sortable = FALSE,
            striped = TRUE,
            
            columns = list(
              position = colDef(
                name = "Pos"
              ),
              player = colDef(
                name = "Player",
                minWidth = 150
              ),
              team = colDef(
                name = "Team",
                minWidth = 150
              ),
              league = colDef(
                name = "League",
                minWidth = 150
              ),
              draft_eligible = colDef(
                name = "Draft Yr"
              ),
              nation = colDef(
                show = FALSE
              ),
              
              pos_fac = colDef(
                show = FALSE
              )
            )
  )})


}#### final brace, dont remove ####

shinyApp(ui, server)