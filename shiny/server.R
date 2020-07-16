library(shiny)
library(gghighlight)
library(fmsb)
library(gridExtra)
library(tidyverse)
library(utf8)
library(shinyBS)
library(shinycssloaders)
library(shinydashboard)
library(shinyjs)
library(shinyWidgets)
library(rintrojs)
library(formattable)
library(knitr)
library(grid)
library(gridExtra)
library(cowplot)
library(gt)
library(plotly)
library(htmlwidgets)
library(scales) 
library(zoo)
source("dis_theme.R")
source("datainstate_cols.R")
load("teams_data.Rdata")
load("players_data.Rdata")

server <- function(input, output, session) {
  ## Adjust selection options based on team and date range
  observeEvent(
    input$team,
    updateSelectInput(session, "opp_name", "select your team's opponent", 
                      choices = unique(teams_data %>%
                                         filter(team == input$team &
                                                  date_time_utc >= min(input$daterange) &
                                                  date_time_utc <= max(input$daterange)) %>%
                                         pull(opp_team)))
  )
  observeEvent(
    input$daterange,
    updateSelectInput(session, "opp_name", "select your team's opponent",
                      choices = unique(teams_data %>%
                                         filter(team == input$team &
                                                  date_time_utc >= min(input$daterange) &
                                                  date_time_utc <= max(input$daterange)) %>%
                                         pull(opp_team)))
  )
  

  ## Create the data frames we will need based on selections
  team_all <- reactive({
    teams_data %>% 
      filter(team == input$team)
  })
  team_vs_opp <- reactive({
    teams_data %>% 
      filter(team == input$team & opp_team == input$opp_name)
  })
  team_date <- reactive({
    teams_data %>% 
      filter(team == input$team & date_time_utc >= min(input$daterange) &
               date_time_utc <= max(input$daterange))
  })
  team_opp_date <- reactive({
    teams_data %>% 
      filter(team == input$team & opp_team == input$opp_name & 
               date_time_utc >= min(input$daterange) &
               date_time_utc <= max(input$daterange))
  })
  players_all <- reactive({
    players_data %>% 
      filter(team == input$team) %>%
      mutate(dk_points = ((kills*3)+(assists*2)+(deaths*-1)+(cs*0.02)+
                            (ifelse(kills >= 10, 2, 0))+
                            ifelse(kills >= 10, 2, 0)))
  })
  players_vs_opp <- reactive({
    players_data %>% 
      filter(team == input$team & team_vs == input$opp_name)%>%
      mutate(dk_points = ((kills*3)+(assists*2)+(deaths*-1)+(cs*0.02)+
                            (ifelse(kills >= 10, 2, 0))+
                            ifelse(kills >= 10, 2, 0)))
  })
  players_date <- reactive({
    players_data %>% 
      filter(team == input$team & date_time_utc >= min(input$daterange) &
               date_time_utc <= max(input$daterange))%>%
      mutate(dk_points = ((kills*3)+(assists*2)+(deaths*-1)+(cs*0.02)+
                            (ifelse(kills >= 10, 2, 0))+
                            ifelse(kills >= 10, 2, 0)))
  })
  players_opp_date <- reactive({
    players_data %>% 
      filter(team == input$team & team_vs == input$opp_name & 
               date_time_utc >= min(input$daterange) &
               date_time_utc <= max(input$daterange))%>%
      mutate(dk_points = ((kills*3)+(assists*2)+(deaths*-1)+(cs*0.02)+
                            (ifelse(kills >= 10, 2, 0))+
                            ifelse(kills >= 10, 2, 0)))
  })
  radar_plot_data <- reactive({
    players_data %>%
      filter(((team == input$team & team_vs == input$opp_name) | 
                (team == input$opp_name & team_vs == input$team)) &
               date_time_utc >= min(input$daterange) &
               date_time_utc <= max(input$daterange))
  })
  trendline_total <- reactive({
    teams_data %>%
      filter(date_time_utc >= min(teams_data%>%
                                    filter(team == input$team)%>%
                                    pull(date_time_utc), na.rm = T) &
               date_time_utc <= max(teams_data%>%
                                      filter(team == input$team)%>%
                                      pull(date_time_utc), na.rm = T)) %>%
      filter(!is.na(gold) & !is.na(dragons) & !is.na(barons)) %>%
      mutate(date_time_utc = as.Date(date_time_utc),
             gold_diff = gold - opp_gold) %>%
      arrange(date_time_utc) %>%
      select(team, is_winner, gold, gold_diff, dragons, barons, date_time_utc) %>%
    mutate(team = ifelse(team != input$team, 
                         ifelse(team == input$opp_name, input$opp_name, "league"),
                         input$team)) %>%
      group_by(date_time_utc, team) %>%
      summarize(team = team, is_winner = mean(is_winner, na.rm = TRUE), 
                gold = mean(gold, na.rm = TRUE), 
                gold_diff = mean(gold_diff, na.rm = TRUE),
                dragons = mean(dragons, na.rm = TRUE),
                barons = mean(barons, na.rm = TRUE),
                date_time_utc = date_time_utc) %>%
      unique() %>%
      ungroup() %>%
      group_by(team) %>%
      mutate(cumgold = cumsum(gold),
             roll_avg_gold = rollapply(lag(gold), 5, 
                                       mean, na.rm=T, 
                                       partial = F,
                                       fill = NA, 
                                       align = "right")) %>%
      ungroup()
  })


  ## Create the table
  output$opponents_table <- render_gt(
    bind_rows(
      team_all() %>%
      select(is_winner, kills, gold, dragons, barons, towers, inhibitors,
             rift_heralds) %>%
      summarize(Stat = "Average",
                `win-%` = percent(mean(is_winner, na.rm = TRUE)),
                kills = mean(kills, na.rm = TRUE),
                gold = mean(gold, na.rm = TRUE),
                dragons = mean(dragons, na.rm = TRUE),
                barons = mean(barons, na.rm = TRUE),
                towers = mean(towers, na.rm = TRUE),
                inhibitors = mean(inhibitors, na.rm = TRUE),
                `rift heralds` = mean(rift_heralds, na.rm = TRUE)),

      team_date() %>%
      select(is_winner, kills, gold, dragons, barons, towers, inhibitors,
             rift_heralds) %>%
      summarize(Stat = "Date Range Average",
                `win-%` = percent(mean(is_winner, na.rm = TRUE)),
                kills = mean(kills, na.rm = TRUE),
                gold = mean(gold, na.rm = TRUE),
                dragons = mean(dragons, na.rm = TRUE),
                barons = mean(barons, na.rm = TRUE),
                towers = mean(towers, na.rm = TRUE),
                inhibitors = mean(inhibitors, na.rm = TRUE),
                `rift heralds` = mean(rift_heralds, na.rm = TRUE)),

      team_vs_opp() %>%
      select(is_winner, kills, gold, dragons, barons, towers, inhibitors,
             rift_heralds) %>%
      summarize(Stat = "Average vs Opponent",
                `win-%` = percent(mean(is_winner, na.rm = TRUE)),
                kills = mean(kills, na.rm = TRUE),
                gold = mean(gold, na.rm = TRUE),
                dragons = mean(dragons, na.rm = TRUE),
                barons = mean(barons, na.rm = TRUE),
                towers = mean(towers, na.rm = TRUE),
                inhibitors = mean(inhibitors, na.rm = TRUE),
                `rift heralds` = mean(rift_heralds, na.rm = TRUE)),

      team_opp_date() %>%
      select(is_winner, kills, gold, dragons, barons, towers, inhibitors,
             rift_heralds) %>%
      summarize(Stat = "Date Range Avg vs Opp",
                `win-%` = percent(mean(is_winner, na.rm = TRUE)),
                kills = mean(kills, na.rm = TRUE),
                gold = mean(gold, na.rm = TRUE),
                dragons = mean(dragons, na.rm = TRUE),
                barons = mean(barons, na.rm = TRUE),
                towers = mean(towers, na.rm = TRUE),
                inhibitors = mean(inhibitors, na.rm = TRUE),
                `rift heralds` = mean(rift_heralds, na.rm = TRUE))) %>%
      gt() %>%
      tab_header(title = "Average Stats Per Game") %>%
      cols_align(align = "center") %>%
      tab_options(table.width = pct(100))

  )
  
  output$radar_plot <- renderPlot({
    r1_plot <- radar_plot_data() %>%
      select(team, kills, deaths, assists, cs, gold, role) %>%
      mutate(kda = ifelse((kills+assists)/deaths == Inf, 
                          (kills+assists), 
                          (kills+assists)/deaths)) %>%
      mutate_at(c("kills", "deaths", "assists", "cs", "gold", "kda"), 
                ~(scale(.) %>% as.vector)) %>%
      mutate(is_team = ifelse(team == input$team, input$team, input$opp_name)) %>%
      filter(role == "Top") %>%
      group_by(is_team) %>%
      summarize(
                kills = mean(kills, na.rm = T), 
                deaths = mean(deaths, na.rm = T), 
                assists = mean(assists, na.rm = T), 
                kda = mean(kda, na.rm = T),
                cs = mean(cs, na.rm = T), 
                gold = mean(gold, na.rm = T)) %>%
      gather(key = "stat", value = "value", -is_team) %>%
      arrange(stat) %>%
      ggplot(aes(x = stat, y = value, group = is_team, fill = is_team, color = is_team))+
      scale_fill_datainstate()+
      scale_color_datainstate()+
      theme_set(theme_dis)+
      geom_point()+
      geom_polygon(alpha = 0.5, rule = "winding")+
      coord_polar()+
      theme(axis.title = element_blank(),
            legend.position = "none",
            axis.text.y = element_blank())+
      ggtitle("Top Lane")
    
    r2_plot <- radar_plot_data() %>%
      select(team, kills, deaths, assists, cs, gold, role) %>%
      mutate(kda = ifelse((kills+assists)/deaths == Inf, 
                          (kills+assists), 
                          (kills+assists)/deaths)) %>%
      mutate_at(c("kills", "deaths", "assists", "cs", "gold", "kda"), 
                ~(scale(.) %>% as.vector)) %>%
      mutate(is_team = ifelse(team == input$team, input$team, input$opp_name)) %>%
      filter(role == "Mid") %>%
      group_by(is_team) %>%
      summarize(
        kills = mean(kills, na.rm = T), 
        deaths = mean(deaths, na.rm = T), 
        assists = mean(assists, na.rm = T), 
        kda = mean(kda, na.rm = T),
        cs = mean(cs, na.rm = T), 
        gold = mean(gold, na.rm = T)) %>%
      gather(key = "stat", value = "value", -is_team) %>%
      arrange(stat) %>%
      ggplot(aes(x = stat, y = value, group = is_team, fill = is_team, color = is_team))+
      scale_fill_datainstate()+
      scale_color_datainstate()+
      theme_set(theme_dis)+
      geom_point()+
      geom_polygon(alpha = 0.5, rule = "winding")+
      coord_polar()+
      theme(axis.title = element_blank(),
            legend.position = "none",
            axis.text.y = element_blank())+
      ggtitle("Mid Lane")
    
    r3_plot <- radar_plot_data() %>%
      select(team, kills, deaths, assists, cs, gold, role) %>%
      mutate(kda = ifelse((kills+assists)/deaths == Inf, 
                          (kills+assists), 
                          (kills+assists)/deaths)) %>%
      mutate_at(c("kills", "deaths", "assists", "cs", "gold", "kda"), 
                ~(scale(.) %>% as.vector)) %>%
      mutate(is_team = ifelse(team == input$team, input$team, input$opp_name)) %>%
      filter(role == "Jungle") %>%
      group_by(is_team) %>%
      summarize(
        kills = mean(kills, na.rm = T), 
        deaths = mean(deaths, na.rm = T), 
        assists = mean(assists, na.rm = T), 
        kda = mean(kda, na.rm = T),
        cs = mean(cs, na.rm = T), 
        gold = mean(gold, na.rm = T)) %>%
      gather(key = "stat", value = "value", -is_team) %>%
      arrange(stat) %>%
      ggplot(aes(x = stat, y = value, group = is_team, fill = is_team, color = is_team))+
      scale_fill_datainstate()+
      scale_color_datainstate()+
      theme_set(theme_dis)+
      geom_point()+
      geom_polygon(alpha = 0.5, rule = "winding")+
      coord_polar()+
      theme(axis.title = element_blank(),
            legend.position = "none",
            axis.text.y = element_blank())+
      ggtitle("Jungler")
    
    r4_plot <- radar_plot_data() %>%
      select(team, kills, deaths, assists, cs, gold, role) %>%
      mutate(kda = ifelse((kills+assists)/deaths == Inf, 
                          (kills+assists), 
                          (kills+assists)/deaths)) %>%
      mutate_at(c("kills", "deaths", "assists", "cs", "gold", "kda"), 
                ~(scale(.) %>% as.vector)) %>%
      mutate(is_team = ifelse(team == input$team, input$team, input$opp_name)) %>%
      filter(role == "AD Carry") %>%
      group_by(is_team) %>%
      summarize(
        kills = mean(kills, na.rm = T), 
        deaths = mean(deaths, na.rm = T), 
        assists = mean(assists, na.rm = T), 
        kda = mean(kda, na.rm = T),
        cs = mean(cs, na.rm = T), 
        gold = mean(gold, na.rm = T)) %>%
      gather(key = "stat", value = "value", -is_team) %>%
      arrange(stat) %>%
      ggplot(aes(x = stat, y = value, group = is_team, fill = is_team, color = is_team))+
      scale_fill_datainstate()+
      scale_color_datainstate()+
      theme_set(theme_dis)+
      geom_point()+
      geom_polygon(alpha = 0.5, rule = "winding")+
      coord_polar()+
      theme(axis.title = element_blank(),
            legend.position = "none",
            axis.text.y = element_blank())+
      ggtitle("ADC")
    
    r5_plot <- radar_plot_data() %>%
      select(team, kills, deaths, assists, cs, gold, role) %>%
      mutate(kda = ifelse((kills+assists)/deaths == Inf, 
                          (kills+assists), 
                          (kills+assists)/deaths)) %>%
      mutate_at(c("kills", "deaths", "assists", "cs", "gold", "kda"), 
                ~(scale(.) %>% as.vector)) %>%
      mutate(team = ifelse(team == input$team, input$team, input$opp_name)) %>%
      filter(role == "Support") %>%
      group_by(team) %>%
      summarize(
        kills = mean(kills, na.rm = T), 
        deaths = mean(deaths, na.rm = T), 
        assists = mean(assists, na.rm = T), 
        kda = mean(kda, na.rm = T),
        cs = mean(cs, na.rm = T), 
        gold = mean(gold, na.rm = T)) %>%
      gather(key = "stat", value = "value", -team) %>%
      arrange(stat) %>%
      ggplot(aes(x = stat, y = value, group = team, fill = team, color = team))+
      scale_fill_datainstate()+
      scale_color_datainstate()+
      theme_set(theme_dis)+
      geom_point()+
      geom_polygon(alpha = 0.5, rule = "winding")+
      coord_polar()+
      theme(axis.title = element_blank(),
            axis.text.y = element_blank())+
      ggtitle("Support")

    grid.arrange(grobs = list(r1_plot, r2_plot, r3_plot, 
                              r4_plot, r5_plot), 
                 ncol = 3)
  })
  output$gold_plot <- renderPlot({
    gold_data <- radar_plot_data() %>%
      group_by(role, team) %>%
      summarize(gold = mean(gold, na.rm = TRUE)) %>%
      ungroup()
      
    ggplot(gold_data, aes(x = role, y = gold, fill = team)) +
      scale_fill_datainstate() + 
      theme_set(theme_dis)+
      geom_bar(data = subset(gold_data,team == input$opp_name), stat = "identity") + 
      geom_bar(data = subset(gold_data,team == input$team), stat = "identity",
               aes(y=gold*(-1))) + 
      scale_y_continuous(breaks = seq(-20000, 20000, 5000),
                         labels = c(20000,15000,10000,5000,0,
                                  5000,10000,15000,20000)) + 
      coord_flip() +
      ggtitle("Average Gold per Game by Position") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$gold_total <- renderPlotly({
    ggplotly(trendline_total() %>%
      ggplot(aes(x=date_time_utc, color = team)) + 
      scale_color_datainstate() + 
      theme_set(theme_dis)+
      geom_line(aes(y=roll_avg_gold)) +
      geom_point(aes(y=roll_avg_gold)) +
      ggtitle("Gold Five Game Rolling Average") +
      xlab("Date") + 
      ylab("") +
      scale_y_continuous(labels = comma) 
    )
  })
  output$gold_date <- renderPlotly({
    ggplotly(trendline_total() %>%
               filter(date_time_utc >= min(input$daterange) &
                        date_time_utc <= max(input$daterange)) %>%
               ggplot(aes(x=date_time_utc, color = team)) + 
               scale_color_datainstate() + 
               theme_set(theme_dis)+
               geom_line(aes(y=roll_avg_gold)) +
               geom_point(aes(y=roll_avg_gold)) +
               ggtitle("Gold Five Game Rolling Average Selected Range") +
               xlab("Date") + 
               ylab("") +
               scale_y_continuous(labels = comma) 
    )
  })
  output$golddiff_total <- renderPlotly({
    ggplotly(trendline_total() %>%
               filter(team != "league") %>%
               ggplot(aes(x=date_time_utc, y = gold_diff ,color = team)) + 
               scale_color_datainstate() + 
               theme_set(theme_dis)+
               geom_line() +
               geom_point() +
               ggtitle("Gold Difference") +
               xlab("Date") + 
               ylab("") +
               scale_y_continuous(labels = comma) 
    )
  })
  output$golddiff_date <- renderPlotly({
    ggplotly(trendline_total() %>%
               filter(date_time_utc >= min(input$daterange) &
                        date_time_utc <= max(input$daterange)) %>%
               filter(team != "league") %>%
               ggplot(aes(x=date_time_utc,y = gold_diff, color = team)) + 
               scale_color_datainstate() + 
               theme_set(theme_dis)+
               geom_line() +
               geom_point() +
               ggtitle("Gold Difference Selected Range") +
               xlab("Date") + 
               ylab("") +
               scale_y_continuous(labels = comma) 
    )
  })
  output$dragons_total <- renderPlotly({
    ggplotly(trendline_total() %>%
               ggplot(aes(x=date_time_utc, y = cumsum(dragons), color = team)) + 
               scale_color_datainstate() + 
               theme_set(theme_dis)+
               geom_line() +
               geom_point() +
               ggtitle("Cummulative Dragons") +
               xlab("Date") + 
               ylab("") +
               scale_y_continuous(labels = comma) 
    )
  })
  output$dragons_date <- renderPlotly({
    ggplotly(trendline_total() %>%
               filter(date_time_utc >= min(input$daterange) &
                        date_time_utc <= max(input$daterange)) %>%
               ggplot(aes(x=date_time_utc,y = cumsum(dragons), color = team)) + 
               scale_color_datainstate() + 
               theme_set(theme_dis)+
               geom_line() +
               geom_point() +
               ggtitle("Cummulative Dragons Selected Range") +
               xlab("Date") + 
               ylab("") +
               scale_y_continuous(labels = comma) 
    )
  })
  output$barons_total <- renderPlotly({
    ggplotly(trendline_total() %>%
               ggplot(aes(x=date_time_utc, y = cumsum(barons), color = team)) + 
               scale_color_datainstate() + 
               theme_set(theme_dis)+
               geom_line() +
               geom_point() +
               ggtitle("Cummulative barons") +
               xlab("Date") + 
               ylab("") +
               scale_y_continuous(labels = comma) 
    )
  })
  output$barons_date <- renderPlotly({
    ggplotly(trendline_total() %>%
               filter(date_time_utc >= min(input$daterange) &
                        date_time_utc <= max(input$daterange)) %>%
               ggplot(aes(x=date_time_utc,y = cumsum(barons), color = team)) + 
               scale_color_datainstate() + 
               theme_set(theme_dis)+
               geom_line() +
               geom_point() +
               ggtitle("Cummulative barons Selected Range") +
               xlab("Date") + 
               ylab("") +
               scale_y_continuous(labels = comma) 
    )
  })
  output$top_performers_table <- render_gt(
  head(players_opp_date() %>%
    select(name, kills, deaths, assists, gold, cs, dk_points) %>%
    group_by(name) %>%
    summarize(name = name, kills = mean(kills, na.rm = TRUE),
              deaths = mean(deaths, na.rm = TRUE),
              assists = mean(assists, na.rm = TRUE),
              gold = mean(gold, na.rm = TRUE),
              cs = mean(cs, na.rm = TRUE),
              dk_points = mean(dk_points, na.rm = TRUE)) %>%
      unique() %>%
      ungroup() %>%
    arrange(desc(dk_points)), 5) %>%
    gt() %>%
    tab_header(title = "Top Performers") %>%
    cols_align(align = "center") %>%
    tab_options(table.width = pct(100))
    
  )
  output$top_performers_position <- render_gt(
    players_opp_date() %>%
           select(name, role, kills, deaths, assists, gold, cs, dk_points) %>%
           group_by(name, role) %>%
           summarize(name = name, role = role, kills = mean(kills, na.rm = TRUE),
                     deaths = mean(deaths, na.rm = TRUE),
                     assists = mean(assists, na.rm = TRUE),
                     gold = mean(gold, na.rm = TRUE),
                     cs = mean(cs, na.rm = TRUE),
                     dk_points = mean(dk_points, na.rm = TRUE)) %>%
           unique() %>%
           arrange(desc(dk_points)) %>%
           top_n(1) %>%
      ungroup() %>%
      gt() %>%
      tab_header(title = "Top Performers by Position") %>%
      cols_align(align = "center") %>%
      tab_options(table.width = pct(100))
    
  )
  output$common_lineup <- render_gt(
    players_opp_date() %>%
      select(name, role, champion, player_win, kills, deaths, assists, gold, 
             cs, dk_points) %>%
      mutate(player_win = ifelse(player_win == "Yes", 1,0)) %>%
      group_by(name, champion) %>%
      mutate(champion_count = n()) %>%
      filter(champion_count > 10) %>%
      summarize(name = name, champion = champion, 
                role = role, kills = mean(kills, na.rm = TRUE),
                deaths = mean(deaths, na.rm = TRUE),
                assists = mean(assists, na.rm = TRUE),
                gold = mean(gold, na.rm = TRUE),
                cs = mean(cs, na.rm = TRUE),
                dk_points = mean(dk_points, na.rm = TRUE)) %>%
      unique() %>%
      arrange(desc(dk_points)) %>%
      top_n(1) %>%
      ungroup() %>%
      gt() %>%
      tab_header(title = "Top Player and Champion Pairings",
                 subtitle = "More than 10 matches (average stats)") %>%
      cols_align(align = "center") %>%
      tab_options(table.width = pct(100))
    
  )

  
  ## Determine which panel to show
  observeEvent("", {
    shinyjs::hide("lineupsandchampions_panel")
    shinyjs::hide("trendsinperformance_panel")
    shinyjs::show("VsOpponent_panel")
  }, once = TRUE)
  observeEvent(input$VsOpponent, {
    shinyjs::hide("lineupsandchampions_panel")
    shinyjs::hide("trendsinperformance_panel")
    shinyjs::show("VsOpponent_panel")
  })
  observeEvent(input$trendsinperformance, {
    shinyjs::hide("lineupsandchampions_panel")
    shinyjs::hide("VsOpponent_panel")
    shinyjs::show("trendsinperformance_panel")
  })
  observeEvent(input$lineupsandchampions, {
    shinyjs::hide("trendsinperformance_panel")
    shinyjs::hide("VsOpponent_panel")
    shinyjs::show("lineupsandchampions_panel")
  })
}