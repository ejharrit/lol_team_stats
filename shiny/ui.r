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

ui <- dashboardPage(
  skin = "blue",
  title = "Data in State",
  
  # HEADER ------------------------------------------------------------------
  
  dashboardHeader(
    title = span(img(src = "logo.png", height = 35)),
    titleWidth = 100,
    dropdownMenu(
      type = "notifications", 
      headerText = strong("Links"), 
      icon = icon("at"), 
      badgeStatus = NULL,
      notificationItem(
        text = "@Leaguepedia",
        icon = icon("bezier-curve"),
        href = "https://lol.gamepedia.com/League_of_Legends_Esports_Wiki"
      ),
      notificationItem(
        text = "Source code",
        icon = icon("github-alt"),
        href = "https://github.com/ejharrit"
      ),
      notificationItem(
        text = "My twitter",
        icon = icon("twitter"),
        href = "https://twitter.com/ejharrit"
      ),
      notificationItem(
        text = "My twitch",
        icon = icon("twitch"),
        href = "https://www.twitch.tv/ejharrit"
      ),
      notificationItem(
        text = "My Linkedin",
        icon = icon("linkedin"),
        href = "https://www.linkedin.com/in/eddy-harrity-5aa150162/"
      ),
      notificationItem(
        text = "Support a cause",
        icon = icon("heart"),
        href = "https://www.extra-life.org/index.cfm?fuseaction=cms.home"
      )
    ),
    tags$li(
      a(
        strong("How to Use"),
        height = 40,
        href = "https://www.twitch.tv/ejharrit",
        title = "",
        target = "_blank"
      ),
      class = "dropdown"
    )
  ),  
  
 # SIDEBAR -----------------------------------------------------------------
  
  dashboardSidebar(width = 300,
                   menuItem("Dashboard", tabName = "dashboard", 
                            icon = icon("dashboard")),
                   selectInput("team", 
                               "Select your team", 
                               choices = unique(as.character(teams_data$team))
                   ),
                   dateRangeInput(
                     "daterange",
                     "select the data range you want to look at",
                     start = min(teams_data$date_time_utc),
                     end = max(teams_data$date_time_utc)
                   ),
                   selectInput(
                     "opp_name",
                     "select your team's opponent",
                     choices = "",
                     selected = ""
                   )
  ),

 # BODY -------------------------------------------------------------------- 
 
    dashboardBody(
     tags$head(
       tags$link(
         rel = "stylesheet",
         type = "text/css",
         href = "datainstate_style.css")
     ),
     useShinyjs(),
     introjsUI(),

 # MAIN BODY ---------------------------------------------------------------- 

     fluidRow(
       column(
         width = 12,
           bsButton("VsOpponent", 
                    label = "Vs Opponent", 
                    icon = icon("users"), 
                    style = "success"),
           bsButton("trendsinperformance", 
                    label = "Trends", 
                    icon = icon("digital-tachograph"), 
                    style = "success"),
           bsButton("lineupsandchampions", 
                    label = "Lineups and Champions", 
                    icon = icon("child"), 
                    style = "success")
       )
    ),

    fluidRow(
      div( id = "VsOpponent_panel",
        column(
          width = 12,
          gt_output("opponents_table")
        ),
        column(
          width = 12,
          plotOutput("radar_plot")
        ),
        column(
          width = 12,
          plotOutput("gold_plot")
        )
      )
     ),
 fluidRow(
   div( id = "trendsinperformance_panel",
     column(
       width = 6,
       plotlyOutput("gold_total")
     ),
     column(
       width = 6,
       plotlyOutput("gold_date")
     ),
     column(
       width = 6,
       plotlyOutput("golddiff_total")
     ),
     column(
       width = 6,
       plotlyOutput("golddiff_date")
     ),
     column(
       width = 6,
       plotlyOutput("dragons_total")
     ),
     column(
       width = 6,
       plotlyOutput("dragons_date")
     ),
     column(
       width = 6,
       plotlyOutput("barons_total")
     ),
     column(
       width = 6,
       plotlyOutput("barons_date")
     )
   )
 ),

 fluidRow(
   div( id = "lineupsandchampions_panel",
     column(
       width = 5,
       gt_output("top_performers_table")
     ),
     column(
       width = 7,
       gt_output("top_performers_position")
     ),
     column(
       width = 12,
       gt_output("common_lineup")
     ),
     column(
       width = 4,
       gt_output("most_common_bans")
     )
   )
 )
   
  )
    
)

