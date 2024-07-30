packages <- c(
  "shiny",
  "shinyjs",
  "shinythemes",
  "tidyverse",
  "lubridate",
  "plotly",
  "DT",
  "waffle",
  "scales"
)

installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

library(shiny)
library(shinyjs)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(plotly)
library(DT)
library(waffle)
library(scales)

source('pages/seasons.R', local = TRUE)
source('pages/laptimes.R', local = TRUE)
source('pages/best.R', local = TRUE)
source('pages/driverFinishes.R', local = TRUE)
source('pages/pitStops.R', local = TRUE)
# Load data
constructor_results <- read.csv("data/constructor_results.csv")
results <- read.csv("data/results.csv")
constructors <- read.csv("data/constructors.csv")
drivers <- read.csv("data/drivers.csv")
races <- read.csv("data/races.csv")
lap_times <- read.csv("data/lap_times.csv")
qualifying <- read.csv("data/qualifying.csv")
team_colors <- read.csv("data/team_colors.csv")
pit_stops <- read.csv("data/pit_stops.csv")

drivers <- drivers %>% 
  mutate(driverName = paste(forename, surname))

cr <- constructor_results %>% 
  inner_join(races, by="raceId") %>% 
  inner_join(constructors, by="constructorId")

dr <- results %>% 
  inner_join(races, by="raceId") %>% 
  inner_join(drivers, by="driverId") %>% 
  mutate(driverName = paste(forename, surname)) %>% 
  mutate(position = ifelse(position == "\\N", "DNF", position))

ui <- fluidPage(theme = shinytheme("flatly"),
                useShinyjs(),
                titlePanel("F1 stats"),
                
                tabsetPanel(
                  seasonsUI,
                  laptimesUI,
                  bestUI,
                  driverFinishesUI,
                  pitStopsUI
                )
)

server <- function(input, output, session) {
  source('pages/seasons.R', local = TRUE)
  source('pages/laptimes.R', local = TRUE)
  source('pages/best.R', local = TRUE)
  source('pages/driverFinishes.R', local = TRUE)
source('pages/pitStops.R', local = TRUE)
  
  seasonsServer(input, output, session, cr, dr, team_colors)
  laptimesServer(input, output, session, lap_times, races, results, drivers, constructors, team_colors)
  bestServer(input, output, session, dr, constructors, qualifying, drivers, team_colors)
  driverFinishesServer(input, output, session, dr)
  pitStopsServer(input, output, pit_stops, races, results, constructors, team_colors)
}

shinyApp(ui = ui, server = server)