laptimesUI <- tabPanel("Laptimes",
                       fluidRow(
                         column(2,
                                selectInput(inputId = "laptimesViewSelect",
                                            label = "Select view",
                                            choices=c(
                                              "all seasons" = "all-seasons",
                                              "single season" = "single-season"
                                            ),
                                            selected = "all-seasons"
                                )),
                         column(10, uiOutput("laptimesFilterContainer"))),

                       plotlyOutput("laptimesPlot"),
                       textOutput("fastestLap"),
                       textOutput("lapRecord"))

laptimesServer <- function(input, output, session, laptimes, races, results, drivers, constructors, team_colors) {
  
  laptimes_year_race <- reactive({
    req(input$laptimesYearSelect, input$laptimesCircuitSelect)
    id <- races %>%
      filter(year == input$laptimesYearSelect & name == input$laptimesCircuitSelect) %>%
      pull(raceId)
    
    Q <- quantile(laptimes$milliseconds, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(laptimes$milliseconds)
    upper <- Q[2]+1.5*iqr
    lower <- Q[1]-1.5*iqr
    
    laptimes %>%
      filter(raceId == id) %>%
      inner_join(drivers, by="driverId") %>% 
      inner_join(results, by=c("raceId", "driverId")) %>% 
      inner_join(constructors, by="constructorId") %>% 
      inner_join(team_colors, by="constructorId") %>% 
      rename(laptime_ms = milliseconds.x) %>% 
      mutate(laptime = strftime(as.POSIXct(Sys.Date()) + laptime_ms / 1000, "%M:%OS3")) %>% 
      filter(laptime_ms < upper & laptime_ms > lower)
  })
  
  laptimes_race <- reactive({
    req(input$laptimesCircuitSelect)
    ids <- races %>% 
      filter(name == input$laptimesCircuitSelect) %>% 
      pull(raceId)
    
    laptimes %>% 
      filter(raceId %in% ids) %>% 
      rename(laptime_ms = milliseconds)
  })
  
  fastest_lap_time <- reactive({
    req(laptimes_year_race())
    laptimes_year_race() %>% 
      filter(laptime_ms == min(laptime_ms)) %>% 
      mutate(laptime = strftime(as.POSIXct(Sys.Date()) + laptime_ms / 1000, "%M:%OS3")) %>% 
      pull(laptime)
  })
  
  fastest_lap_driver <- reactive({
    req(laptimes_year_race())
    laptimes_year_race() %>% 
      filter(laptime_ms == min(laptime_ms)) %>% 
      mutate(laptime = strftime(as.POSIXct(Sys.Date()) + laptime_ms / 1000, "%M:%OS3")) %>% 
      pull(driverName)
  })
  
  lap_record_time <- reactive({
    req(laptimes_race())
    laptimes_race() %>% 
      filter(laptime_ms == min(laptime_ms)) %>% 
      mutate(laptime = strftime(as.POSIXct(Sys.Date()) + laptime_ms / 1000, "%M:%OS3")) %>% 
      pull(laptime)
  })
  
  lap_record_driver <- reactive({
    req(laptimes_race())
    laptimes_race() %>% 
      filter(laptime_ms == min(laptime_ms)) %>% 
      inner_join(drivers, by="driverId") %>% 
      mutate(laptime = strftime(as.POSIXct(Sys.Date()) + laptime_ms / 1000, "%M:%OS3")) %>% 
      pull(driverName)
  })
  
  plotAllSeasons <- function() {
    data <- laptimes_race() %>% 
      inner_join(races, by="raceId") %>% 
      group_by(year) %>% 
      summarise(mean = mean(laptime_ms), best = min(laptime_ms)) %>% 
      mutate(mean = strftime(as.POSIXct(Sys.Date()) + mean / 1000, "%M:%OS3"),
             best = strftime(as.POSIXct(Sys.Date()) + best / 1000, "%M:%OS3"))
    
    plot <- plot_ly() %>% 
      add_trace(data = data, 
                x=~year, 
                y=~mean, 
                type = 'scatter',
                mode = 'lines+markers',
                name='average laptime') %>% 
      add_trace(data = data, 
                x=~year, 
                y=~best,
                type = 'scatter',
                mode = 'lines+markers',
                name='fastest laptime') %>% 
      layout(yaxis=list(title='laptime'))
    
    plot
  }
  
  plotSingleSeason <- function() {
    data <- laptimes_year_race()
    
    driver_means <- data %>%
      group_by(driverName) %>%
      summarise(mean_laptime = median(laptime_ms)) %>%
      arrange(mean_laptime)
  
    groups <- data %>% 
      mutate(driverName = factor(driverName, levels = driver_means$driverName)) %>% 
      split(.$driverName)
    
    plot <- plot_ly()
    for (group in groups) {
      plot <- plot %>% 
        add_trace(data = group,
                  x = ~driverName,
                  y = ~laptime,
                  type = 'box',
                  name = ~driverName,
                  orientation = 'v',
                  fillcolor = alpha(group$color[1], 0.50),
                  line = list(color = group$color[1]),
                  marker = list(color = group$color[1]))
    }
    
    plot
  }
  
  observeEvent(input$laptimesViewSelect, {
    output$laptimesFilterContainer <- renderUI({
      if (input$laptimesViewSelect == "all-seasons") {
        selectInput(inputId = "laptimesCircuitSelect",
                    label = "Select circuit",
                    choices = races %>% select(name) %>% unique %>% pull(name))
      } else {
        fluidRow(
          column(6, sliderInput("laptimesYearSelect",
                                "Select season:",
                                min = 1950,
                                max = 2023,
                                value = 2023,
                                step = 1,
                                width = "100%")),
          column(4, selectInput(
            inputId = "laptimesCircuitSelect",
            label = "Select circuit",
            choices = NULL))
        )
      }
    })
  })
  
  observeEvent(input$laptimesViewSelect, {
    if (input$laptimesViewSelect == "all-seasons") {
      observeEvent(input$laptimesCircuitSelect, {
        req(input$laptimesCircuitSelect)
        output$laptimesPlot <- renderPlotly({
          plotAllSeasons()
        })
      })
    } else {
      observeEvent(input$laptimesYearSelect, {
        circuit_choices <- races %>% filter(year == input$laptimesYearSelect) %>% select(name) %>% pull(name)
        updateSelectInput(session, "laptimesCircuitSelect", choices = circuit_choices)
      })
      
      observeEvent(input$laptimesCircuitSelect, {
        req(input$laptimesYearSelect, input$laptimesCircuitSelect)
        output$laptimesPlot <- renderPlotly({
          plotSingleSeason()
        })
      })
    }
  })
  
  output$fastestLap <- renderText({
    req(input$laptimesViewSelect == "single-season", fastest_lap_driver(), fastest_lap_time())
    paste("Fastest lap: ", fastest_lap_driver(), " (", fastest_lap_time(), ")")
  })
  
  output$lapRecord <- renderText({
    req(input$laptimesViewSelect == "single-season", lap_record_driver(), lap_record_time())
    paste("Lap record: ", lap_record_driver(), " (", lap_record_time(), ")")
  })
}

