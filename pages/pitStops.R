pitStopsUI <- tabPanel("Pit stop times",
                       sliderInput("pitStopYear",
                                   "Select season:",
                                   min = 2011,
                                   max = 2023,
                                   value = 2023,
                                   step = 1,
                                   width = "100%"),
                       plotlyOutput("pitStopsPlot"),
                       dataTableOutput("dto"))

pitStopsServer <- function(input, output, pit_stops, races, results, constructors, team_colors) {
  df <- reactive({
    Q <- quantile(pit_stops$milliseconds, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(pit_stops$milliseconds)
    upper <- Q[2]+1.5*iqr
    lower <- Q[1]-1.5*iqr
    
    pit_stops %>% 
      filter(milliseconds < upper & milliseconds > lower) %>% 
      inner_join(races, by = "raceId") %>% 
      inner_join(results, by=c("raceId", "driverId")) %>% 
      inner_join(constructors, by="constructorId") %>% 
      inner_join(team_colors, by="constructorId") %>% 
      filter(year == input$pitStopYear) %>% 
      mutate(pitTime = strftime(as.POSIXct(Sys.Date()) + milliseconds.x / 1000, "%M:%OS3"))
    })
  
  plotPitStopTimes <- function() {
    
    median_times <- df() %>%
      group_by(name.y) %>%
      summarise(median = median(milliseconds.x)) %>%
      arrange(median)

    data <- df() %>%
      mutate(name.y = factor(name.y, levels = median_times$name.y)) %>%
      split(.$name.y)

    p <- plot_ly()
    for (group in data) {
      p <- p %>%
        add_trace(data = group,
                  y = ~name.y,
                  x = ~pitTime,
                  split = ~name.y,
                  type = 'box',
                  orientation = 'h',
                  fillcolor = alpha(group$color[1], 0.50),
                  line = list(color = group$color[1]))
    }
    p %>%
      layout(yaxis = list(title = "Constructor / Team", autorange="reversed"),
             xaxis = list(title = "Time spent in pits"))
  }
    
  observeEvent(input$pitStopYear, {
    output$pitStopsPlot <- renderPlotly({
     plotPitStopTimes()
    })
  })
  
}