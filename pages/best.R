bestUI <- tabPanel("Best",
                   selectInput(
                     inputId = "bestBy",
                     label = "View best drivers by",
                     choices = c(
                       "Wins" = "wins",
                       "Pole positions" = "poles",
                       "Race starts" = "starts"
                     ),
                     selected = "wins"),
                   
                   plotlyOutput("bestPlot"))

bestServer <- function(input, output, session, dr, constructors, qualifying, drivers, team_colors) {
  
  bestByWins = reactive({
    total_wins <- dr %>%
      group_by(driverName) %>%
      summarise(total_wins = sum(position == 1), .groups = "drop") %>% 
      arrange(desc(total_wins)) %>% 
      head(20)
    
    dr %>% 
      inner_join(constructors, by="constructorId") %>%
      mutate(constructor = name.y) %>%
      group_by(constructor, driverName) %>%
      summarise(wins = sum(position == 1), .groups="drop") %>%
      inner_join(total_wins, by="driverName") %>% 
      inner_join(team_colors, by=c("constructor"="name"))
  })
    
  
  bestByPoles = reactive({
    total_poles <- results %>%
      group_by(driverId) %>%
      summarise(total_poles = sum(grid == 1), .groups = "drop") %>% 
      arrange(desc(total_poles)) %>% 
      head(20)
    
    results %>% 
      inner_join(constructors, by="constructorId") %>%
      mutate(constructor = name) %>%
      group_by(constructor, driverId) %>%
      summarise(poles = sum(grid == 1), .groups="drop") %>%
      inner_join(total_poles, by="driverId") %>% 
      inner_join(drivers, by="driverId") %>% 
      inner_join(team_colors, by=c("constructor"="name"))
  })
  
  bestByStarts = reactive({
    total_starts <- dr %>%
      group_by(driverName) %>%
      summarise(total_starts = n(), .groups = "drop") %>% 
      arrange(desc(total_starts)) %>% 
      head(20)
    
    dr %>% 
      inner_join(constructors, by="constructorId") %>%
      mutate(constructor = name.y) %>%
      group_by(constructor, driverName) %>%
      summarise(starts = n(), .groups="drop") %>%
      inner_join(total_starts, by="driverName") %>% 
      inner_join(team_colors, by=c("constructor"="name"))
  })
  
  plotBestByWins <- function() {
    data <- bestByWins() %>%
      mutate(driverName = reorder(.$driverName, .$total_wins)) %>%
      split(.$constructor)

    p <- plot_ly()

    for (group in data) {
      p <- p %>%
        add_bars(data = group,
                 x = ~wins,
                 y = ~driverName,
                 name = ~constructor,
                 marker = list(color = group$color[1])) %>%
        layout(barmode = "stack")
    }

    p
    
  }
  
  plotBestByPoles <- function() {
    data <- bestByPoles() %>%
      mutate(driverName = reorder(.$driverName, .$total_poles)) %>%
      split(.$constructor)
    
    p <- plot_ly()
    
    for (group in data) {
      p <- p %>%
        add_bars(data = group,
                 x = ~poles,
                 y = ~driverName,
                 name = ~constructor,
                 marker = list(color = group$color[1])) %>%
        layout(barmode = "stack")
    }
    
    p
  }
  
  plotBestByStarts <- function() {
    data <- bestByStarts() %>%
      mutate(driverName = reorder(.$driverName, .$total_starts)) %>%
      split(.$constructor)
    
    p <- plot_ly()
    
    for (group in data) {
      p <- p %>%
        add_bars(data = group,
                 x = ~starts,
                 y = ~driverName,
                 name = ~constructor,
                 marker = list(color = group$color[1])) %>%
        layout(barmode = "stack")
    }
    
    p
  }
  
  observeEvent(input$bestBy, {
    output$bestPlot <- renderPlotly({
      switch(isolate(input$bestBy),
             "wins" = plotBestByWins(),
             "poles" = plotBestByPoles(),
             "starts" =  plotBestByStarts()
      )
    })
  })
}