seasonsUI <- tabPanel("Seasons",
         fluidRow(
           column(2, selectInput(
             inputId = "driverConstructor",
             label = "Select view",
             choices = c(
               "Drivers results" = "drivers-results",
               "Drivers standings" = "drivers-standings",
               "Constructors standings" = "constructors-standings"
             ),
             selected = "drivers-results"
           )),
           column(10, sliderInput("year",
                                  "Select season:",
                                  min = 1950,
                                  max = 2023,
                                  value = 2023,
                                  step = 1,
                                  width = "100%")
           )
         ),
         fluidRow(
           column(10, column(12,
                             plotlyOutput("seasonsPlot"),
                             DTOutput("seasonsTable")
           )),
           column(2, actionLink("selectall","Select/Deselect All"),
                  uiOutput("driverConstructorSelect"))
         ))


seasonsServer <- function(input, output, session, cr, dr, team_colors) {
  
  driver_results <- reactive({
    req(input$year, input$driversSelect)
    dr %>% 
      filter(year == input$year) %>% 
      {positions <<- .$position %>% unique() %>% length(); .} %>% 
      mutate(position = factor(position, levels = c(as.character(1:positions), "DNF"))) %>% 
      filter(driverName %in% input$driversSelect) %>% 
      inner_join(team_colors, by="constructorId") %>% 
      inner_join(races, by="raceId") %>% 
      arrange(raceId) %>% 
      mutate(raceName = factor(name.x, levels = unique(name.x[order(raceId)])))
  })
  
  driver_standings <- reactive({
    req(input$year, input$driversSelect)
    driver_standings <- dr %>% filter(year == input$year) %>% 
      arrange(driverId, raceId) %>%
      group_by(driverId) %>% 
      mutate(cumulative_sum=cumsum(points)) %>% 
      ungroup() %>% 
      group_by(raceId) %>% 
      arrange(desc(cumulative_sum), .by_group = TRUE) %>% 
      mutate(ranking=seq(1:n())) %>% 
      {rankings <<- .$ranking %>% unique() %>% length(); .} %>%
      mutate(ranking = factor(ranking, levels = as.character(1:rankings-1))) %>% 
      ungroup() %>% 
      filter(driverName %in% input$driversSelect) %>% 
      inner_join(team_colors, by="constructorId") %>% 
      arrange(raceId) %>% 
      mutate(raceName = factor(name.x, levels = unique(name.x[order(raceId)])))
  })
  
  constructor_standings <- reactive({
    req(input$year, input$constructorsSelect)
    cr %>% filter(year == input$year & name.y %in% input$constructorsSelect) %>%
      arrange(constructorId, raceId) %>%
      group_by(constructorId) %>%
      mutate(cumulative_sum = cumsum(points)) %>%
      ungroup() %>%
      group_by(raceId) %>%
      arrange(desc(cumulative_sum), .by_group = TRUE) %>%
      mutate(ranking = row_number()) %>% 
      ungroup() %>% 
      inner_join(team_colors, by="constructorId") %>% 
      mutate(raceName = factor(name.x, levels = unique(name.x[order(raceId)])))
  })
  
  driver_points <- reactive({
    dr %>% filter(year == input$year & driverName %in% input$driversSelect) %>% 
      group_by(driverName) %>% 
      summarise(wins = sum(position == 1), poles=sum(grid == 1), points=sum(points)) %>% 
      arrange(desc(points)) %>% 
      rename(name=driverName)
  })
  
  constructor_points <- reactive({
    cr %>% filter(year == input$year & name.y %in% input$constructorsSelect) %>% 
      group_by(name.y) %>% 
      summarise(points=sum(points)) %>% 
      arrange(desc(points)) %>% 
      rename(name=name.y)
  })
  
  plotDriversResults  <- function() {
    data <- driver_results()
    
    last_points <- data %>%
      filter(raceId == max(raceId)) %>% 
      select(raceId, raceName, driverName, position)
    
    data <- data %>% split(.$driverId)
    
    p <- plot_ly()
    
    for(group in data) {
      p <- p %>%
        add_trace(data = group,
                  x = ~raceName,
                  y = ~position,
                  text = ~driverName,
                  name = ~driverName,
                  type = 'scatter',
                  mode = 'lines+markers',
                  line = list(color = group$color[1], width = 3),
                  marker = list(color = group$color[1], size = 8))
    }
    
    p <- p %>% 
      layout(
        xaxis = list(title = "Race"),
        yaxis = list(autorange = "reversed", title = "Position finished"),
        showlegend = FALSE
      ) %>%
      add_annotations(
        x = last_points$raceName,
        y = last_points$position,
        text = last_points$driverName,
        xanchor = 'left',
        showarrow = FALSE,
        font = list(size = 12)
      )
    
    p
  }
  
  plotDriversStandings <- function() {
    data <- driver_standings()
    
    last_points <- data %>% 
      filter(raceId == max(raceId))
    
    data <- data %>% split(.$driverId)
    
    p <- plot_ly()
    
    for(group in data) {
      p <- p %>%
        add_trace(data = group,
                  x = ~raceName,
                  y = ~ranking,
                  text = ~driverName,
                  type = 'scatter',
                  mode = 'lines+markers',
                  line = list(color = group$color[1], width = 3),
                  marker = list(color = group$color, size = 8))
    }
    
    p <- p %>% 
      layout(
        xaxis = list(title = "Race"),
        yaxis = list(autorange = "reversed", title = "Championship standing"),
        showlegend = FALSE
      ) %>% 
      add_annotations(
        x = last_points$raceName,
        y = last_points$ranking,
        text = last_points$driverName,
        xanchor = 'left',
        showarrow = FALSE,
        font = list(size = 12)
      )
    
    p
  }
  
  plotConstructorsStandings <- function() {
    data <- constructor_standings()
    
    last_points <- data %>% 
      filter(raceId == max(raceId))
    
    data <- data %>% split(.$constructorId)

    p <- plot_ly()

    for(group in data) {
      p <- p %>%
        add_trace(data = group,
                  x = ~raceName,
                  y = ~ranking,
                  text = ~name.y,
                  type = 'scatter',
                  mode = 'lines+markers',
                  hoverinfo = 'text',
                  line = list(color = group$color[1], width = 3),
                  marker = list(color = group$color, size = 8))
    }

    p <- p %>%
      layout(
            xaxis = list(title = "Race"),
            yaxis = list(autorange = "reversed", title = "Championship standing"),
            showlegend = FALSE
          ) %>%
          add_annotations(
            x = last_points$raceName,
            y = last_points$ranking,
            text = last_points$name.y,
            xanchor = 'left',
            showarrow = FALSE,
            font = list(size = 12)
          )

    p
  }
  
  observe({
    if(input$selectall == 0) return(NULL) 
    else if (input$selectall%%2 == 0)
    {
      updateCheckboxGroupInput(session, "driversSelect", 
                               choices = dr %>% filter(year == input$year) %>% distinct(driverName) %>% pull(driverName))
      updateCheckboxGroupInput(session, "constructorsSelect", "Select Constructors", 
                               choices = cr %>% filter(year == input$year) %>% distinct(name.y) %>% pull(name.y))
    }
    else
    {
      updateCheckboxGroupInput(session, "driversSelect",
                               choices = dr %>% filter(year == input$year) %>% distinct(driverName) %>% pull(driverName),
                               selected = dr %>% filter(year == input$year) %>% distinct(driverName) %>% pull(driverName))
      updateCheckboxGroupInput(session, "constructorsSelect",
                               choices = cr %>% filter(year == input$year) %>% distinct(name.y) %>% pull(name.y),
                               selected = cr %>% filter(year == input$year) %>% distinct(name.y) %>% pull(name.y))
      
    }
  })
  
  observeEvent(input$driverConstructor, {
    
    output$driverConstructorSelect <- renderUI({
      switch(isolate(input$driverConstructor),
             "drivers-results" = checkboxGroupInput("driversSelect",
                                            "Select drivers",
                                            choices = dr %>% filter(year == input$year) %>% distinct(driverName) %>% pull(driverName),
                                            selected = dr %>% filter(year == input$year) %>% distinct(driverName) %>% pull(driverName)
             ),
             "drivers-standings" = checkboxGroupInput("driversSelect",
                                                      "Select drivers",
                                                      choices = dr %>% filter(year == input$year) %>% distinct(driverName) %>% pull(driverName),
                                                      selected = dr %>% filter(year == input$year) %>% distinct(driverName) %>% pull(driverName)
             ),
             "constructors-standings" = checkboxGroupInput("constructorsSelect",
                                                 "Select constructors",
                                                 choices = cr %>% filter(year == input$year) %>% distinct(name.y) %>% pull(name.y),
                                                 selected = cr %>% filter(year == input$year) %>% distinct(name.y) %>% pull(name.y)
             )
      )
    })
    
    output$seasonsPlot <- renderPlotly({
      switch(isolate(input$driverConstructor),
             "drivers-results" = plotDriversResults(),
             "drivers-standings" = plotDriversStandings(),
             "constructors-standings" = plotConstructorsStandings()
      )
    })
    
    output$seasonsTable <- DT::renderDataTable({
      switch(isolate(input$driverConstructor),
             "drivers-results" = driver_points(),
             "drivers-standings" = driver_points(),
             "constructors-standings" = constructor_points()
      )
    })
    
  })
}