driverFinishesUI <- tabPanel("Driver Finishes",
                             selectInput(inputId="driverSelect",
                                         label="Select driver",
                                         choices=NULL),
                             textOutput("totalRacesText"),
                             plotOutput("finishesPlot"),
                             dataTableOutput("finishesTable")
                             )

driverFinishesServer <- function(input, output, session, dr) {
  updateSelectInput(session, "driverSelect", choices = drivers %>% pull(driverName))
  
  df <- reactive({
    dr %>% 
      filter(driverName == input$driverSelect) %>% 
      summarise(P1 = sum(position == "1"),
                P2 = sum(position == "2"),
                P3 = sum(position == "3"),
                "no points" = sum(points == 0 & position != "DNF"),
                points = sum(points > 0 & !(position %in% c("1", "2", "3"))),
                DNF = sum(position == "DNF")) %>% 
      pivot_longer(c(1:ncol(.)), names_to = "finished", values_to = "count") %>% 
      mutate(finished = factor(finished, levels=c("P1", "P2", "P3", "points", "no points", "DNF")))
  })
  
  observeEvent("driverSelect",
               output$finishesPlot <- renderPlot({
                 ggplot(df(), aes(fill=finished, values=count)) +
                   geom_waffle(colour = "white") +
                   coord_equal() +
                   theme_void()
               })
  )
  
  observeEvent("driverSelect",
               output$finishesTable <- renderDataTable({
                 df()
               }))
  
  observeEvent("driverSelect", 
               output$totalRacesText <- renderText({
                 paste("Total races: ", sum(df()$count))
               }))
}




