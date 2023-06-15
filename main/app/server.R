

function(input, output) {
  
  listings_b <- reactive({ 
    listings %>%
      filter(neighbourhood_group_cleansed == input$borough) %>%
      group_by(neighbourhood_cleansed) %>% 
      summarise(count = n(),
                avg_price = mean(price),
                avg_size = mean(accommodates))
  }) 
  
  listings_n <- reactive({ 
    listings %>%
      filter(neighbourhood_cleansed == input$neighborhood) %>%
      summarise(count = n(),
                avg_price = mean(price),
                avg_size = mean(accommodates))
  }) 
  
  # output using output ID from UI
  output$count <- renderPlot(
    
    listings_b() %>%
      top_n(10, count) %>% 
      ggplot(aes(x = reorder(neighbourhood_cleansed, -count), y = count)) +
      geom_bar(stat = 'identity') +
      ggtitle("Number of Listings by Neighborhood")
  )
  
  output$price <- renderPlot(
    
    listings_b() %>%
      top_n(10, count) %>% 
      ggplot(aes(x = reorder(neighbourhood_cleansed, -avg_price), y = avg_price)) +
      geom_col(stat = 'identity') +
      ggtitle("Average Price by Neighborhood")
  )
  
  output$neighb_avg <- renderPlot(
    
    #empty plot
    listings_n() %>%
      ggplot(aes(x = 1, y = avg_size)) +
      geom_col(stat = 'identity') +
      ggtitle("Average Size")
  )
  
}
  
  
  
  
  

  
  # # output using delay ID
  # 
  # output$delay <- renderPlot(
  #   
  #   flights_delay() %>% # call function from above
  #     pivot_longer(arrival:departure, # move columns to new variable to be used for categorization
  #                  names_to = 'type',
  #                  values_to = 'delay') %>%
  #     ggplot() +
  #     geom_col(
  #       aes(x = carrier, y = delay, fill = type),
  #       position = 'dodge'
  #     ) +
  #     ggtitle("Average Delay")
  # )
  
