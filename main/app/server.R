
function(input, output, session) {
  
  listings_b <- reactive({ 
    listings %>%
      filter(neighbourhood_group_cleansed == input$borough)
  }) 
  
  listings_n <- reactive({ 
    listings %>%
      filter(neighbourhood_cleansed == input$neighborhood)
  }) 
  
  observe({ 
    updateSelectizeInput(session, 
                         'neighborhood', 
                         choices = sort(unique(listings_b()$neighbourhood_cleansed))) 
  }) 
  
  # column(5, plotOutput('donut_bedrooms')),
  
  output$price_borough <- renderPlot(
    
    listings %>%
      group_by(neighbourhood_group_cleansed) %>% 
      summarise(avg_price = mean(price)) %>%
      mutate(neighbourhood_group_cleansed = fct_reorder(neighbourhood_group_cleansed, -avg_price)) %>% 
      ggplot(aes(fill = neighbourhood_group_cleansed, x = reorder(neighbourhood_group_cleansed, -avg_price), y = avg_price)) +
      geom_bar(position = 'dodge', stat = 'identity') + 
      scale_fill_brewer(palette='Greys', direction = -1) +
      labs(title = 'Average Listing Price by Borough') +
      theme(legend.position = "none")
  )
  
  
  output$price_neighb <- renderPlot(
    
    listings_b() %>%
      group_by(neighbourhood_cleansed) %>% 
      summarise(count = n(),
                avg_price = mean(price)) %>%
      arrange(desc(avg_price)) %>% 
      top_n(10, count) %>%
      mutate(neighbourhood_cleansed = fct_reorder(neighbourhood_cleansed, avg_price)) %>% 
      ggplot(aes(fill = neighbourhood_cleansed, x = reorder(neighbourhood_cleansed, -avg_price), y = avg_price)) +
      geom_bar(position = 'dodge', stat = 'identity') + 
      scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Greys"))(10)) +
      labs(title = 'Average Listing Price by Neighborhood') +
      theme(legend.position = "none")

  )
  
  output$price_bedroom <- renderPlot(

    listings_n() %>%
      filter(!is.na(bedrooms),
             bedrooms < 5) %>% # I changed this - look for error here
      group_by(bedrooms) %>%
      summarise(avg_price = mean(price)) %>%
      ggplot(aes(fill = factor(bedrooms), x = -bedrooms, y = avg_price)) +
      geom_bar(stat = 'identity') +
      ggtitle("Average Price by Bedrooms") +
      scale_fill_brewer(palette='Greys') +
      labs(title = 'Average Listing Price by Size') +
      theme(legend.position = "none")
  )

  
  output$donut_borough <- renderPlot(
    
    listings %>% 
      group_by(neighbourhood_group_cleansed) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count)) %>% 
      mutate(neighbourhood_group_cleansed = fct_reorder(neighbourhood_group_cleansed, -count),
             perc = prop.table(count) * 100,
             ymax = cumsum(perc),
             ymin = ifelse(ymax == perc, 0, lag(ymax))) %>% 
      ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=neighbourhood_group_cleansed)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      scale_fill_brewer("Boroughs", palette='Set2') +
      theme_void() + 
      labs(title = 'All NYC Listings')

  )
  
  output$donut_neighb <- renderPlot(
    
    listings_b() %>% 
      group_by(neighbourhood_cleansed) %>% 
      summarise(count = n()) %>% 
      mutate(perc = prop.table(count) * 100) %>% 
      arrange(desc(perc)) %>% 
      top_n(10, perc) %>% 
      mutate(neighbourhood_cleansed = fct_reorder(neighbourhood_cleansed, -count),
             ymax = cumsum(perc),
             ymin = ifelse(ymax == perc, 0, lag(ymax))) %>% 
      ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=neighbourhood_cleansed)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      scale_fill_manual("Neighborhoods", values = colorRampPalette(brewer.pal(8, "Set2"))(10)) +
      theme_void() + 
      labs(title = 'Borough Selection: Listings by Neighborhood')

  )
  
  output$donut_bedroom <- renderPlot(
    
    listings_n() %>%
      filter(!is.na(bedrooms)) %>% 
      group_by(bedrooms) %>% 
      summarise(count = n()) %>% 
      slice(seq_len(4)) %>% 
      mutate(perc = prop.table(count) * 100) %>% 
      arrange(desc(perc)) %>% 
      mutate(bedrooms = factor(bedrooms),
             ymax = cumsum(perc),
             ymin = ifelse(ymax == perc, 0, lag(ymax))) %>% 
      ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=bedrooms)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(10)) +
      theme_void() + 
      labs(title = 'Neighborhood Selection: Listings by Size')
    
  )
  
}

