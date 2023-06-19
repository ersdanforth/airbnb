
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
  
  output$price_borough <- renderPlot(
    
    listings %>%
      group_by(neighbourhood_group_cleansed) %>% 
      summarise(avg_price = mean(price)) %>%
      mutate(neighbourhood_group_cleansed = fct_reorder(neighbourhood_group_cleansed, -avg_price)) %>% 
      ggplot(aes(fill = neighbourhood_group_cleansed, x = reorder(neighbourhood_group_cleansed, -avg_price), y = avg_price)) +
      geom_bar(position = 'dodge', stat = 'identity') + 
      scale_fill_grey(start = .25, end = .75) +
      theme(legend.position = "none", plot.title=element_text(hjust=0.5),
            axis.text.x = element_text(angle = 20)) +
      labs(title = 'NYC:\nAverage Listing Price by Borough',
           y = 'Listing Price ($)',
           x = 'Borough')
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
      scale_fill_grey(start = 0.75, end = 0.25) +
      theme(legend.position = "none", plot.title=element_text(hjust=0.5),
            axis.text.x = element_text(angle = 20)) +
      labs(title = 'Borough Selection:\nAverage Listing Price by Neighborhood',
           y = 'Listing Price ($)',
           x = 'Neighborhood')

  )
  
  output$price_bedroom <- renderPlot(

    listings_n() %>%
      filter(!is.na(bedrooms),
             bedrooms < 5) %>%
      group_by(bedrooms) %>%
      summarise(avg_price = mean(price)) %>%
      ggplot(aes(fill = factor(bedrooms), x = -bedrooms, y = avg_price)) +
      geom_bar(stat = 'identity') +
      scale_fill_grey(start = 0.75, end = 0.25) +
      labs(title = 'Average Listing Price by Size') +
      theme(legend.position = "none", plot.title=element_text(hjust=0.5),
            axis.text.x = element_text(angle = 20)) +
      labs(title = 'Neighborhood Selection:\nAverage Listing Price by Bedrooms',
           y = 'Listing Price ($)',
           x = 'Bedrooms')
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
      labs(title = 'NYC: Listings by Borough') +
      theme(plot.title=element_text(hjust=0.8), plot.margin = margin(0,.4,0,-.3, 'cm'))

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
      labs(title = 'Borough Selection: Listings by Neighborhood (Top 10)') +
      theme(plot.title=element_text(hjust=-.75), plot.margin = margin(0,.4,0,-.3, 'cm'))

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
      scale_fill_manual("Bedrooms", values = colorRampPalette(brewer.pal(8, "Set2"))(10)) +
      theme_void() + 
      labs(title = 'Neighborhood Selection: Listings by Size') +
      theme(plot.title=element_text(hjust=0.9), plot.margin = margin(0,.4,0,-.3, 'cm'))
    
  )

  output$price_reviews_borough <- renderPlot(
  
    df_sub %>% 
      ggplot(aes(x = price, y=reviews_per_month, col = neighbourhood_group_cleansed)) +
      geom_smooth(method = "loess", se = FALSE) +
      scale_color_brewer("Boroughs", palette='Set2') +
      coord_cartesian(xlim = c(0, 750), y = c(0, 2.5)) +
      labs(title = 'Demand by Listing Price Across Five Boroughs',
           y = 'Monthly Reviews',
           x = 'Price in $')
    
  )
  
  output$price_reviews_neighb <- renderPlot(
    
    listings_b() %>% 
      group_by(neighbourhood_cleansed) %>% 
      summarise(count = n()) %>% 
      top_n(5, count) %>% 
      right_join(listings, by = 'neighbourhood_cleansed') %>% 
      filter(!is.na(count)) %>% 
      ggplot(aes(x = price, y=reviews_per_month, col = neighbourhood_cleansed)) +
      geom_smooth(method = "loess", se = FALSE) +
      scale_color_brewer("Neighborhoods", palette='Set2') +
      coord_cartesian(xlim = c(0, 750), y = c(0, 2.5)) +
      labs(title = 'Borough Selection: Demand by Listing Price Across Top 5 Neighborhoods',
           y = 'Monthly Reviews',
           x = 'Price in $')
  )
  
  output$test1 <- renderPlot(
    
    df_join %>% 
      ggplot(aes(x = median_rent, y = count)) +
      geom_point() +
      geom_smooth() +
      coord_cartesian(xlim = c(2000, 5000), ylim = c(0, 2000)) + 
      labs(title = 'Median Rent and Airbnb Listing Count',
           x = 'Median Rent',
           y = 'Airbnb Listing Count')
  )
  
  output$test2 <- renderPlot(
  
    df_join %>% 
      ggplot(aes(x = median_rent, y = avg_price)) +
      geom_point() +
      geom_smooth() +
      coord_cartesian(xlim = c(2000, 5000), ylim = c(0, 500)) +
      labs(title = 'Median Rent and Airbnb Listing Price',
           x = 'Median Rent',
           y = 'Average Airbnb Listing Price')
  )

  output$test3 <- renderPlot(
    
    df_join %>% 
      ggplot(aes(x = median_rent, y = avg_reviews)) +
      geom_point() +
      geom_smooth() + 
      coord_cartesian(xlim = c(2000, 5000), ylim = c(0, 3.5)) +
      labs(title = 'Median Rent and Airbnb Listing Reviews',
           x = 'Median Rent',
           y = 'Average Listing Monthly Reviews')
  )
  
  output$test4 <- renderPlot(
    
    df_join %>% 
      ggplot(aes(x = rental_inventory, y = count)) +
      geom_point() +
      geom_smooth() +
      coord_cartesian(xlim = c(0, 1000), ylim = c(0, 2000)) + 
      labs(title = 'Rental Inventory and Airbnb Listing Count',
           x = 'Rental Inventory',
           y = 'Airbnb Listing Count')
  )
  
  output$test5 <- renderPlot(
    
    df_join %>% 
      ggplot(aes(x = rental_inventory, y = avg_price)) +
      geom_point() +
      geom_smooth() +
      coord_cartesian(xlim = c(0, 1000), ylim = c(0, 500)) +
      labs(title = 'Rental Inventory and Airbnb Listing Price',
           x = 'Rental Inventory',
           y = 'Average Airbnb Listing Price')
  )
  
  output$test6 <- renderPlot(
    
    df_join %>% 
      ggplot(aes(x = rental_inventory, y = avg_reviews)) +
      geom_point() +
      geom_smooth() + 
      coord_cartesian(xlim = c(0, 1000), ylim = c(0, 3.5)) +
      labs(title = 'Rental Inventory and Airbnb Listing Reviews',
           x = 'Rental Inventory',
           y = 'Average Listing Monthly Reviews')
  )
  
}

