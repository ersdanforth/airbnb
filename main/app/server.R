
function(input, output, session) {
  
  listings_b <- reactive({
    if(input$borough == "All") return(df_sub)
    else return (df_sub %>% filter(neighbourhood_group_cleansed == input$borough))
  }) 
  
  listings_price <- reactive({
    if(input$borough_price == "All") return(df_sub)
    else return (df_sub %>% filter(neighbourhood_group_cleansed == input$borough_price))
  }) 

  listings_size <- reactive({
    if(input$neighborhood == 'All' & input$borough == 'All') return(df_sub)
    if (input$neighborhood == 'All') return(df_sub %>% filter(neighbourhood_group_cleansed == input$borough))
    else return(df_sub %>% filter(neighbourhood_cleansed == input$neighborhood))
  })
  
  observe({ 
    updateSelectizeInput(session, 
                         'neighborhood', 
                         choices = c("All", sort(unique(listings_b()$neighbourhood_cleansed))))
  }) 
  
  output$price_borough <- renderPlot(
    
    df_sub %>%
      group_by(neighbourhood_group_cleansed) %>%
      summarise(count = n()) %>%
      top_n(10, count) %>% 
      inner_join(df_sub, by = 'neighbourhood_group_cleansed') %>% 
      mutate(neighbourhood_group_cleansed = fct_reorder(neighbourhood_group_cleansed, count)) %>% 
      ggplot(aes(fill = neighbourhood_group_cleansed, x = reorder(neighbourhood_group_cleansed, -count), y = price)) +
      geom_boxplot() + 
      coord_cartesian(ylim = c(0, 500)) +
      scale_fill_brewer("Boroughs", palette='Set2', direction = -1) +
      theme(legend.position = "none", plot.title=element_text(hjust=0.5),
            axis.text.x = element_text(angle = 20)) +
      labs(title = 'Listing Price by Borough',
           y = 'Listing Price ($)',
           x = 'Borough') 
  )
  
  
  output$price_neighb <- renderPlot(
    
    listings_b() %>%
      group_by(neighbourhood_cleansed) %>% 
      summarise(count = n()) %>%
      arrange(desc(count)) %>% 
      slice(1:10) %>% 
      inner_join(df_sub, by = 'neighbourhood_cleansed') %>% 
      mutate(neighbourhood_cleansed = fct_reorder(neighbourhood_cleansed, count)) %>% 
      ggplot(aes(fill = neighbourhood_cleansed, x = reorder(neighbourhood_cleansed, -count), y = price)) +
      geom_boxplot() + 
      coord_cartesian(ylim = c(0, 500)) +
      scale_fill_manual(values = colorRampPalette(rev(brewer.pal(8, "Set2")))(10)) +
      theme(legend.position = "none", plot.title=element_text(hjust=0.5),
            axis.text.x = element_text(angle = 20)) +
      labs(title = 'Listing Price by Neighborhood',
           y = 'Listing Price ($)',
           x = 'Neighborhood')

  )
  
  output$price_bedroom <- renderPlot(

    listings_size() %>%
      filter(!is.na(bedrooms),
             bedrooms < 4) %>%
      group_by(bedrooms) %>%
      ggplot(aes(fill = factor(bedrooms), x = bedrooms, y = price)) +
      geom_boxplot() +
      coord_cartesian(ylim = c(0, 500)) +
      scale_fill_manual(values = colorRampPalette(brewer.pal(8, "Set2"))(10)) +
      labs(title = 'Average Listing Price by Size') +
      theme(legend.position = "none", plot.title=element_text(hjust=0.5),
            axis.text.x = element_text(angle = 20)) +
      labs(title = 'Listing Price by Size',
           y = 'Listing Price ($)',
           x = 'Bedrooms')
  )

  
  output$donut_borough <- renderPlot(
    
    df_sub %>%
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
      labs(title = 'Listings by Borough') +
      theme(plot.title=element_text(hjust=0.5))

  )
  
  output$donut_neighb <- renderPlot(
    
    listings_b() %>% 
      group_by(neighbourhood_cleansed) %>% 
      summarise(count = n()) %>% 
      mutate(perc = prop.table(count) * 100) %>% 
      arrange(desc(perc)) %>% 
      slice(1:10) %>% 
      mutate(neighbourhood_cleansed = fct_reorder(neighbourhood_cleansed, -count),
             ymax = cumsum(perc),
             ymin = ifelse(ymax == perc, 0, lag(ymax))) %>% 
      ggplot(aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=neighbourhood_cleansed)) +
      geom_rect() +
      coord_polar(theta="y") +
      xlim(c(2, 4)) +
      scale_fill_manual("Neighborhoods", values = colorRampPalette(brewer.pal(8, "Set2"))(10)) +
      theme_void() + 
      labs(title = 'Listings by Neighborhood (Top 10)') +
      theme(plot.title=element_text(hjust=0.5))

  )
  
  output$donut_bedroom <- renderPlot(
    
    listings_size() %>%
      filter(!is.na(bedrooms)) %>% 
      group_by(bedrooms) %>% 
      summarise(count = n()) %>% 
      slice(seq_len(3)) %>% 
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
      labs(title = 'Listings by Size') +
      theme(plot.title=element_text(hjust=0.5))
    
  )

  output$price_reviews_borough <- renderPlot(
  
    df_sub %>% 
      ggplot(aes(x = price, y=reviews_per_month, col = neighbourhood_group_cleansed)) +
      geom_smooth(method = "loess", se = FALSE) +
      scale_color_brewer("Boroughs", palette='Set2') +
      coord_cartesian(xlim = c(0, 500), y = c(0, 2.5)) +
      labs(title = 'Demand by Listing Price Across Five Boroughs',
           y = 'Monthly Reviews',
           x = 'Price in $')
  
  )
  
  output$price_reviews_neighb <- renderPlot(
    
    listings_price() %>% 
      group_by(neighbourhood_cleansed) %>% 
      summarise(count = n()) %>% 
      top_n(5, count) %>% 
      right_join(df_sub, by = 'neighbourhood_cleansed') %>% 
      filter(!is.na(count)) %>% 
      ggplot(aes(x = price, y=reviews_per_month, col = neighbourhood_cleansed)) +
      geom_smooth(method = "loess", se = FALSE) +
      scale_color_brewer("Neighborhoods", palette='Set2') +
      coord_cartesian(xlim = c(0, 500), y = c(0, 2.5)) +
      labs(title = 'Demand by Listing Price Across Top 5 Neighborhoods',
           y = 'Monthly Reviews',
           x = 'Price in $')
  )
  
  output$rent_count <- renderPlot(
    
    df_join %>% 
      ggplot(aes(x = median_rent, y = count)) +
      geom_point() +
      geom_smooth() +
      coord_cartesian(xlim = c(2000, 5000), ylim = c(0, 2000)) + 
      labs(title = 'Median Rent and Airbnb Listing Count',
           x = 'Median Rent',
           y = 'Airbnb Listing Count')
  )
  
  output$rent_price <- renderPlot(
  
    df_join %>% 
      ggplot(aes(x = median_rent, y = avg_price)) +
      geom_point() +
      geom_smooth() +
      coord_cartesian(xlim = c(2000, 5000), ylim = c(0, 500)) +
      labs(title = 'Median Rent and Airbnb Listing Price',
           x = 'Median Rent',
           y = 'Average Airbnb Listing Price')
  )

  output$rent_reviews <- renderPlot(
    
    df_join %>% 
      ggplot(aes(x = median_rent, y = avg_reviews)) +
      geom_point() +
      geom_smooth() + 
      coord_cartesian(xlim = c(2000, 5000), ylim = c(0, 3.5)) +
      labs(title = 'Median Rent and Airbnb Listing Reviews',
           x = 'Median Rent',
           y = 'Average Listing Monthly Reviews')
  )
  
  output$inv_count <- renderPlot(
    
    df_join %>% 
      ggplot(aes(x = rental_inventory, y = count)) +
      geom_point() +
      geom_smooth() +
      coord_cartesian(xlim = c(0, 1000), ylim = c(0, 2000)) + 
      labs(title = 'Rental Inventory and Airbnb Listing Count',
           x = 'Rental Inventory',
           y = 'Airbnb Listing Count')
  )
  
  output$inv_price <- renderPlot(
    
    df_join %>% 
      ggplot(aes(x = rental_inventory, y = avg_price)) +
      geom_point() +
      geom_smooth() +
      coord_cartesian(xlim = c(0, 1000), ylim = c(0, 500)) +
      labs(title = 'Rental Inventory and Airbnb Listing Price',
           x = 'Rental Inventory',
           y = 'Average Airbnb Listing Price')
  )
  
  output$inv_reviews <- renderPlot(
    
    df_join %>% 
      ggplot(aes(x = rental_inventory, y = avg_reviews)) +
      geom_point() +
      geom_smooth() + 
      coord_cartesian(xlim = c(0, 1000), ylim = c(0, 3.5)) +
      labs(title = 'Rental Inventory and Airbnb Listing Reviews',
           x = 'Rental Inventory',
           y = 'Average Listing Monthly Reviews')
  )
  
  output$revenue <- renderPlot(

    df_join %>%
      filter(borough %in% input$borough_check) %>% 
      mutate(avg_revenue = avg_revenue*(input$occupancy)/100) %>%
      ggplot(aes(x = median_rent, y = avg_revenue)) +
      geom_point(aes(col = borough)) +
      scale_color_brewer('Boroughs', palette='Set2') +
      geom_abline(slope = 1) +
      theme(legend.position="bottom", legend.title = element_blank(),
            plot.margin = margin(.5, .5, -.25, .5, "cm")) +
      coord_cartesian(xlim = c(1000, 7000), ylim = c(1000, 7000)) +
      labs(title = 'Neighborhood-Level Projected Revenue from Airbnb Listing or Long-Term Rental',
           y = 'Airbnb Monthly Revenue ($)',
           x = 'Long-Term Rental Monthly Revenue ($)')
  )
  
  output$hover_info <- renderPrint({
    x <- nearPoints(df_join %>% 
                 mutate(avg_revenue = round(avg_revenue*(input$occupancy)/100, 2)) %>% 
                 dplyr::select(neighborhood, avg_revenue, median_rent),
               input$plot_hover, threshold = 5, maxpoints = 1)[1,1:3]
    neighb <- x[,1]
    airbnb <- x[,2]
    rental <- x[,3]
    cat("Neighborhood: ", neighb, "\nPredicted Airbnb Revenue: ", airbnb, "\nMedian Rental Revenue: ", rental, sep = "")
  })
    
  
}

