
dashboardPage(
  
  dashboardHeader(title='Airbnb Q1 2023'),
  
  dashboardSidebar(
    
    sidebarUserPanel("Emmeline Danforth",
                     image = "./ED.jpeg"),
    
    sidebarMenu(
      menuItem("Start Here", tabName = "intro", icon = icon("house")),
      menuItem("Market Overview", tabName = "overview", icon = icon("map")),
      menuItem("Supply and Demand", tabName = "supply", icon = icon("chart-line")),
      menuItem("Airbnb and the Rental Market", tabName = "rentals", icon = icon("building")),
      menuItem("Projecting Revenue", tabName = "revenue", icon = icon("dollar-sign"))
    )
  
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = 'intro',
              h1("NYC Airbnb Listings: March 2023"),
              br(),
              h4('Project Introduction'),
              p('This project aims to explore the connection between Airbnb and the long-term rental market in NYC. Although NYC law
                prohibits short-term rentals through Airbnb (defined by a stay shorter than 30 days), this is not enforced. What is the
                relationship between Airbnb price and availability and rental price and availability? Based on current trends,
                how can a landlord maximize revenue?'),
              br(),
              h4("Data Sources"),
              p('This analysis is based on two data sources. Inside Airbnb provides data on current Airbnb listings to 
              enable publicly available analysis and foster transparency around Airbnb\'s impact on residential communities. Street Easy, a
                popular site to search for NYC rentals, provides monthly aggregate data on neighborhood rental market statistics. The data
                used here are March 2023 outputs from both sites.'),
              br(),
              h4("Tabs"),
              p('The tabs on this dashboard each provide different visualizations to portray the Airbnb market across NYC neighborhoods 
                and the interaction between Airbnb listings and the rental market. Click into each tab for more detail.')
              ),
  
      tabItem(tabName = 'overview',
              
              fluidRow(box(title = "Exploring the Airbnb Market in NYC", width = 12, solidHeader = TRUE, status = "primary",
                           "Use the filters to view listing quantity and price across boroughs, neighborhoods,
                           and by number of bedrooms."
              )),
              fluidRow(box(selectizeInput(inputId = "borough", label = "Borough",
                                          choices = c("All", sort(unique(listings$neighbourhood_group_cleansed))))),
                       box(selectizeInput(inputId = "neighborhood", label = "Neighborhood",
                                          choices = c("All", sort(unique(listings$neighbourhood_cleansed)))))),
              fluidRow(
                tabBox(width = 12, height = "475px",
                       tabPanel('By Borough', column(6, plotOutput('donut_borough')), column(6,plotOutput('price_borough'))),
                       tabPanel('By Neighborhood', column(6, plotOutput('donut_neighb')), column(6,plotOutput('price_neighb'))),
                       tabPanel('By Size', column(6, plotOutput('donut_bedroom')), column(6,plotOutput('price_bedroom')))
              )
              )),
      
      tabItem(tabName = 'supply',
              fluidRow(
                box(width = 3, height = 375, title = "Plotting Demand", "Here, we use a listing's monthly reviews as a 
                    proxy for its popularity or demand. While this is an imprecise metric, it is 
                    the best approximation of a listing's use that is available in the dataset. User ratings, while
                    available, have little variation across the dataset and aren't helpful for depicting
                    listing popularity.", br(), br(),
                    "This line plot shows the relationship between price and monthly reviews across each of the 
                    five boroughs. With the exception of Manhattan, each of the boroughs shows declining demand for
                    listings above $200."),
                box(width = 9, height = 375, plotOutput('price_reviews_borough', height = 360))
              ),
              fluidRow(
                box(width = 9, height = 375, plotOutput('price_reviews_neighb', height = 360)),
                box(width = 3, height = 375, title = "Demand by Neighborhood",  
                    selectizeInput(inputId = "borough_price", label = "Borough",
                                   choices = c("All", sort(unique(listings$neighbourhood_group_cleansed)))), 
                    "This plot displays monthly reviews against price for the five neighborhoods with the most 
                    listings in the selected borough.")
              )),
      
      tabItem(tabName = 'rentals',
              fluidRow(box(title = "Neighborhood-Level Airbnb Supply and Demand against Rental Market Pricing and Availability", 
                           width = 12, solidHeader = TRUE, status = "primary",
                           "These visuals illusrate the relationship between the Airbnb market and the long-term rental market in
                           each NYC neighborhood. On each tab, the y axis represents average Airbnb listing supply, price, or number of reviews 
                           for an individual neighborhood. The x axis represents either median rent or total rental inventory for that 
                           neighborhood based on StreetEasy March 2023 data."
              )),
              fluidRow(
                tabBox(
                  title = "Median Rents",
                  height = "250px",
                  tabPanel("Airbnb Count", plotOutput('test1')),
                  tabPanel("Airbnb Price", plotOutput('test2')),
                  tabPanel("Airbnb Monthly Reviews", plotOutput('test3')),
                ),
                
                tabBox(
                  title = "Rental Inventory",
                  height = "250px",
                  tabPanel("Airbnb Count", plotOutput('test4')),
                  tabPanel("Airbnb Price", plotOutput('test5')),
                  tabPanel("Airbnb Monthly Reviews", plotOutput('test6')),
                )
              )),
      
      tabItem(tabName = 'revenue',
              
              fluidRow(box(title = "Projecting Revenue", width = 12, solidHeader = TRUE, status = "primary",
                "The visual below compares revenue for short-term rentals (based on a nightly Airbnb rate) vs. long-term rentals (based on
                a year-long lease in the rental market). Using the Occupancy Rate slider, choose a projected occupancy rate for an 
                Airbnb listing to see projected monthly revenue on the graph. Points above the line represent neighborhoods that 
                bring in more revenue for Airbnbs than for long-term rentals, while points below the line are neighborhoods that bring 
                in more revenue for long-term rentals than for Airbnbs. Hover over a point to see the data.")),
              
              fluidRow(
                column(4, 
                       box(h3("Airbnb Occupancy Rate"), p("Choose a monthly occupancy rate. On the slide,
                                                          50 indicates 50% occupancy, ~15 nights/month."),
                           width = NULL, height = 265,
                           sliderInput("occupancy", label = NULL, min = 0, max = 100, value = 50)),
                       box(h3("Projected Revenue"), p("Hover to choose a neighborhood."), width = NULL, height = 265,
                              verbatimTextOutput("hover_info"))),
                column(8, 
                       box(width = NULL, height = 550, 
                           plotOutput('revenue', height = 525, 
                                      hover = hoverOpts(id = "plot_hover", delayType = "throttle"))))
              )
            )
    
    )
  )
)
