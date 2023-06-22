
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
    ),
    
    
    selectizeInput(inputId = "borough", # must define an ID for each widget
                   label = "Borough", # title seen by user
                   choices = sort(unique(listings$neighbourhood_group_cleansed))), # options in dropdown
    
    selectizeInput(inputId = "neighborhood",
                   label = "Neighborhood",
                   choices = sort(unique(listings$neighbourhood_cleansed)))
  
  ),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = 'intro',
              h1("NYC Airbnb Listings: March 2023"),
              br(),
              h4("Data Sources"),
              p('This analysis is based on two primary data sources. Inside Airbnb is a project providing data on Airbnb listings to enable 
                publicly available analysis and foster transparency around Airbnb\'s impact on residential communities. Street Easy, a
                popular site to search for NYC rentals, provides monthly aggregate data on neighborhood rental market statistics.'),
              br(),
              h4("Tabs"),
              p('The tabs on this dashboard each provide different visualizations to portray the Airbnb market across NYC neighborhoods 
                and the interaction between Airbnb listings and the rental market.'),
              br(),
              h4("Filters"),
              p('While some visuals display citywide data, others are filtered based on the dropdown menus on the left. Use
                the Borough menu to narrow in on data for one borough, then the Neighborhood menu to choose a specific neighborhood
                within that borough.')
              ),
  
      tabItem(tabName = 'overview',
              fluidRow(
                column(4, plotOutput('donut_borough')),
                column(4, plotOutput('donut_neighb')),
                column(4, plotOutput('donut_bedroom')),
                column(4, plotOutput('price_borough')),
                column(4, plotOutput('price_neighb')),
                column(4, plotOutput('price_bedroom'))
              )),
      
      tabItem(tabName = 'supply',
              fluidRow(
                box(width = 3, height = 375, title = "Plotting Demand", "Here, we use a listing's monthly reviews as a 
                    proxy for its popularity or demand. While this is an imprecise metric, it is 
                    the best approximation of a listing's use that is available in the dataset. User ratings, while
                    available, have little variation across the dataset and aren't helpful for depicting
                    listing popularity.", br(), br(),
                    "This line plot shows the relationship between price and monthly reviews across each of the 
                    five boroughs. With the excpetion of Manhattan, each of the boroughs shows declining demand for
                    listings above $200."),
                box(width = 9, height = 375, plotOutput('price_reviews_borough', height = 360))
              ),
              fluidRow(
                box(width = 9, height = 375, plotOutput('price_reviews_neighb', height = 360)),
                box(width = 3, height = 375, title = "Demand by Neighborhood", "Use the sidebar Borough filter to choose a borough. This
                    plot displays reviews against price for the five neighborhoods with the most listings in
                    that borough.")
              )),
      
      tabItem(tabName = 'rentals',
              h4("Neighborhood-Level Airbnb Supply and Demand against Rental Market Pricing and Availability"),
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
