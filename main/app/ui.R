
dashboardPage(
  
  dashboardHeader(title='Airbnb Q1 2023'),
  
  dashboardSidebar(
    
    sidebarUserPanel("Emmeline Danforth",
                     image = "./nycdsa.jpg"),
    
    sidebarMenu(
      menuItem("Start Here", tabName = "intro", icon = icon("house")),
      menuItem("Market Overview", tabName = "overview", icon = icon("map")),
      menuItem("Supply and Demand", tabName = "supply", icon = icon("chart-line")),
      menuItem("Airbnb and the Rental Market", tabName = "rentals", icon = icon("building"))
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
                column(10, plotOutput('price_reviews_borough')),
                column(10, plotOutput('price_reviews_neighb'))
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
              ))
      
      #tabItem(tabName = 'data', DTOutput('table')) # formatted table
    )
  )
)
