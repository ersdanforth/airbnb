
dashboardPage(
  
  dashboardHeader(title='Airbnb Q1 2023'),
  
  dashboardSidebar(
    
    sidebarUserPanel("Emmy Danforth",
                     image = "./nycdsa.jpg"),
    
    sidebarMenu(
      menuItem("Market Overview", tabName = "overview", icon = icon("map")),
      menuItem("Supply and Demand", tabName = "supply", icon = icon("chart-line"))
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
      tabItem(tabName = 'overview',
              fluidRow(
                column(6, plotOutput('donut_borough')),
                column(6, plotOutput('price_borough')),
                column(6, plotOutput('donut_neighb')),
                column(6, plotOutput('price_neighb')),
                column(6, plotOutput('donut_bedroom')),
                column(6, plotOutput('price_bedroom'))
              )),
      
      tabItem(tabName = 'supply',
              fluidRow(
                
              ))
      
      #tabItem(tabName = 'data', DTOutput('table')) # formatted table
    )
  )
)
