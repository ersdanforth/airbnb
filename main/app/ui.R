
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
              fluidRow(                         # use fluid row to put elements side by side
                column(5, plotOutput("count")), # define output ID 
                column(7, plotOutput("price")),
                column(6, plotOutput('neighb_avg'))
              )),
      
      tabItem(tabName = 'supply',
              fluidRow(
                
              ))
      
      #tabItem(tabName = 'data', DTOutput('table')) # formatted table
    )
  )
)
