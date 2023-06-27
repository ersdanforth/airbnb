
#library(shiny)
library(tidyverse)
library(shinydashboard)
library(RColorBrewer)
library(plotly)
library(shinyWidgets)

# import data
listings <- read.csv("data/listings.csv")
rent <- read.csv("data/streeteasy/median_rent.csv")
inventory <- read.csv("data/streeteasy/rental_inventory.csv")

# convert price to numeric
listings$price<- as.numeric(gsub('[^[:alnum:] ]', '', listings$price))/100

# filter airbnb listings for plotting
df_sub <- listings %>% 
  filter(!is.na(bedrooms),
         reviews_per_month <= quantile(reviews_per_month, .99, na.rm = TRUE),
         price <= quantile(price, .99, na.rm = TRUE),
         minimum_nights <= quantile(minimum_nights, .99, na.rm = TRUE),
         calculated_host_listings_count <= quantile(calculated_host_listings_count, .99, na.rm = TRUE))

# clean rentals data
rentals <- full_join(rent, inventory, by = c('areaName', 'Borough', 'areaType')) %>% 
  transmute(median_rent = X2023.03.x,
            rental_inventory = X2023.03.y,
            neighborhood = areaName,
            borough = Borough,
            area_type = areaType) %>% 
  mutate(neighborhood = gsub(pattern = 'Central Harlem', replacement = 'Harlem', 
                             x = neighborhood, ignore.case = F, fixed = T)) %>% 
  mutate(neighborhood = gsub(pattern = 'Midtown West', replacement = 'Hell\'s Kitchen', 
                             x = neighborhood, ignore.case = F, fixed = T)) %>% 
  filter(area_type == 'neighborhood')

# group airbnb data by neighborhood
airbnb_neighb <- listings %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(count = n(),
            avg_price = mean(price, na.rm = TRUE),
            avg_reviews = mean(reviews_per_month, na.rm = TRUE),
            avg_rating = mean(review_scores_rating, na.rm = TRUE),
            avg_host_listings = mean(calculated_host_listings_count, na.rm = TRUE)) %>% 
  as.data.frame()

# join rentals and airbnb data
df_join <- full_join(airbnb_neighb, rentals, 
                     by = c('neighbourhood_cleansed' = 'neighborhood'), keep = TRUE)
df_join %>% mutate(avg_revenue = avg_price * 30)

