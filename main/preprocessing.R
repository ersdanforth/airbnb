

### 1. Read in libraries and data

rm(list = ls())
options(scipen = 99)
setwd("/Users/emmydanforth/Documents/NYCDSA_git/airbnb")

library(tidyverse)

listings <- read.csv("./data/listings.csv")
calendar <- read.csv("./data/calendar.csv")
rent <- read.csv("./data/streeteasy/median_rent.csv")
inventory <- read.csv("./data/streeteasy/rental_inventory.csv")


### 2. Exploratory data analysis


str(listings)
names(listings)

# select columns of interest
df <- listings %>% 
  select(id, host_id, host_since, host_response_rate, host_acceptance_rate,
         host_is_superhost, host_listings_count, host_total_listings_count, 
         host_has_profile_pic, neighbourhood_cleansed, neighbourhood_group_cleansed,
         latitude, longitude, property_type, room_type, accommodates, bathrooms_text, bedrooms,
         beds, amenities, price, minimum_nights, maximum_nights, has_availability,
         availability_30, availability_365, number_of_reviews, number_of_reviews_ltm,
         number_of_reviews_l30d, first_review, last_review, review_scores_rating, review_scores_accuracy,
         review_scores_cleanliness, review_scores_location, review_scores_value,
         reviews_per_month, calculated_host_listings_count)



# exploring and cleaning variables


df$host_response_rate = as.numeric(sub("%","",df$host_response_rate))/100
summary(df$host_response_rate) # 1st Q = .99

df$host_acceptance_rate = as.numeric(sub("%","",df$host_acceptance_rate))/100
summary(df$host_acceptance_rate) # 1st Q = .78

df$host_since = as_date(df$host_since)
summary(df$host_since) # longest since since 2008



table(df$neighbourhood_group_cleansed)
# Bk = 16237, Mh = 17658
# compare market in Brooklyn vs. Manhattan?




table(df$neighbourhood_cleansed) %>% 
  as.data.frame %>% 
  arrange(desc(Freq))
# top 5 neighbs are Bed-Stuy, Williamsburg, Midtown, Harlem, Bushwick

table(df$property_type)
table(df$room_type) # entire home most common

summary(df$accommodates) # 0-16
table(df$accommodates)  # 2 guests is most common, 44% of listings
19015/42931

summary(df$bedrooms) # 1-16 bedrooms
table(df$bedrooms) # 1 bedroom is most common
summary(df$beds) #1-42 beds
table(df$beds) # 1 bed is most common

table(df$price)
df$price<- as.numeric(gsub('[^[:alnum:] ]', '', df$price))/100
summary(df$price) # 0-99000

# exploring listings with outlier prices
# perhaps the prices are actually monthly?
df %>% 
  select(price, accommodates, property_type, minimum_nights) %>% 
  arrange(desc(price)) %>% 
  top_n(30, price)

summary(df$minimum_nights) # 1-1250
summary(df$maximum_nights) #1- 2 bil (it appears these numbers are not meaningful)

summary(df$availability_365) # 0-365, median 89
summary(df$number_of_reviews) #0-1842, median 5

df %>% 
  select(price, number_of_reviews, property_type) %>% 
  arrange(desc(number_of_reviews)) %>% 
  top_n(10, number_of_reviews)
# some listings have reviews about 1000, prices are in normal range

summary(df$review_scores_rating) # 0-5
summary(df$review_scores_accuracy)
summary(df$review_scores_cleanliness)
summary(df$review_scores_location)
summary(df$review_scores_value)

summary(df$reviews_per_month) # max 86?

# how do some listings have more than 30 reviews per month?
df %>% 
  select(price, number_of_reviews, reviews_per_month) %>% 
  arrange(desc(reviews_per_month)) %>% 
  top_n(10, reviews_per_month)


### 3. Exploratory Visualizations

# most expensive neihgborhoods
df %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(count = n(),
            avg_price = mean(price)) %>% 
  top_n(10, avg_price) %>% 
  arrange(desc(avg_price)) %>% 
  ggplot(aes(x = neighbourhood_cleansed, y = avg_price)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Top 10 Most Expensive NYC Neighborhoods',
       x = 'Neighborhood',



# price in most popular neighborhoods
df %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(count = n(),
            avg_price = mean(price)) %>% 
  top_n(5, count) %>% 
  arrange(desc(avg_price)) %>% 
  ggplot(aes(x = neighbourhood_cleansed, y = avg_price)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Price in Top 5 Neighborhoods with Most Listings',
       x = 'Neighborhood',
       y = 'Average Listing Price in $')


# price in most popular neighborhoods
df %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(count = n(),
            avg_price = mean(price)) %>% 
  top_n(5, count) %>% 
  arrange(desc(avg_price)) %>% 
  ggplot(aes(x = neighbourhood_cleansed, y = avg_price)) +
  geom_bar(stat = 'identity') +
  labs(title = 'Price in Top 5 Neighborhoods with Most Listings',
       x = 'Neighborhood',
       y = 'Average Listing Price in $')



### 4. Merging data with streeteasy by neighborhood

rent_3_23 <- rent %>% 
  select(areaName, Borough, areaType, X2023.03)
rent_3_23
summary(rent_3_23) # 49 NAs

inventory_3_23 <- inventory %>% 
  select(areaName, Borough, areaType, X2023.03)
inventory_3_23
dim(rent_3_23)
dim(inventory_3_23)
summary(inventory_3_23) # no NAs

rentals <- full_join(rent_3_23, inventory_3_23, by = c('areaName', 'Borough', 'areaType')) %>% 
  transmute(median_rent = X2023.03.x,
            rental_inventory = X2023.03.y,
            neighborhood = areaName,
            borough = Borough,
            area_type = areaType)

head(rentals)
dim(rentals)
summary(rentals)

# 177 neighborhoods
rentals_neighb <- rentals %>%
  filter(area_type == 'neighborhood')
dim(rentals_neighb)

# 223 neighborhoods
n_distinct(listings$neighbourhood_cleansed)

223 - 177
# 46 difference

airbnb_neighb <- df %>% 
  group_by(neighbourhood_cleansed) %>% 
  summarise(count = n(),
            avg_price = mean(price, na.rm = TRUE),
            avg_reviews = mean(number_of_reviews, na.rm = TRUE),
            avg_rating = mean(review_scores_rating, na.rm = TRUE),
            avg_location = mean(review_scores_location, na.rm = TRUE),
            avg_host_listings = mean(calculated_host_listings_count, na.rm = TRUE)) %>% 
  as.data.frame()
head(airbnb_neighb)
dim(airbnb_neighb) # 223 neighborhoods
          
join_test <- full_join(airbnb_neighb, rentals_neighb, 
                       by = c('neighbourhood_cleansed' = 'neighborhood'), keep = TRUE)
head(join_test)
dim(join_test)

listings_unmatched <- join_test %>% 
  filter(is.na(join_test$neighborhood))
dim(listings_unmatched) ## 88 unmatched, out of 223 (39%)
listings_unmatched$neighbourhood_cleansed

streeteasy_unmatched <- join_test %>% 
  filter(is.na(join_test$neighbourhood_cleansed))
dim(streeteasy_unmatched) # 42 unmatched, out of 177 (24%)
streeteasy_unmatched$neighborhood

join_test %>% 
  top_n(20, count) %>% 
  arrange(desc(count)) 
# of top 20 neighborhoods with Airbnb listings, 
# missing street easy match for Harlem and Hell's Kitchen

rentals_neighb %>% filter(grepl('Harlem', neighborhood)) # central, east, west harlem
airbnb_neighb %>% filter(grepl('Harlem', neighbourhood_cleansed)) # east harlem, harlem

rentals_neighb %>% filter(grepl('Midtown', neighborhood)) #midtown, east, south, west
airbnb_neighb %>% filter(grepl('Midtown', neighbourhood_cleansed)) # midtown
airbnb_neighb %>% filter(grepl('Hell', neighbourhood_cleansed)) # hell's kitchen

rentals_neighb_clean <- rentals_neighb %>% 
  mutate(neighborhood = gsub(pattern = 'Central Harlem', replacement = 'Harlem', 
                       x = neighborhood, ignore.case = F, fixed = T)) %>% 
  mutate(neighborhood = gsub(pattern = 'Midtown West', replacement = 'Hell\'s Kitchen', 
                       x = neighborhood, ignore.case = F, fixed = T))

join_test_2 <- full_join(airbnb_neighb, rentals_neighb_clean, 
                       by = c('neighbourhood_cleansed' = 'neighborhood'), keep = TRUE)
join_test_2 %>% 
  top_n(20, count) %>% 
  arrange(desc(count))

listings_unmatched <- join_test_2 %>% 
  filter(is.na(join_test_2$neighborhood))
dim(listings_unmatched) ## 86 unmatched
listings_unmatched$neighbourhood_cleansed

streeteasy_unmatched <- join_test_2 %>% 
  filter(is.na(join_test_2$neighbourhood_cleansed))
dim(streeteasy_unmatched) # 40 unmatched
streeteasy_unmatched$neighborhood






### 5. Rentals and airbnb visualizations

# median rent vs. avg airbnb price
# appears to be positive correlation
join_test_2 %>% 
  ggplot(aes(x = median_rent, y = avg_price)) +
  geom_point() +
  coord_cartesian(xlim = c(1500, 6000), ylim = c(0, 500))

# median rent vs. airbnb count
# no clear correlation
join_test_2 %>% 
  ggplot(aes(x = median_rent, y = count)) +
  geom_point() +
  coord_cartesian(xlim = c(1500, 6000), ylim = c(0, 500))

# rental inventory vs. avg airbnb price
# no correlation
join_test_2 %>% 
  ggplot(aes(x = rental_inventory, y = avg_price)) +
  geom_point() +
  coord_cartesian(xlim = c(0, 1500), ylim = c(0, 1000))

# rental inventory vs. airbnb count
# looks like positive correlation
join_test_2 %>% 
  ggplot(aes(x = rental_inventory, y = count)) +
  geom_point() + 
  coord_cartesian(xlim = c(0, 1500), ylim = c(0, 2000))

# avg airbnb price as percentage of median rent, top 10 neighbs
join_test_2 %>% 
  mutate(ratio = avg_price/median_rent) %>% 
  top_n(10, count) %>% 
  arrange(desc(count)) %>% 
  ggplot(aes(x = reorder(neighbourhood_cleansed, -ratio), y = ratio)) +
  geom_bar(stat = 'identity')
# of top 10 neighbs,
# midtown has most expensive airbnb prices compared to rental prices

# avg airbnb price as percentage of median rent
join_test_2 %>% 
  mutate(ratio = avg_price/median_rent) %>% 
  top_n(10, ratio) %>% 
  ggplot(aes(x = reorder(neighbourhood_cleansed, -ratio), y = ratio)) +
  geom_bar(stat = 'identity')
# briarwood, queens has highest airbnb to rental price ratio

# rental inventory vs. airbnb review count
# looks like positive correlation
join_test_2 %>% 
  ggplot(aes(x = rental_inventory, y = avg_reviews)) +
  geom_point() + 
  coord_cartesian(xlim = c(0, 1500), ylim = c(0, 100))

summary(join_test_2$avg_reviews) # Q1-Q3: 19-34


  
