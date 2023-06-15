
library(shiny)
library(tidyverse)
library(shinydashboard)

setwd("/Users/emmydanforth/Documents/NYCDSA_git/airbnb")

listings <- read.csv("./data/listings.csv")
#calendar <- read.csv("./data/calendar.csv")
#rent <- read.csv("./data/streeteasy/median_rent.csv")
#inventory <- read.csv("./data/streeteasy/rental_inventory.csv")

# convert variables to numeric
listings$price<- as.numeric(gsub('[^[:alnum:] ]', '', listings$price))/100