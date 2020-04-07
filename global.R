# Libraries Used:
library(leaflet)
library(dplyr)
library(shiny)
library(ggplot2)
library(stringr)
library(plotly)
library(shinythemes)
library(png)
library(jpeg)

# Data Load
town_data <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/RI_Town_Data_All.csv", stringsAsFactors = FALSE)
age_data <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/Age_Distribution_Long.csv", stringsAsFactors = FALSE)
gender_data <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/Gender.csv", stringsAsFactors = FALSE)
hospital_data <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/Hospital%20Long.csv", stringsAsFactors = FALSE)
new_cases_long <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/New_Cases_Long.csv", stringsAsFactors = FALSE)
ri_confirmed_cases_wide <- read.csv("https://raw.githubusercontent.com/jjenki22/Rhode-Island-COVID-19/master/Data/New_Cases_Wide.csv", stringsAsFactors = FALSE)

# Editing Town Data to be consistent
town_data$Town <- str_squish(town_data$Town)
town_data$Date <- str_squish(town_data$Date)
town_data$Cases <- str_squish(town_data$Cases)

# Editing Dates
town_data$Date <- as.Date(town_data$Date, format="%m/%d/%y")
age_data$Date <- as.Date(age_data$Date, format= "%m/%d/%y")
gender_data$Date <- as.Date(gender_data$Date, format= "%m/%d/%y")
hospital_data$Date <- as.Date(hospital_data$Date, "%m/%d/%y")
new_cases_long$Date <- as.Date(new_cases_long$Date, "%m/%d/%y")
ri_confirmed_cases_wide$Date <- as.Date(ri_confirmed_cases_wide$Date, "%m/%d/%y")

# Sort town Data
town_data_descending <- town_data[rev(order(as.Date(town_data$Date))),]
