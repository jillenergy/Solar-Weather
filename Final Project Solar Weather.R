##Final Project: SOLAR & WEATHER
## Emilie Bolduc & Jill Anderson
## DATA 607

##Project description

##LOAD LIBRARIES
suppressMessages(suppressWarnings(library(RCurl)))
suppressMessages(suppressWarnings(library(tidyr)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(ggplot2)))
suppressMessages(suppressWarnings(library(DT)))
suppressMessages(suppressWarnings(library(knitr)))
suppressMessages(suppressWarnings(library(stringr)))

##DATA COLLECTION
##DATA: Sunshine minutes in each hour
sunshine_url <- getURL("https://raw.githubusercontent.com/jillenergy/Solar-Weather/master/SunshineMinAug2017.csv")
sunshine_raw <- read.csv(text = sunshine_url)
head(sunshine_raw)

##DATA: Cloud cover minutes in each hour
cloud_url <- getURL("https://raw.githubusercontent.com/jillenergy/Solar-Weather/master/CloudCoverMinAug2017.csv")
cloud_raw <- read.csv(text = cloud_url)
head(cloud_raw)

##DATA: Solar production
solar_url <- getURL("TBD")
solar_raw <- read.csv(text = solar_url)
head(solar_raw)

##DATA TIDYING
##Separate the date and hour start times into two columns in order to be able to match solar production and weather
##Create dataframe with the columns: Date / HourStart / HourEnd / CloudCover



##Aggregate solar production into hourly to match the weather data
solar_raw




