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

##DATA: Solar production (kWh) in each minute from 2 solar panels located at NYPA's headquarters in WPO
solar_raw <- read.csv("/Users/emiliembolduc/CUNY Data 607/Final Project/Data/WPOsolarPHA+PHB_08.01-31.2017.csv", header = TRUE, stringsAsFactors = FALSE)
head(solar_raw)

###DATA TIDYING
##SOLAR DATA
##Separate the date and minute intervals into two columns 
##Create dataframe with the columns: Panel Name / Date / Time / kWh
solar_df <- as.data.frame(solar_raw, stringsAsFactors = FALSE)
solar_df$Date <- sapply(strsplit(as.character(solar_df$Timestamp), " "), "[", 1)
solar_df$Minutes <- sapply(strsplit(as.character(solar_df$Timestamp), " "), "[", 2)
new_solar <- data.frame(solar_df$Device, solar_df$Date, solar_df$Minutes, solar_df$Value, stringsAsFactors = FALSE)
colnames(new_solar) <- c("PanelName", "Date", "Minutes", "kWh")
head(new_solar)

#Make variables of the PanelNames from observations with tidyr ‘spread’ function, in order to then take the mean of both.
new_solar <- spread(new_solar, "PanelName", "kWh")
dplyr::tbl_df(new_solar)
colnames(new_solar) <- c("Date", "Minutes", "PHA_kWh", "PHB_kWh")
head(new_solar)
sapply(new_solar, class)

##Take the mean of the kWh produced by the two solar panels
#First need to change data in columns "PHA_kWh" and "PHB_kWh" from character class to numeric.
cols.num <- c("PHA_kWh","PHB_kWh")
new_solar[cols.num] <- sapply(new_solar[cols.num],as.numeric)
sapply(new_solar, class)

#Second, format out of scientific notation and round to 5 digits after decimal
new_solar <- new_solar %>% mutate_if(is.numeric, funs(round(., 5)))
####Reference: https://stackoverflow.com/questions/27613310/rounding-selected-columns-of-data-table-in-r

#Third, take the sum of kWh produced by the two solar panels (accounting for the NAs in the data using na.rm=TRUE)
new_solar$SolarSum <- rowSums(new_solar[,3:4], na.rm = TRUE)
tail(new_solar)

##Fourth, conver the "Date" to ISO 8601 standard
new_solar$Date <- format(as.Date(new_solar$Date, format = "%m/%d/%y"))
tail(new_solar)

##Separate the date and hour start times into two columns in order to be able to match solar production and weather
##Create dataframe with the columns: Date / HourBegin / PercentCloudCover
sunshine_df <- as.data.frame(sunshine_raw)
sunshine_df$Date <- sapply(strsplit(as.character(sunshine_df$IntervalStartDt), " "), "[", 1)
sunshine_df$HourBegin <- sapply(strsplit(as.character(sunshine_df$IntervalStartDt), " "), "[", 2)
new_sunshine <- data.frame(sunshine_df$Date,sunshine_df$HourBegin,sunshine_df$Sunshine)
colnames(new_sunshine) <- c("Date", "HourBegin", "SunshineMinutes")
new_sunshine$Date <- format(as.Date(new_sunshine$Date, format = "%m/%d/%Y"))
head(new_sunshine)

cloud_df <- as.data.frame(cloud_raw)
cloud_df$Date <- sapply(strsplit(as.character(cloud_df$IntervalStartDt), " "), "[", 1)
cloud_df$HourBegin <- sapply(strsplit(as.character(cloud_df$IntervalStartDt), "  "), "[", 2)
new_cloud <- data.frame(cloud_df$Date,cloud_df$HourBegin,cloud_df$CloudCover)
colnames(new_cloud) <- c("Date", "HourBegin", "PercentCloudCover")
new_cloud$Date <- format(as.Date(new_cloud$Date, format = "%m/%d/%Y"))
head(new_cloud)
new_cloud

##DATA ANALYSIS
##Aggregate the Solar, Sunshine and Cloud Cover data set into one point for each day in the month

onedate <- new_cloud[c(TRUE,rep(FALSE,23)), ]
head(onedate)

SunshineDaily <- round(colMeans(matrix(new_sunshine$SunshineMinutes, nrow=24)), digits=0)
SunshineDaily_df <- data.frame(onedate$Date,SunshineDaily)
colnames(SunshineDaily_df) <- c("Date","SunshineMinutes")
SunshineDaily_df 

CloudCoverDaily <- round(colMeans(matrix(new_cloud$PercentCloudCover, nrow=24)), digits=0)
CloudCoverDaily_df <- data.frame(onedate$Date,CloudCoverDaily)
colnames(CloudCoverDaily_df) <- c("Date","PercentCloudCover")
CloudCoverDaily_df 

SolarDaily_df <- aggregate(new_solar$SolarSum, list(Day = new_solar$Date), sum, na.rm = TRUE)
colnames(SolarDaily_df) <- c("Date", "kWh Produced")
SolarDaily_df


##VISUALIZE
##Visualize the Percentage of Cloud Cover Data
ggplot(data=CloudCoverDaily_df, aes(x=Date, y=PercentCloudCover, group=1)) +
  geom_point(color="#0066ff", size=2) +
  scale_x_discrete(breaks=c("2013","2014","2015","2016","2017")) +
  ggtitle("Percent Cloud Cover by Hour in White Plains, NY for August 2017") +
  labs(x="Date in August 2017", y="Average Percent of Cloud Cover per Day") +
  theme(axis.title.y = element_text(size=12, color="#666666")) +
  theme(axis.text = element_text(size=12, family="Trebuchet MS")) +
  theme(plot.title = element_text(size=12, family="Trebuchet MS", face="bold", hjust=0, color="#666666"))

##Visualize the Minutes of Sunshine in each hour
ggplot(data=SunshineDaily_df, aes(x=Date, y=SunshineMinutes, group=1)) +
  geom_point(color="#0066ff", size=2) +
  scale_x_discrete(breaks=c("2013","2014","2015","2016","2017")) +
  ggtitle("Average Minutes of Sunshine by Hour in White Plains, NY for August 2017") +
  labs(x="Date in August 2017", y="Average Minutes of Sunshine per Day") +
  theme(axis.title.y = element_text(size=12, color="#666666")) +
  theme(axis.text = element_text(size=12, family="Trebuchet MS")) +
  theme(plot.title = element_text(size=12, family="Trebuchet MS", face="bold", hjust=0, color="#666666"))

### TO DO
## JILL - pretty ggplots with cloud cover and sunshine (decide if you want to conver to % rather than minutes) combined and then one with all three combined.
## EMILIE - Time permitting - turn Solar data in daily (GOT STUCK ON THIS Solar data is in 1-minute intervals. Need to collapse the 1-minute interval data to hourly to be able to compare and analyze with the hourly sunshine and cloud cover data. Take the mean of the energy produced in 60 minutes to come up with the hourly production.)
## J&E - Conclusion from analysis
