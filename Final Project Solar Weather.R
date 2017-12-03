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
##solar_url <- getURL("TBD")
##solar_raw <- read.csv(text = solar_url)
##head(solar_raw)

##DATA TIDYING
##Separate the date and hour start times into two columns in order to be able to match solar production and weather
##Create dataframe with the columns: Date / HourBegin / PercentCloudCover
sunshine_df <- as.data.frame(sunshine_raw)
sunshine_df$Date <- sapply(strsplit(as.character(sunshine_df$IntervalStartDt), " "), "[", 1)
sunshine_df$HourBegin <- sapply(strsplit(as.character(sunshine_df$IntervalStartDt), " "), "[", 2)
new_sunshine <- data.frame(sunshine_df$Date,sunshine_df$HourBegin,sunshine_df$Sunshine)
colnames(new_sunshine) <- c("Date", "HourBegin", "SunshineMinutes")
head(new_sunshine)

cloud_df <- as.data.frame(cloud_raw)
cloud_df$Date <- sapply(strsplit(as.character(cloud_df$IntervalStartDt), " "), "[", 1)
cloud_df$HourBegin <- sapply(strsplit(as.character(cloud_df$IntervalStartDt), "  "), "[", 2)
new_cloud <- data.frame(cloud_df$Date,cloud_df$HourBegin,cloud_df$CloudCover)
colnames(new_cloud) <- c("Date", "HourBegin", "PercentCloudCover")
head(new_cloud)


##DATA ANALYSIS
##Aggregate the Sunshine and Cloud Cover data set into one point for each day in the month

onedate <- new_cloud[c(TRUE,rep(FALSE,23)), ]

SunshineDaily <- round(colMeans(matrix(new_sunshine$SunshineMinutes, nrow=24)), digits=0)
SunshineDaily_df <- data.frame(onedate$Date,SunshineDaily)
colnames(SunshineDaily_df) <- c("Date","SunshineMinutes")
SunshineDaily_df 

CloudCoverDaily <- round(colMeans(matrix(new_cloud$PercentCloudCover, nrow=24)), digits=0)
CloudCoverDaily_df <- data.frame(onedate$Date,CloudCoverDaily)
colnames(CloudCoverDaily_df) <- c("Date","PercentCloudCover")
CloudCoverDaily_df 



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


