#Reproduce Data Project 1   

#reference packages
library(downloader)
library("plyr")

filename <-paste(getwd(),"/activity.zip",sep="")  #set file name to unzip
unzip (filename, exdir = getwd())                 #unzip file for analysis


### Loading and preprocessing the data
rawdata<-read.csv("./activity.csv",
                  header=TRUE,
                  sep=",",
                  colClasses=c("numeric","character","factor"),
                  na.strings = "NA"
)                                                 #read in raw .csv file      

str(rawdata)                                      #check formats
rawdata$date <- as.Date(rawdata$date)             #convert charcater date to actual date format
str(rawdata)                                      #check formats                 


### What is mean total number of steps taken per day?
totalsteps<-ddply(rawdata,c("date"),
                  summarize,
                  totalsteps=sum(steps,
                                 rm.na=TRUE))     #calculate total number of steps by day

summary(totalsteps$totalsteps)
hist(totalsteps$totalsteps,
     xlab="Total Steps by Day",
     ylab="Frequency",
     ylim=c(0,30),
     main="Histogram of Total Steps by Day"
)                                                 #histogram of total steps by day


meansteps<-mean(totalsteps$totalsteps,na.rm=TRUE)
meansteps

mediansteps<-median(totalsteps$totalsteps,na.rm=TRUE)
mediansteps

### What is the average daily activity pattern?


### Imputing missing values

