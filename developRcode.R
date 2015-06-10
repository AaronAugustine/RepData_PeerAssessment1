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
                  colClasses=c("numeric","character","numeric"),
                  na.strings = "NA"
)                                                 #read in raw .csv file      

str(rawdata)                                      #check formats
rawdata$date <- as.Date(rawdata$date)             #convert charcater date to actual date format
str(rawdata)                                      #check formats                 


### What is mean total number of steps taken per day?
totalsteps<-ddply(rawdata,c("date"),
                  summarize,
                  totalsteps=sum(steps,
                                 na.rm=TRUE))     #calculate total number of steps by day

summary(totalsteps$totalsteps)
hist(totalsteps$totalsteps,
     xlab="Total Steps by Day",
     ylab="Frequency",
     ylim=c(0,40),
     main="Histogram of Total Steps by Day"
)                                                 #histogram of total steps by day


meansteps<-mean(totalsteps$totalsteps,na.rm=TRUE) #calculate mean total steps
meansteps

mediansteps<-median(totalsteps$totalsteps,na.rm=TRUE) # calcualte median total steps
mediansteps

### What is the average daily activity pattern?
avgsteps<-ddply(rawdata,c("interval"),summarize,
                  avgsteps=mean(steps,
                  na.rm=TRUE))     #calculate mean number of steps by day

with(avgsteps, plot(interval, avgsteps
                   ,xlab="Interval"
                   ,ylab="Avg Steps across Days"
                   ,type="l"))

maxsteps<-avgsteps[avgsteps$avgsteps==max(avgsteps$avgsteps,na.rm=TRUE),]
maxsteps                  

### Imputing missing values 
nummissing<-sum(is.na(rawdata$steps))             #number of missing records in the file
nummissing

adjdata <- merge(rawdata,avgsteps,
                 by.x="interval",
                 by.y="interval",
                 all.x=TRUE)
adjdata$adjsteps <- adjdata$steps                 #copy steps into adjsteps
adjdata$adjsteps[is.na(adjdata$steps)] <- adjdata$avgsteps[is.na(adjdata$steps)] 

adjtotalsteps<-ddply(adjdata,c("date"),
                  summarize,
                  totalsteps=sum(adjsteps,
                                 na.rm=TRUE))     #calculate total number of steps by day

summary(adjtotalsteps$totalsteps)
hist(adjtotalsteps$totalsteps,
     xlab="Total Steps by Day",
     ylab="Frequency",
     ylim=c(0,40),
     main="Histogram of Total Steps by Day adjusted for NAs"
)                                                 #histogram of total steps by day


adjmeansteps<-mean(adjtotalsteps$totalsteps,na.rm=TRUE) #calculate mean total steps
adjmeansteps

adjmediansteps<-median(adjtotalsteps$totalsteps,na.rm=TRUE) # calcualte median total steps
adjmediansteps


### Are there differences in activity patterns between weekdays and weekends?
adjdata$weekday <- weekdays(adjdata$date)
adjdata<-transform(adjdata ,
                   weekend = ifelse(weekday %in% c("Sunday","Saturday"),
                                    "Weekend",
                                    "Weekday" ))
avgstepsbyweekend<-ddply(adjdata,c("interval","weekend"),summarize,
                         avgsteps=mean(steps,
                                       na.rm=TRUE)) 
                                        #calculate mean number of steps by weekend
library(lattice) 
xyplot(avgsteps~interval | weekend, data = avgstepsbyweekend,
       type = 'l',
       xlab = 'Interval',
       ylab = 'Number of Steps',
       main = 'Number of Steps across intervals, Weekends vs. Weekdays',
       layout = c(1,2))
