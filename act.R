#  Peer Graded Assignment: Course Project 1: Activity monitoring data 
#  Mac os X 10.11.5
#Introduction

#This assignment makes use of data from a personal 
#activity monitoring device. This device collects data at 5 minute intervals through
#out the day. The data consists of two months of data from an anonymous individual 
#collected during the months of October and November, 2012 and include the number of 
#steps taken in 5 minute intervals each day.

# load the necessary packages

library(dplyr)
library(knitr)
library(ggplot2)
library(lattice)


#Preparation


# Task.1: Download and read the source data file 

fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#ownload.file(fileurl,destfile = "./act.zip", method = "curl")
unlink(fileurl)
unzip(zipfile = "./act.zip")
actData <- read.csv("activity.csv",header = TRUE,colClasses=c("numeric", "character", "numeric"))

# Tidy the data classess

actData$date <- as.Date(actData$date,"%Y-%m-%d")
actData$interval<- as.factor(actData$interval)

#Task.2: Histogram of the total number of steps taken each day

stepsperday <- aggregate(actData$steps, by= list(actData$date),sum,na.rm = TRUE)

names(stepsperday) <- c("date","steps")

head(stepsperday)

month <- as.POSIXlt(stepsperday$date)$mon+1

day <- as.POSIXlt(stepsperday$date)$mday

histplot <- ggplot(data= stepsperday,aes(x = factor(day),fill = factor(month), y= steps)) + 
        geom_bar(stat = "identity") +
        labs(title = "Total Number of Steps per Day") + 
        labs(x= "Days",y="Total Numbers per Day") + 
        scale_fill_discrete(name = "Month")

print(histplot)

#Task.3:Mean and median number of steps taken each day

meanstepsperday <- aggregate(actData$steps, by = list(actData$date),mean,na.rm = TRUE)

names(meanstepsperday) <- c("date","mean")

actData1 <- actData[-which(actData$steps == 0),]

medianstepsperday <- aggregate(actData1$steps, by = list(actData1$date),FUN = median,na.rm = TRUE)

names(medianstepsperday) <- c("date","median")

MMstepsperday <- data.frame(Mean = meanstepsperday$mean,Median = medianstepsperday$median, Date = meanstepsperday$date)

print(MMstepsperday)

#Task.4: Time series plot of the average number of steps taken 

intervalSteps <- aggregate(actData$steps,by = list(actData$interval),mean, na.rm = TRUE)

names(intervalSteps) <- c("interval", "steps")

intervalSteps$interval <- as.numeric(intervalSteps$interval)

timeplot <- ggplot(intervalSteps,aes(x = interval,y = steps)) +
        geom_line(color = "orange", size = 1) + 
        labs(x="5-minute interval",y= "steps",title="Average Number of Steps Taken")

print(timeplot)
        

#Task.5 The 5-minute interval that, on average, contains the maximum number of steps

maxInterval <- intervalSteps[which.max(intervalSteps$steps),]

print(maxInterval)

#Task.6 Code to describe and show a strategy for imputing missing data


newactData <- actData

newmeansteps <- aggregate(newactData$steps, by = list(newactData$interval),mean, na.rm= TRUE)

names(newmeansteps) <- c("interval","steps")

Mergeddata <- merge(newactData,newmeansteps,by = "interval")

Mergeddata$steps.x[is.na(Mergeddata$steps.x)] <- Mergeddata$steps.y[is.na(Mergeddata$steps.x)]

names(Mergeddata) <- c("interval","steps","date","meanstep")

#Task.7 Histogram of the total number of steps taken each day after missing values are imputed

stepsfilled <- aggregate(Mergeddata$steps,by = list(Mergeddata$date),sum,na.rm=TRUE)

names(stepsfilled) <- c("datef","stepsf")

head(stepsfilled)

monthf <- as.POSIXlt(stepsfilled$datef)$mon+1

dayf <- as.POSIXlt(stepsfilled$datef)$mday


histplotfilled <- ggplot(data= stepsfilled,aes(x = factor(dayf),fill = factor(monthf), y= stepsf)) + 
        geom_bar(stat = "identity") +
        labs(title = "Total Number of Steps per Day After Filled") + 
        labs(x= "Days",y="Total Numbers per Day") + 
        scale_fill_discrete(name = "Month")

print(histplotfilled)


# Task.8 Panel plot comparing the average number of steps taken per 
#5-minute interval across weekdays and weekends

wdays <- as.POSIXlt(actData$date)$wday

wdaysclass <- rep(0, length(actData$date))

wdaysclass[wdays>=1 & wdays <= 5] <- 1 #weekdays

wdaysclass[wdays ==6 | wdays == 0] <-2 #weekend

dayfactor <- factor(wdaysclass,levels = c(1,2),labels = c("weekdays","weekends"))

newactData$days <- dayfactor

WWstepsmean <- aggregate(steps~interval+days,newactData,mean, na.rm= TRUE)

xyplot(steps ~ interval | factor(days), data = WWstepsmean,aspect = 1/2, type = "l",col = "blue")













