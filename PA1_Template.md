---
title: 'Reproducible Research: Peer Assessment 1'
author: "Manoj Krishna"
date: "February 24, 2018"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

## Introduction
It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement - a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from the course web site:

Dataset: Activity monitoring data [52K]
The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)
date: The date on which the measurement was taken in YYYY-MM-DD format
interval: Identifier for the 5-minute interval in which measurement was taken
The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

## Loading and preprocessing the data


```{r Loading}
#remove all objects
rm(list=ls())

#if the source file is loaded onto the working directory , if not, downlaod and unzip the file:
if(!file.exists("activity.csv")) {
        tempfile <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",destfile = tempfile)
        unzip(tempfile)
        unlink(tempfile)
}
#loading the data
activity <- read.csv("activity.csv")
```

Inspecting the Data Set
```{r summary, echo=TRUE}
summary(activity)
str(activity)
```


# Mean Total Number of Steps taken Per Day

##1. Total number of steps taken per day
```{r total number of steps, echo=TRUE}
activity_steps_day <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
```


##2, Histogram of total number of steps taken each day
```{r Histogram, echo=TRUE}
hist(activity_steps_day$steps, xlab = "Steps per Day", main = "Total number of steps taken per day", col = "wheat")
```


##3, mean and median of the total number of steps taken per day

```{r Mean and Median, echo=TRUE}
mean_steps <- mean(activity_steps_day$steps)
median_steps <- median(activity_steps_day$steps)
#set a normal number format to display the results
mean_steps <- format(mean_steps,digits=1)
median_steps <- format(median_steps,digits=1)
#
```
Mean steps per day: 10766
Median steps per day: 10765

# Average Daily Activity Pattern

##1, Time Series Plot

```{r Time Series Plot, echo=TRUE}
#Aggregate function for mean over all days, for each interval
activity_steps_mean <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm = TRUE)
#Plot
plot(activity_steps_mean$interval, activity_steps_mean$steps, type = "l", col = "tan3", xlab = "Intervals", ylab = "Total steps per interval", main = "Number of steps per interval (averaged) (NA removed)")
#
```


##2, Which 5-minute interval, on average across all the days in the dataset, contains the #maximum number of steps?
#
```{r 5 Minute Interval, echo=TRUE}
#what is the highest steps value? (maximum of steps on one given interval)
max_steps <-max(activity_steps_mean$steps)
#for which interval are the numbers of steps per interval at the highest?
max_interval <- activity_steps_mean$interval[which(activity_steps_mean$steps == max_steps)]
max_steps <- round(max_steps, digits = 2)
#
```
The highest number of steps for a 5 minutes interval is 206.17,
which corresponds to interval 835

#Imputing missing values

##1. Calculate total number of missing values in the dataset
```{r Missing Values, echo=TRUE}
sum(is.na(activity))

```


##2. Devise a strategy for filling in all of the missing values in the dataset
```{r filling missing valules, echo=TRUE}
##subset general dataset with missing values only
missing_values <- subset(activity, is.na(steps))
#plot repartition, by date or by intervals
par(mfrow = c(2,1), mar = c(2, 2, 1, 1))
hist(missing_values$interval, main="NAs repartition per interval")
hist(as.numeric(missing_values$date), main = "NAs repartion per date", breaks = 61)

```


##3, Create new dataset with the missing data filled in
#
```{r Missing data set, echo=TRUE}
# calculate mean of steps per interval, we end up with a mean for all 288 intervals
MeanStepsPerInterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
# cut the 'activity' dataset in 2 parts (with and without NAs)
activity_NAs <- activity[is.na(activity$steps),]
activity_non_NAs <- activity[!is.na(activity$steps),]
#replace missing values in activity_NAs
activity_NAs$steps <- as.factor(activity_NAs$interval)
levels(activity_NAs$steps) <- MeanStepsPerInterval
#change the vector back as integer 
levels(activity_NAs$steps) <- round(as.numeric(levels(activity_NAs$steps)))
activity_NAs$steps <- as.integer(as.vector(activity_NAs$steps))
#merge/rbind the two datasets together
imputed_activity <- rbind(activity_NAs, activity_non_NAs)
#
```


##4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
#
```{r Making histogram, echo=TRUE}
#Plotting parameters to place previous histogram and new one next to each other
par(mfrow = c(1,2))
#Plot again the histogram from the first part of the assignment
activity_steps_day <- aggregate(steps ~ date, data = activity, FUN = sum, na.rm = TRUE)
hist(activity_steps_day$steps, xlab = "Steps per Day", main = "NAs REMOVED - Total steps/day", col = "wheat")
#Plot new histogram, with imputed missing values
imp_activity_steps_day <- aggregate(steps ~ date, data = imputed_activity, FUN = sum, na.rm = TRUE)
hist(imp_activity_steps_day$steps, xlab = "Steps per Day", main = "NAs IMPUTED - Total steps/day", col = "wheat")
#
```


The mean and median values, and store the new and old results in a data frame for easier comparison:
```{r Old Mean and Median, echo=TRUE}
#
imp_mean_steps <- mean(imp_activity_steps_day$steps)
imp_median_steps <- median(imp_activity_steps_day$steps)
#we set a normal number format to display the results
imp_mean_steps <- format(imp_mean_steps,digits=1)
imp_median_steps <- format(imp_median_steps,digits=1)
#store the results in a dataframe
results_mean_median <- data.frame(c(mean_steps, median_steps), c(imp_mean_steps, imp_median_steps))
colnames(results_mean_median) <- c("NA removed", "Imputed NA values")
rownames(results_mean_median) <- c("mean", "median")
#
#
```

Using the xtable package to print the table with all values:
```{r xtable, echo=TRUE}
#
library(xtable)
xt <- xtable(results_mean_median)
print(xt, type  = "html")
#
```

Imputing missing values did not change the mean value. But the median value is reduced  by 0.027% (3/10765*100).
Both histograms show the same structure, with imputed NAs with higher frequencies.


#Are there differences in activity patterns between weekdays and weekends?

##1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" ##indicating whether a given date is a weekday or weekend day.
```{r New Factor variable, echo=TRUE}
#elseif function to categorize Saturday and Sunday as factor level "weekend", all the rest as "weekday"
imputed_activity$dayType <- ifelse(weekdays(as.Date(imputed_activity$date)) == "Samstag" | weekdays(as.Date(imputed_activity$date)) == "Sonntag", "weekend", "weekday")
#transform dayType variable into factor
imputed_activity$dayType <- factor(imputed_activity$dayType)
#
```
#


##2. panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval ##(x-axis) and the average number of steps taken, averaged across all weekday days or ##weekend days (y-axis). See the README file in the GitHub repository to see an example of ##what this plot should look like using simulated data.
```{r Create Panel Plots, echo=TRUE}
#Aggregate a table showing mean steps for all intervals, acrlss week days and weekend days
steps_interval_dayType <- aggregate(steps ~ interval + dayType, data = imputed_activity, FUN = mean)
#verify new dataframe 
head(steps_interval_dayType)
#
#
#add descriptive variables
names(steps_interval_dayType) <- c("interval", "day_type", "mean_steps")
#plot with ggplot2
library(ggplot2)
plot <- ggplot(steps_interval_dayType, aes(interval, mean_steps))
plot + geom_line(color = "tan3") + facet_grid(day_type~.) + labs(x = "Intervals", y = "Average Steps", title = "Activity Patterns")
#
```

