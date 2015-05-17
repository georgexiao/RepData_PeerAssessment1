# Reproducible Research: Peer Assessment 1
George Xiao  
5/16/2015  

## Introduction
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

[Here](https://github.com/rdpeng/RepData_PeerAssessment1) you can find the full description of this assignment. 

## Global Settings


```r
knitr::opts_chunk$set(fig.width=10, fig.height=6, fig.path='Figs/',
                      echo=TRUE, warning=FALSE, message=FALSE)
library(knitr)
library(ggplot2)
```

## Loading and preprocessing the data
Firstly, let's unzip the [data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) (if you haven't done that) and load it into R.


```r
if(!file.exists("activity.csv")) unzip(zipfile="activity.zip")
activity <- read.table("activity.csv", sep = ",", header = T)
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

There are three columns in the data:

- **steps**: Number of steps taking in a 5-minute interval (missing values are coded as `NA`).
- **date**: The date on which the measurement was taken in YYYY-MM-DD format.
- **interval**: Identifier for the 5-minute interval in which measurement was taken.

One thing we want to do before analysis is to change the **date** into the date type in R:


```r
activity$date <- as.Date(activity$date, "%Y-%m-%d")
```

## What is mean total number of steps taken per day?
Fistly, calculate the total number of steps taken per day:


```r
stepsPerDay <- aggregate(steps ~ date, data = activity, FUN=sum, na.rm=TRUE)
names(stepsPerDay) <- c("Date", "Steps")
```

Secondly, let's create a histogram by library `ggplot2`:

```r
ggplot(stepsPerDay, aes(x = Steps)) + 
    geom_histogram(binwidth = 2000, col = "white", aes(fill = ..count..)) +
    scale_fill_gradient("Count", low = "grey80", high = "steelblue4") +
    labs(x = "Steps per Day", y = "Count") + 
    labs(title = "Histogram of the total number of steps taken each day")
```

![](Figs/histogram-1.png) 

Lastly, calculate and report the mean and median of the total number of steps taken per day:

```r
sprintf("The mean and median of the total number of steps taken per day is %.2f and %.2f.", 
        mean(stepsPerDay$Steps), median(stepsPerDay$Steps))
```

```
## [1] "The mean and median of the total number of steps taken per day is 10766.19 and 10765.00."
```

## What is the average daily activity pattern?
To answer this question, I will make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
stepsMeanPerInterval <- aggregate(steps ~ interval, data = activity, FUN = mean, na.rm=TRUE)
names(stepsMeanPerInterval) <- c("Interval", "Steps")

ggplot(stepsMeanPerInterval, aes(x= Interval, y = Steps)) + 
    geom_line(col = "steelblue4", size=1) +
    labs(x = "5 Mins Interval", y = "Mean Steps per Interval") + 
    labs(title = "Time series plot of mean steps per interval")
```

![](Figs/time_serious-1.png) 

Then calculate which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:


```r
maxSteps <- stepsMeanPerInterval[stepsMeanPerInterval$Steps == 
                                     max(stepsMeanPerInterval$Steps), "Interval"]
sprintf(paste("The '%s' 5 minutes interval contains the maximum number of steps,", 
        "on average across all the days in the dataset."), maxSteps)
```

```
## [1] "The '835' 5 minutes interval contains the maximum number of steps, on average across all the days in the dataset."
```


## Imputing missing values
Fistly, calculate and report the total number of missing values in the dataset:

```r
numNA <- sum(is.na(activity$steps))
sprintf("The total number of missing values in the dataset is %s.", numNA)
```

```
## [1] "The total number of missing values in the dataset is 2304."
```

Secondly, use mean for that 5-minute interval to fill in all of the missing values in the dataset. Create a new dataset `newActivity` that is equal to the original dataset but with the missing data filled in:


```r
newActivity <- activity 
for (i in 1:nrow(newActivity)) {
    if (is.na(newActivity$steps[i]))
        newActivity$steps[i] <- stepsMeanPerInterval[stepsMeanPerInterval$Interval 
                                                     == newActivity$interval[i], "Steps"]
    }
```

Lastly, make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day: 


```r
newStepsPerDay <- aggregate(steps ~ date, data = newActivity, FUN = sum, na.rm=TRUE)
names(newStepsPerDay) <- c("Interval", "Steps")

ggplot(newStepsPerDay, aes(x = Steps)) + 
    geom_histogram(binwidth = 2000, col = "white", aes(fill = ..count..)) +
    scale_fill_gradient("Count", low = "grey80", high = "steelblue4") +
    labs(x = "Steps per Day", y = "Count") + 
    labs(title = "Histogram of the total number of steps taken each day")
```

![](Figs/new_hist-1.png) 

Compare these values with the estimates from the first part of the assignment. Find out the impact of imputing missing data on the estimates of the total daily number of steps:


```r
sprintf(paste("The mean and median of the total number of steps per day for the new data is %.2f and %.2f.", 
        "The difference from the original mean and median is %.2f and %.2f."), 
        mean(newStepsPerDay$Steps), median(newStepsPerDay$Steps),
        mean(stepsPerDay$Steps) - mean(newStepsPerDay$Steps), 
        median(stepsPerDay$Steps) - median(newStepsPerDay$Steps))
```

```
## [1] "The mean and median of the total number of steps per day for the new data is 10766.19 and 10766.19. The difference from the original mean and median is 0.00 and -1.19."
```

Therefore, the mean of new data without NAs is the same as the original mean. The new median is less than orignal one by 1.19.

## Are there differences in activity patterns between weekdays and weekends?
Use the dataset with the filled-in missing values for this part. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
for (i in 1:nrow(activity)) {
    activity$weekday[i] <- 
        if(weekdays(activity$date[i]) %in% c("Sunday", "Saturday")) "Weekend" else "Weekday"
}
```

Then make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
stepsMeanPerIntervalByDate <- aggregate(steps ~ interval + weekday, data = activity, 
                                        FUN = mean, na.rm=TRUE)
names(stepsMeanPerIntervalByDate) <- c("Interval", "Weekday", "Steps")

ggplot(stepsMeanPerIntervalByDate, aes(x= Interval, y = Steps)) + 
    geom_line(col = "steelblue4", size=1) +
    labs(x = "5 Mins Interval", y = "Mean Steps per Interval") + 
    labs(title = "Weekdays VS Weekends: Time series plot of mean steps per interval") +
    facet_grid(Weekday~.)
```

![](Figs/weekday_vs_weekend-1.png) 

