---
title: "Reproducible Research: Peer Assessment 1"
author: "Jiawen Cheong"
output: html_document
keep_md: true
---



## Introduction

This assignment makes use of data from a personal activity monitoring
device. This device collects data at 5 minute intervals through out the
day. The data consists of two months of data from an anonymous
individual collected during the months of October and November, 2012
and include the number of steps taken in 5 minute intervals each day.

## Data

The data for this assignment can be downloaded from the course web
site:

* Dataset: [Activity monitoring data](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) [52K]

The variables included in this dataset are:

* **steps**: Number of steps taking in a 5-minute interval (missing
    values are coded as `NA`)

* **date**: The date on which the measurement was taken in YYYY-MM-DD
    format

* **interval**: Identifier for the 5-minute interval in which
    measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there
are a total of 17,568 observations in this
dataset.


## Assignment

This assignment is answered in multiple parts. For each part, the code included to generate the output is presented.    

******

### Q1 Loading and preprocessing the data

1. **Load the data (i.e. read.csv())**

```r
# Unzip and load data
activitydata <- read.csv(unz("./activity.zip", "activity.csv"), header = TRUE)
```

2. **Process/transform the data (if necessary) into a format suitable for your analysis**


```r
# Convert "date" column to class "Date"
activitydata <- plyr::mutate(activitydata, date = as.Date(as.character(date)))
```

******

### Q2 What is mean total number of steps taken per day?

1. **Make a histogram of the total number of steps taken each day**

```r
# Find total number of steps taken each day
totalsteps <- with(activitydata, tapply(steps, date, sum))

# Create Histogram
par(mar = c(5,4,1,1), las = 1)
hist(totalsteps, main = "Distribution of Total Steps Taken Each Day", 
     xlab = "Total Steps Taken Each Day")
```

![plot of chunk totalsteps](figure/totalsteps-1.png)

2. **Calculate and report the mean and median total number of steps taken per day**

```r
# Find mean and media of total number of steps taken each day
meansteps <- mean(totalsteps, na.rm = TRUE)
mediansteps <- median(totalsteps, na.rm = TRUE)
```
The mean number of steps taken daily is **1.0766189 &times; 10<sup>4</sup>**.  
The median number of steps taken daily is **10765**.

******

### Q3 What is the average daily activity pattern?  

1. **Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**

```r
# Average number of steps taken in each 5-min interval across all days
dailypattern <- with(activitydata, tapply(steps,interval, mean, na.rm=TRUE))

# Create time series plot 
par(mar = c(5,4,1,1), las = 1)
plot(names(dailypattern), dailypattern, type = "l", main = "Time Series Plot of Average Daily Step Pattern", ylab = "Number of Steps", xlab = "5-min Intervals")
```

![plot of chunk dailypattern](figure/dailypattern-1.png)

2. **Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?** 

```r
# Average number of steps taken in each 5-min interval across all days
which.max(dailypattern)
```

```
## 835 
## 104
```

The 5-minute interval with the maximum average steps is **0835**.

******

### Q4 Inputing missing values

1. **Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**


```r
missingvalues <- sum(is.na(activitydata))
```
The total number of missing values in the dataset is **2304**.

2. **Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.**
We will use the mean of the respective 5-minute interval to replace NA values. These new values can be obtained from the dataset "dailypattern", which was created earlier. 


3. **Create a new dataset that is equal to the original dataset but with the missing data filled in.**


```r
# Create new dataset
cleandata <- activitydata

# Replace NA values with mean of respective 5-minute interval
for (i in 1:nrow(cleandata)) {
      if (is.na(cleandata[i,1])) {
            cleandata[i,1] <- dailypattern[as.character(cleandata[i,3])]
      } 
}
```

3. **Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?**


```r
# Find total number of steps taken each day
newtotalsteps <- with(cleandata, tapply(steps, date, sum))

# Create Histogram
par(mar = c(5,4,1,1), las = 1)
hist(newtotalsteps, main = "Distribution of Total Steps Taken Each Day", 
     xlab = "Total Steps Taken Each Day")
```

![plot of chunk newhistogram](figure/newhistogram-1.png)

```r
# Find mean and media of total number of steps taken each day
newmeansteps <- mean(newtotalsteps, na.rm = TRUE)
newmediansteps <- median(newtotalsteps, na.rm = TRUE)
```

The mean number of steps taken daily for the new dataset is **1.0766189 &times; 10<sup>4</sup>**, compared with the orginal mean of **1.0766189 &times; 10<sup>4</sup>**.  

The median number of steps taken daily for the new dataset is **1.0766189 &times; 10<sup>4</sup>**, compared with the orginal median of **10765**.  

There is no change to the mean and median of the total number of steps taken daily. However, the distribution of total steps taken each day has changed to account for the new values. 

******

### Q5 Are there differences in activity patterns between weekdays and weekends?

1. **Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.**

```r
# Create new factor variable indicating if a given data is a weekday or weekend
wkdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
cleandata <- mutate(cleandata, day = factor(1 * (weekdays(date) %in% wkdays), labels = c("weekday", "weekend")))
```

2. **Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).**

```r
# Split data into weekdays and weekends
by_day <- split(cleandata, cleandata$day)

# Find daily pattern for weekdays and weekends
day_patterns <- sapply(by_day, function(x) tapply(x$steps, x$interval, mean)) %>%
      melt(varnames = c("intervals","day"), value.name="steps")

# Create Panel Plot
xyplot(steps ~ intervals | day, data = day_patterns, type = "l", layout = c(1,2), ylab = "Number of steps", xlab = "Interval", main = "Daily Step Pattern of Weekdays and Weekends")
```

![plot of chunk panelplot](figure/panelplot-1.png)
