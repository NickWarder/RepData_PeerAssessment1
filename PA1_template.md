# Reproducible Research: Peer Assessment 1


## Code for reading in the dataset and/or processing the data

```r
##Here we bring in the data from the activity file, stored in the working directory, and preprocess the dates and intervals.
library(ggplot2)
data <- read.csv("activity.csv")
data$date <- as.Date(data$date, "%Y-%m-%d")
steps <- aggregate(steps ~ date, data, sum)
intervals <- aggregate(steps ~ interval, data, mean)
```


##Histogram of the total number of steps taken each day

```r
##We use the standard hist function to get the view we want from the aggregated data.

hist(steps$steps, xlab="Daily Steps", ylab="Frequency",main="Histogram of Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## What is the average daily activity pattern?
### Mean and median number of steps taken each day

```r
#The mean steps per day is 
mean(steps$steps)
```

```
## [1] 10766.19
```

```r
#The median steps per day is 
median(steps$steps)
```

```
## [1] 10765
```

###Time series plot of the average number of steps taken

```r
##We use the standard plot function to get the view we want from the aggregated data.

plot(intervals, type="l", xlab="5-minute interval", ylab="Steps", main="Steps per 5-minute interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

###The 5-minute interval that, on average, contains the maximum number of steps

```r
##The peak 5-minute interval for steps is
intervals[which.max(intervals$steps),c("interval")]
```

```
## [1] 835
```

## Code to describe and show a strategy for imputing missing data in the base dataset

```r
##
impute <- is.na(data$steps)
#There are
sum(impute)
```

```
## [1] 2304
```

```r
#missing values in the base dataset.

##Create a new dataset that is equal to original but with missing data filled in.
data$steps[is.na(data$steps)] <- median(data$steps, na.rm=TRUE)

##Get the new values
newSteps <- aggregate(steps ~ date, data, sum)
newIntervals <- aggregate(steps ~ interval, data, mean)
#The imputed mean steps per day is 
mean(newSteps$steps)
```

```
## [1] 9354.23
```

```r
#The imputed median steps per day is 
median(newSteps$steps)
```

```
## [1] 10395
```

```r
#The mean has changed by
mean(steps$steps) - mean(newSteps$steps)
```

```
## [1] 1411.959
```

```r
#and the median has changed by
median(steps$steps) - median(newSteps$steps) 
```

```
## [1] 370
```

```r
#as a result of this imputing. Updated graph follows.
```


##Histogram of the total number of steps taken each day after missing values are imputed

```r
##We use the standard hist function to get the view we want from the aggregated data.

hist(newSteps$steps, xlab="Daily Steps", ylab="Frequency",main="Imputed Histogram of Total Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

##Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

```r
##Label as to whether this is the weekend or not so we can split graphs
dayOfWeek <- weekdays(as.Date(data$date))
for (i in 1:nrow(data)) {
  if(dayOfWeek[i] == "Saturday") {dayOfWeek[i] <- "Weekend"}
  else if(dayOfWeek[i] == "Sunday"){dayOfWeek[i] <- "Weekend"}
  else {dayOfWeek[i] = "Weekday"}
}

data <- cbind(data,dayOfWeek)

#Split out the data
Weekday <- subset(data,data$dayOfWeek == "Weekday")
Weekend <- subset(data,data$dayOfWeek == "Weekend")

WeekdayIntervals <- aggregate(steps ~ interval, Weekday, mean)
WeekendIntervals <- aggregate(steps ~ interval, Weekend, mean)

#Display
par(mfrow=c(1,2))
plot(WeekdayIntervals, type="l", main="Weekday")
plot(WeekendIntervals, type="l", main="Weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->
