Reproducible Research: Peer Assessment 1
==========================================
Created by Xianyang Wong on May 16, 2015

### Basic settings

```r
echo = TRUE              
options(scipen = 1)      
```

### Loading and preprocessing the data

```r
unzip("activity.zip")
data <- read.csv("activity.csv", colClasses = c("integer", "Date", "factor"))
data$month <- as.numeric(format(data$date, "%m"))
base <- na.omit(data)
rownames(base) <- 1:nrow(base)
head(base)
```

```
##   steps       date interval month
## 1     0 2012-10-02        0    10
## 2     0 2012-10-02        5    10
## 3     0 2012-10-02       10    10
## 4     0 2012-10-02       15    10
## 5     0 2012-10-02       20    10
## 6     0 2012-10-02       25    10
```

```r
dim(base)
```

```
## [1] 15264     4
```

### What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.

* Calculate the total number of steps taken per day and make a histogram to display the info.


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
ggplot(base, aes(date, steps)) + geom_bar(stat = "identity", colour = "blue", fill = "blue", width = 0.8) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day", x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

* Calculate and report the mean and median total number of steps taken per day.

Mean total number of steps taken per day:

```r
totalSteps <- aggregate(base$steps, list(Date = base$date), FUN = "sum")$x
mean(totalSteps)
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
median(totalSteps)
```

```
## [1] 10765
```

### What is the average daily activity pattern?
* Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
avgSteps <- aggregate(base$steps, list(interval = as.numeric(as.character(base$interval))), FUN = "mean")
names(avgSteps)[2] <- "meanSteps"

ggplot(avgSteps, aes(interval, meanSteps)) + geom_line(color = "blue", size = 0.8) + 
  labs(title = "5-minute Interval Time Series Plot", x = "5-minute intervals", y = "Average Number of Steps Taken")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png) 

* Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgSteps[avgSteps$meanSteps == max(avgSteps$meanSteps), ]
```

```
##     interval meanSteps
## 104      835  206.1698
```

### Imputing missing values
* The total number of rows with NAs:


```r
sum(is.na(data))
```

```
## [1] 2304
```

* Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Missing values in the steps column are to be filled by using the mean for that 5-minute interval.

* Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
filleddata <- data 
for (i in 1:nrow(filleddata)) {
    if (is.na(filleddata$steps[i])) {
        filleddata$steps[i] <- avgSteps[which(filleddata$interval[i] == avgSteps$interval), ]$meanSteps
    }
}

head(filleddata)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
sum(is.na(filleddata))
```

```
## [1] 0
```

* Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 


```r
ggplot(filleddata, aes(date, steps)) + geom_bar(stat = "identity", colour = "blue", fill = "blue", width = 0.8) + facet_grid(. ~ month, scales = "free") + labs(title = "Histogram of Total Number of Steps Taken Each Day (no missing data)", x = "Date", y = "Total number of steps")
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png) 

* Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Mean total number of steps taken per day:

```r
newTotalSteps <- aggregate(filleddata$steps, 
                           list(Date = filleddata$date), 
                           FUN = "sum")$x
newMean <- mean(newTotalSteps)
newMean
```

```
## [1] 10766.19
```
Median total number of steps taken per day:

```r
newMedian <- median(newTotalSteps)
newMedian
```

```
## [1] 10766.19
```
Compare them with the two before imputing missing data:

```r
oldMean <- mean(totalSteps)
oldMedian <- median(totalSteps)
newMean - oldMean
```

```
## [1] 0
```

```r
newMedian - oldMedian
```

```
## [1] 1.188679
```
After imputing the missing data, the new mean of total steps taken per day is the same as that of the old mean while the new median of total steps taken per day is greater than that of the old median.
Also, the new mean is now equal to the new median.

### Are there differences in activity patterns between weekdays and weekends?

* Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
head(filleddata)
```

```
##       steps       date interval month
## 1 1.7169811 2012-10-01        0    10
## 2 0.3396226 2012-10-01        5    10
## 3 0.1320755 2012-10-01       10    10
## 4 0.1509434 2012-10-01       15    10
## 5 0.0754717 2012-10-01       20    10
## 6 2.0943396 2012-10-01       25    10
```

```r
filleddata$weekdays <- factor(format(filleddata$date, "%A"))
levels(filleddata$weekdays)
```

```
## [1] "Friday"    "Monday"    "Saturday"  "Sunday"    "Thursday"  "Tuesday"  
## [7] "Wednesday"
```

```r
levels(filleddata$weekdays) <- list(weekday = c("Monday", "Tuesday",
                                             "Wednesday", 
                                             "Thursday", "Friday"),
                                 weekend = c("Saturday", "Sunday"))
levels(filleddata$weekdays)
```

```
## [1] "weekday" "weekend"
```

```r
table(filleddata$weekdays)
```

```
## 
## weekday weekend 
##   12960    4608
```

* Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
avgSteps <- aggregate(filleddata$steps, 
                      list(interval = as.numeric(as.character(filleddata$interval)), 
                           weekdays = filleddata$weekdays),
                      FUN = "mean")
names(avgSteps)[3] <- "meanSteps"
library(lattice)
xyplot(avgSteps$meanSteps ~ avgSteps$interval | avgSteps$weekdays, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
