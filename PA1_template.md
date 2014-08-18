# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
- Load the data (i.e. ```read.csv()```)  

```r
data0 <- read.csv("activity.csv", colClasses = c("integer", "Date", "integer"))
```

- Process/transform the data (if necessary) into a format suitable for your analysis  

Make a new data set containing no NA  

```r
data1 <- data0[!is.na(data0$steps), ]
```
Change the language of dates to English  

```r
Sys.setlocale("LC_TIME", "C")
```

```
## [1] "C"
```

## What is mean total number of steps taken per day?
- Make a histogram of the total number of steps taken each day  

```r
totalsteps <- tapply(data1$steps, data1$date, sum)
hist(totalsteps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

- Calculate and report the mean and median total number of steps taken per day  

```r
list(mean = mean(totalsteps), median = median(totalsteps))
```

```
## $mean
## [1] 10766
## 
## $median
## [1] 10765
```

## What is the average daily activity pattern?
- Make a time series plot (i.e. ```type = "l"```) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

```r
averagesteps <- tapply(data1$steps, data1$interval, mean)
plot(names(averagesteps), averagesteps, type = "l", xlab = "Interval", ylab = "Average number of steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 
 
- Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
names(which.max(averagesteps))
```

```
## [1] "835"
```

## Imputing missing values
- Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with ```NA```s)  

```r
sum(apply(data0, 1, function(x) sum(is.na(x)) > 0))
```

```
## [1] 2304
```

- Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
Use the mean for that 5-minute interval  

- Create a new dataset that is equal to the original dataset but with the missing data filled in.  

```r
data2 <- data0
for (i in 1: nrow(data2))
  if (is.na(data2[i, 1]))
      data2[i, 1] = averagesteps[as.character(data2[i, 3])]
```

- Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  

```r
totalsteps <- tapply(data2$steps, data2$date, sum)
hist(totalsteps)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 

```r
list(mean = mean(totalsteps), median = median(totalsteps))
```

```
## $mean
## [1] 10766
## 
## $median
## [1] 10766
```
The mean stays the same, but the median becomes slightly larger.  

## Are there differences in activity patterns between weekdays and weekends?
- Create a new factor variable in the dataset with two levels ¨C ¡°weekday¡± and ¡°weekend¡± indicating whether a given date is a weekday or weekend day.  

```r
weekday <- weekdays(data1$date)
data1$weekday <- factor(weekday == "Saturday" | weekday == "Sunday", labels = c("weekday", "weekend"))
```

- Make a panel plot containing a time series plot (i.e. ```type = "l"```) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).  

```r
data <- split(data1, data1$weekday)

weekend <- as.data.frame(tapply(data$weekend$steps, data$weekend$interval, mean))
names(weekend) <- "steps"
weekend$interval <- as.integer(rownames(weekend))
weekend$weekday <- "weekend"

weekday <- as.data.frame(tapply(data$weekday$steps, data$weekday$interval, mean))
names(weekday) <- "steps"
weekday$interval <- as.integer(rownames(weekend))
weekday$weekday <- "weekday"

weekdata <- rbind(weekend, weekday)
weekdata$weekday <- factor(weekdata$weekday)
library(lattice)
xyplot(steps ~ interval | weekday, data = weekdata, layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 
