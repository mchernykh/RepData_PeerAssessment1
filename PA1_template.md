---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
library(ggplot2)
data <- read.csv("activity.csv", colClasses = c("numeric", "POSIXct", "numeric"))

# analysis in locale-sensitive!
Sys.setlocale("LC_TIME", "English")
```

```
## [1] "English_United States.1252"
```


## What is mean total number of steps taken per day?

### Make a histogram of the total number of steps taken each day

```r
total_steps_by_day <- data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = T))

ggplot(total_steps_by_day, aes(x = date, y = total_steps)) +
  geom_bar(stat = "identity") +
  xlab("day") + 
  ylab("total steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

### Calculate and report the mean and median total number of steps taken per day

```r
mean(total_steps_by_day$total_steps)
```

```
## [1] 9354.23
```


```r
median(total_steps_by_day$total_steps)
```

```
## [1] 10395
```



## What is the average daily activity pattern?

### Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
average_steps_by_interval <- data %>%
  group_by(interval) %>%
  summarise(mean_steps = mean(steps, na.rm = T))

ggplot(average_steps_by_interval, aes(x = interval, y = mean_steps)) +
  geom_line() +
  xlab("interval") + 
  ylab("average steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
average_steps_by_interval[which.max(average_steps_by_interval$mean_steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval mean_steps
## 1      835   206.1698
```

## Imputing missing values


### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Lets impute mean number f steps for the interval.  
**It is floating point number!**

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
imputed_data <- data %>%
  group_by(interval) %>%
  mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps)) %>%
  ungroup()
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
total_steps_by_day_imputed <- imputed_data %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps, na.rm = T))

ggplot(total_steps_by_day_imputed, aes(x = date, y = total_steps)) +
  geom_bar(stat = "identity") +
  xlab("day") + 
  ylab("total steps (imputed data)")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

Mean and median of steps:


```r
mean(total_steps_by_day_imputed$total_steps)
```

```
## [1] 10766.19
```


```r
median(total_steps_by_day_imputed$total_steps)
```

```
## [1] 10766.19
```

Mean and median values for days are higher for imputed dataset. It may be caused by imputting by interval to days without observations.

```r
day_difference <- total_steps_by_day_imputed
day_difference$total_steps <- day_difference$total_steps - total_steps_by_day$total_steps

ggplot(day_difference, aes(x = date, y = total_steps)) +
  geom_bar(stat = "identity") +
  xlab("day") + 
  ylab("day_difference for total_steps (imputed data - original data)")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

## Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
imputed_data$wday <- weekdays(as.Date(imputed_data$date))
imputed_data$weekday <- factor(ifelse(imputed_data$wday %in% c("Sunday", "Saturday"), "weekend", "weekday"))
```

### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
avg_steps_int_wd <- imputed_data %>%
  group_by(interval, weekday) %>%
  summarise(mean_steps = mean(steps, na.rm = T))

ggplot(avg_steps_int_wd, aes(x = interval, y = mean_steps)) +
  geom_line() +
  xlab("interval") + 
  ylab("average steps") +
  facet_grid(weekday ~ .)
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14-1.png) 
