---
title: "Reproducible Research: Peer Assessment 1"
auther: Shiva sai M
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

### Loading and unzippng the data



```r
# graphics library load
library(ggplot2)

# Unzip archive
unzip("activity.zip")

# Read base data into a data frame.
baseData <- read.csv("activity.csv")

# Data Head
head(baseData)
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

### Modifying NA & Date from string class to Date class



```r
# change date type to date class 
baseData$date <- as.Date(as.character(baseData$date))
# create NA logical vector
baseDataNA <- is.na(baseData$steps)
# create clean base for later mean calculations
cleanBase <- baseData[!baseDataNA,]
```


## What is mean total number of steps taken per day?

### 1.Calculate the toal number of steps taken per day


```r
# aggregate clean non NA steps per day (SUM)
SummedDataByDay <- aggregate(baseData$steps, by=list(baseData$date), sum)
# adjust column names
names(SummedDataByDay)[1] ="date"
names(SummedDataByDay)[2] ="totalsteps"
# top 15 of Summed Steps by day
head(SummedDataByDay,15)
```

```
##          date totalsteps
## 1  2012-10-01         NA
## 2  2012-10-02        126
## 3  2012-10-03      11352
## 4  2012-10-04      12116
## 5  2012-10-05      13294
## 6  2012-10-06      15420
## 7  2012-10-07      11015
## 8  2012-10-08         NA
## 9  2012-10-09      12811
## 10 2012-10-10       9900
## 11 2012-10-11      10304
## 12 2012-10-12      17382
## 13 2012-10-13      12426
## 14 2012-10-14      15098
## 15 2012-10-15      10139
```

### 2.histogram for the total number of steps taken each day


```r
# Plot using ggplot
ggplot(SummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->


### 3.Calculating the mean & median of the total number of steps taken per day


```r
# Mean of steps taken per day
mean(SummedDataByDay$totalsteps,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
#median of steps taken per day
median(SummedDataByDay$totalsteps,na.rm=TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

### 1.A time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)


```r
nonNASubset <- baseData[!baseDataNA,]
MeanDataByInterval <- aggregate(nonNASubset$steps, by=list(nonNASubset$interval), mean)
# set the column names
names(MeanDataByInterval)[1] ="interval"
names(MeanDataByInterval)[2] ="steps"

ggplot(MeanDataByInterval, aes(x = interval, y=steps)) +
  labs(title = "Sum of Steps by Interval", x = "5 min interval", y = "steps")+
  geom_line(color="red") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
maxInterval <- MeanDataByInterval[which.max(MeanDataByInterval$steps),]
maxInterval
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
# generate listing of NA's
missingVals <- sum(baseDataNA)
missingVals
```

```
## [1] 2304
```


### 2. Devise a strategy for filling in all of the missing values in the dataset.

#### strategy used : replacing the NA values with the mean interval steps for the corresponding intervals

### 3. Creating a new dataset that is equal to the original dataset but with the missing data filled in.


```r
baseData2 <- baseData
missingData <- is.na(baseData2$steps)
meanVals <- tapply(cleanBase$steps, cleanBase$interval, mean, na.rm=TRUE, simplify=TRUE)
baseData2$steps[missingData] <- meanVals[as.character(baseData2$interval[missingData])]
# original missing data count
sum(missingData)
```

```
## [1] 2304
```

```r
# count of NA values
sum(is.na(baseData2$steps))
```

```
## [1] 0
```


### 4a) Histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.



```r
FullSummedDataByDay <- aggregate(baseData2$steps, by=list(baseData2$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)
```

```
##          date totalsteps
## 1  2012-10-01   10766.19
## 2  2012-10-02     126.00
## 3  2012-10-03   11352.00
## 4  2012-10-04   12116.00
## 5  2012-10-05   13294.00
## 6  2012-10-06   15420.00
## 7  2012-10-07   11015.00
## 8  2012-10-08   10766.19
## 9  2012-10-09   12811.00
## 10 2012-10-10    9900.00
## 11 2012-10-11   10304.00
## 12 2012-10-12   17382.00
## 13 2012-10-13   12426.00
## 14 2012-10-14   15098.00
## 15 2012-10-15   10139.00
```



```r
# Plot using ggplot
ggplot(FullSummedDataByDay, aes(x = totalsteps)) +
  geom_histogram(fill = "steelblue", binwidth=1000) +
  labs(title = "Total Daily Steps", x = "Steps", y = "Frequency")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->


mean of new data



```r
mean(FullSummedDataByDay$totalsteps)
```

```
## [1] 10766.19
```


median of new data 



```r
median(FullSummedDataByDay$totalsteps)
```

```
## [1] 10766.19
```




### 4b) Do these values differ from the estimates from the first part of the assignment?

#### Yes, The mean was unchanged while the meadian increased by 1.19 steps

##### Original mean & median : 10766.19 & 10765
#####      New mean & median : 10766.19 & 10766.19

### 4c) What is the impact of inputing missing data on the estimates of the total daily number of steps?

#### Rplacing the missing data with seems to have pushed the whole data set towards the mean 



## Are there differences in activity patterns between weekdays and weekends?


```r
baseData2$weekday <- weekdays(baseData2$date)
baseData2$weekend <- ifelse (baseData2$weekday == "Saturday" | baseData2$weekday == "Sunday", "Weekend", "Weekday")
#baseData2$weekend <- as.factor(baseData2$weekend)
head(baseData2,5)
```

```
##       steps       date interval weekday weekend
## 1 1.7169811 2012-10-01        0  Monday Weekday
## 2 0.3396226 2012-10-01        5  Monday Weekday
## 3 0.1320755 2012-10-01       10  Monday Weekday
## 4 0.1509434 2012-10-01       15  Monday Weekday
## 5 0.0754717 2012-10-01       20  Monday Weekday
```

```r
MeanDataWeekendWeekday <- aggregate(baseData2$steps, by=list(baseData2$weekend, baseData2$interval), mean)
names(MeanDataWeekendWeekday)[1] ="weekend"
names(MeanDataWeekendWeekday)[2] ="interval"
names(MeanDataWeekendWeekday)[3] ="steps"

ggplot(MeanDataWeekendWeekday, aes(x = interval, y=steps, color=weekend)) +
  geom_line() +
  facet_grid(weekend ~ .) +
  labs(title = "Mean of Steps by Interval", x = "interval", y = "steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

#### The earlier stages of the day seems to have more number of steps on week days most likely because these are usually the working hours.
#### The weekends on the other hand have the steps fairly spread across the whole day and also seem to have more number of steps than weekdays.
