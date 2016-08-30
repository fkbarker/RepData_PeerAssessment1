# Reproducable Anaylsis Assignment 1
Filita Barker  
29 August 2016  



## Loading and processing data

The data is unzipped and loaded into R. It is then processed for analysis


```r
setwd("~/Education/DataScience/5 Reproducible Research/Week 2")
unzip("repdata_data_activity.zip")
activity<-read.csv("activity.csv")
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
## What is mean total number of steps taken per day?
This section looks at the number of steps per day and show's them in a histogram. To answer the following questions
For this part of the assignment, you can ignore the missing values in the dataset.

1.Calculate the total number of steps taken per day
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
3. Calculate and report the mean and median of the total number of steps taken per day



```r
actdate<-transform(activity,date=factor(date))
stepday<-with(actdate,tapply(steps,date,sum,na.rm=T))
hist(stepday, breaks =10)
```

![](Assignment_1_files/figure-html/stepday-1.png)<!-- -->


```r
meanstep<- round(mean(stepday))
medianstep<-round(median(stepday))
```

The mean of total steps each day is 9354 and the median is 10395.

## What is the average daily activity pattern?
This section plots a time series plot to answer the following questions
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
actInt<-transform(activity,interval=factor(interval))
AvgInt<-with(actInt,tapply(steps,interval,mean,na.rm=T))
uAvgInt<-unname(AvgInt)
plot(names(AvgInt),uAvgInt, type= 'l', xlab = 'Interval', ylab='steps' )
title('Average steps over the intervals of the day')
```

![](Assignment_1_files/figure-html/Interval-1.png)<!-- -->

```r
maxInt<-names(which.max(AvgInt))
```
The maximum interval is at 835.

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
NaRows<-sum(is.na(activity$steps))
```
The total number of Missing values in the dataset is 2304.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

I have chosen to use the mean for that 5-minute interval to fill in the missing data.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
actmiss<-activity
i<-1
for (i in 1: length(actmiss$steps)){
    if (is.na(actmiss$steps[i])) {
        intv<- actInt$interval[i]
        repVal<- AvgInt[intv]
        actmiss$steps[i]<-repVal
    }
    i<-i+1
}
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
actdate2<-transform(actmiss,date=factor(date))
stepday2<-with(actdate2,tapply(steps,date,sum,na.rm=T))
hist(stepday2, breaks =10)
```

![](Assignment_1_files/figure-html/stepdayagain-1.png)<!-- -->


```r
meanstep2<- round(mean(stepday2))
medianstep2<-round(median(stepday2))
```

The mean of total steps each day is 9354 before the missing values were added and 10766 after and the median is 10395 before the missing values were added and 10766.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
actweek<-cbind(actmiss, c(1:length(actmiss$date)))
names(actweek)[4]<-"weekday"
w<-1
for (w in 1:length(actweek$date)){
    tempday<-weekdays(as.POSIXct(actmiss$date[w]))
    if (tempday %in% c("Saturday" , "Sunday")){
        actweek$weekday[w]<- "Weekend"
    } else {
        actweek$weekday[w]<- "Weekday"
    }
    w<-w+1
}
actweek2<-transform(actweek, weekday = as.factor(weekday))
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
subday<-subset(actweek, weekday %in% c("Weekday"),select = c(steps,interval))
subend<-subset(actweek, weekday %in% c("Weekend"),select = c(steps,interval))
Avgday<-with(subday,tapply(steps,interval,mean,na.rm=T))
Avgend<-with(subend,tapply(steps,interval,mean,na.rm=T))
par(mfrow=c(2,1))
par(cex = 0.6)
par(mar = c(3,3 ,1,1 ), oma = c(1, 1,1,1))
plot(names(Avgday),unname(Avgday), type= 'l', xlab = 'Interval', ylab='steps',main = "weekday" )
plot(names(Avgend),unname(Avgend), type= 'l', xlab = 'Interval', ylab='steps', main = "weekend")
```

![](Assignment_1_files/figure-html/panelplot-1.png)<!-- -->

