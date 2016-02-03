---
title: "PA1_template.Rmd"
output: html_document
---

# Load data and transform:


```r
data<-read.csv("/Users/jingshi/desktop/Coursera/activity.csv",sep=",",na.strings="NA")
steps<-aggregate(data$steps,by=list(Date=data$date),FUN=sum)
colnames(steps)<-c("Date","Steps")
```

# Mean total number of steps taken per day:
## total steps per day:

```r
print (steps)
```

## histogram of total steps per day:

```r
png("/Users/jingshi/desktop/Coursera/hist1.png",width=480,height=480)
hist(steps$Steps,main="Total steps per day",xlab="Steps per day")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

## mean and median of total steps per day:

```r
summary(steps$Steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```
## Mean steps=10770
## Median steps=10760

# Average daily activity pattern
## time series plot

```r
a<-data
a[is.na(a)]<-0
ave<-aggregate(a$steps,by=list(Int=a$interval),FUN=mean)
png("/Users/jingshi/desktop/Coursera/timeseriesplot.png",width=480,height=480)
plot(ave$Int,ave$x,type="l",main="Time series plot",xlab="5-minute interval",ylab="average number of steps taken")
dev.off()
```

```
## quartz_off_screen 
##                 2
```
## - Accoding to the time series plot, the 835th 5-min interval contain the maximum average steps, which is 170.13.

# Imputing missing value
## total number of missing value 

```r
nmiss<-sum(is.na(data))
```
## There are 2304 NA values in the dataset.

## create new dataset that fills missing data in with mean for that 5-min interval

```r
library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
newdat<-ddply(data, ~ interval, transform, steps = impute.mean(steps))
```

## Histogram of total number of steps per day, mean, and median total number of steps per day

```r
newsteps<-aggregate(newdat$steps,by=list(Date=newdat$date),FUN=sum)
png("/Users/jingshi/desktop/Coursera/hist2.png",width=480,height=480)
hist(newsteps$x,main="Histogram of the total number of steps taken per day",xlab="steps")
dev.off()
```

```
## quartz_off_screen 
##                 2
```

```r
summary(newsteps$x)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```
## Mean steps = 10770
## Median steps = 10770

# Differences in activity patterns between weekdays and weekends;
## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day;

```r
newdat$weekday <- weekdays(as.Date(newdat$date))
WeekendDays <- c("Saturday", "Sunday")
newdat$daytype <- as.factor( ifelse(newdat$weekday %in% WeekendDays, "weekend", "weekday") )
```

## panel plot;

```r
a <- with( newdat, aggregate( steps ~ interval + daytype, FUN = mean, na.rm = TRUE ))
png("/Users/jingshi/desktop/Coursera/panelplot1.png",width=480,height=480)
library(ggplot2)
ggplot(data = a , aes(x = interval, y = steps, colour = daytype)) + geom_line(stat = "identity") + ggtitle("Mean steps per 5 minutes interval in weekdays and weekends" ) + facet_grid(daytype ~ .)
dev.off()
```

```
## quartz_off_screen 
##                 2
```
