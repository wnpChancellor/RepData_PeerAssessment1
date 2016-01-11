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

```
##          Date Steps
## 1  2012-10-01    NA
## 2  2012-10-02   126
## 3  2012-10-03 11352
## 4  2012-10-04 12116
## 5  2012-10-05 13294
## 6  2012-10-06 15420
## 7  2012-10-07 11015
## 8  2012-10-08    NA
## 9  2012-10-09 12811
## 10 2012-10-10  9900
## 11 2012-10-11 10304
## 12 2012-10-12 17382
## 13 2012-10-13 12426
## 14 2012-10-14 15098
## 15 2012-10-15 10139
## 16 2012-10-16 15084
## 17 2012-10-17 13452
## 18 2012-10-18 10056
## 19 2012-10-19 11829
## 20 2012-10-20 10395
## 21 2012-10-21  8821
## 22 2012-10-22 13460
## 23 2012-10-23  8918
## 24 2012-10-24  8355
## 25 2012-10-25  2492
## 26 2012-10-26  6778
## 27 2012-10-27 10119
## 28 2012-10-28 11458
## 29 2012-10-29  5018
## 30 2012-10-30  9819
## 31 2012-10-31 15414
## 32 2012-11-01    NA
## 33 2012-11-02 10600
## 34 2012-11-03 10571
## 35 2012-11-04    NA
## 36 2012-11-05 10439
## 37 2012-11-06  8334
## 38 2012-11-07 12883
## 39 2012-11-08  3219
## 40 2012-11-09    NA
## 41 2012-11-10    NA
## 42 2012-11-11 12608
## 43 2012-11-12 10765
## 44 2012-11-13  7336
## 45 2012-11-14    NA
## 46 2012-11-15    41
## 47 2012-11-16  5441
## 48 2012-11-17 14339
## 49 2012-11-18 15110
## 50 2012-11-19  8841
## 51 2012-11-20  4472
## 52 2012-11-21 12787
## 53 2012-11-22 20427
## 54 2012-11-23 21194
## 55 2012-11-24 14478
## 56 2012-11-25 11834
## 57 2012-11-26 11162
## 58 2012-11-27 13646
## 59 2012-11-28 10183
## 60 2012-11-29  7047
## 61 2012-11-30    NA
```

## histogram of total steps per day:

```r
hist(steps$Steps,main="Total steps per day",xlab="Steps per day")
```

![plot of chunk step_plot](figure/step_plot-1.png) 

## mean and median of total steps per day:

```r
summary(steps$Steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##      41    8841   10760   10770   13290   21190       8
```
Mean steps=10770
Median steps=10760

# Average daily activity pattern
## time series plot

```r
a<-data
a[is.na(a)]<-0
ave<-aggregate(a$steps,by=list(Int=a$interval),FUN=mean)
plot(ave$Int,ave$x,type="l",main="Time series plot",xlab="5-minute interval",ylab="average number of steps taken")
```

![plot of chunk plot](figure/plot-1.png) 
- Accoding to the time series plot, the 835th 5-min interval contain the maximum average steps, which is 170.13.

# Imputing missing value
## total number of missing value 

```r
nmiss<-sum(is.na(data))
```
There are 2304 NA values in the dataset.

## create new dataset that fills missing data in with mean for that 5-min interval

```r
library(plyr)
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
newdat<-ddply(data, ~ interval, transform, steps = impute.mean(steps))
```

## Histogram of total number of steps per day, mean, and median total number of steps per day

```r
newsteps<-aggregate(newdat$steps,by=list(Date=newdat$date),FUN=sum)
hist(newsteps$x,main="Histogram of the total number of steps taken per day",xlab="steps")
```

![plot of chunk stats](figure/stats-1.png) 

```r
summary(newsteps$x)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10770   10770   12810   21190
```
Mean steps = 10770
Median steps = 10770
