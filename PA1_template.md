# Reproducible Research: Peer Assessment 1


Loading and preprocessing the data
 
Load and unzip the dataset (already available in github)

```r
setwd("C:/Users/Elaine Carson/Desktop/RepData")

library(dplyr);library(data.table);library(lattice);library(knitr);library(lubridate)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
## 
## 
## Attaching package: 'data.table'
## 
## The following object is masked from 'package:dplyr':
## 
##     last
## 
## 
## Attaching package: 'lubridate'
## 
## The following objects are masked from 'package:data.table':
## 
##     hour, mday, month, quarter, wday, week, yday, year
```

```r
#Get the dataset (already available in github)
#if (!file.exists("data")) { dir.create("data")}
#fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#download.file(fileUrl, destfile = "./data/activitydata.zip")
#dateDownloaded <- date()
#dateDownloaded
unzip("./data/activitydata.zip") 
data<-read.csv("activity.csv",header=TRUE, sep=",")
data$steps<-as.numeric(data$steps)
data$date<-as.Date(data$date)
```

What is mean total number of steps taken per day?

Group steps by day 

```r
daygroup <- group_by(data, date)
tday <- summarise(group_by(data, date), totalsteps = sum(steps))
days<-as.numeric(c(1:61))
tday<-cbind(tday,days)
```


1. Make a histogram of the total number of steps taken each day. Used a barplot because it showed the bimodal distribution which was the goal.

```r
par(mar = c(5,4,1,1))
barplot(tday$totalsteps,names.arg = c(1:61), xlab='October and November, 2012', ylab='Total Steps', main='Fig 1. Total Steps per Day')
```

![plot of chunk barplot](figure/barplot.png) 

2. Calculate and report the mean and median total number of steps taken per day

```r
meansteps <- mean(tday$totalsteps, na.rm = TRUE)
mediansteps<-median(tday$totalsteps, na.rm = TRUE)
```
The mean total number of steps per day is 1.0766 &times; 10<sup>4</sup>. The median total number of steps per day is 1.0765 &times; 10<sup>4</sup>.

What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

Aggregate data without NA values.

```r
aveSteps <- aggregate(data$steps, list(Interval = data$interval), mean, na.rm=TRUE)
```


```r
par(mar=c(5,4,1,1))
with(aveSteps, { plot(Interval,x,type="l",ylab="Average Steps",xlab = "5- minute Interval",main = "Fig 2. Average Daily Steps per 5-minute Interval" )})
```

![plot of chunk plotavesteps](figure/plotavesteps.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
maxActivity<-(aveSteps[aveSteps$x==(max(aveSteps$x)),])
maxActivity<-data.frame(maxActivity,row.names = NULL)
```

Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
y <-sum(is.na(data$steps))
```
The total number of missing values in the dataset is 2304.

2. Devise a strategy for filling in all of the missing values in the dataset. Will try both mean and median.
mean = 10766    median = 10765  Will use median to replace NA's . 

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
stepsrepMean<-sapply(tday$totalsteps,function(x) ifelse(is.na(x), mean(tday$totalsteps, na.rm = TRUE), x))
stepsrepMedian<-sapply(tday$totalsteps,function(x) ifelse(is.na(x), median(tday$totalsteps, na.rm = TRUE), x))
```


4. Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day. 


```r
tday2<-cbind(tday,stepsrepMean,stepsrepMedian)
barplot(tday2$stepsrepMean,names.arg = c(1:61), xlab='October and November, 2012', ylab='Total Steps', main='Fig. 3 Mean Total Steps per Day',sub='Missing Values Replaced by Mean Steps')
```

![plot of chunk barcomparemeanmeadian](figure/barcomparemeanmeadian1.png) 

```r
barplot(tday2$stepsrepMedian,names.arg = c(1:61), xlab='October and November, 2012', ylab='Total Steps', main='Fig.4 MedianTotal Steps per Day',sub='Missing Values Replaced by Median Steps')
```

![plot of chunk barcomparemeanmeadian](figure/barcomparemeanmeadian2.png) 

Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
comparemeans<-(cat("The mean value of steps after missing values are replaced by the mean in the dataset is",(mean(tday2$stepsrepMean)),". ", "The mean value with NA's removed is",(mean(tday$totalsteps, na.rm = TRUE)),"."))
```

```
## The mean value of steps after missing values are replaced by the mean in the dataset is 10766 .  The mean value with NA's removed is 10766 .
```

```r
comparemedians<-(cat("The median value of steps after missing values are replaced by the median in the dataset is",(median(tday2$stepsrepMedian)),". ", "The median value with NA's removed is",(median(tday$totalsteps, na.rm = TRUE)),".","There is no difference in either value.Statisticians warn that there are other pitfalls in using these replacement methods."))
```

```
## The median value of steps after missing values are replaced by the median in the dataset is 10765 .  The median value with NA's removed is 10765 . There is no difference in either value.Statisticians warn that there are other pitfalls in using these replacement methods.
```
Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
data$dow = as.factor(weekdays(data$date, abbreviate=TRUE)) 
data2<-subset(data,data$dow=="Mon")

data2$d<-gsub("Mon","Weekday",data2$dow)
data3<-subset(data,data$dow=="Tue") 
data3$d<-gsub("Tue","Weekday",data3$dow)
data4<-subset(data,data$dow=="Wed") 
data4$d<-gsub("Wed","Weekday",data4$dow)
data5<-subset(data,data$dow=="Thu") 
data5$d<-gsub("Thu","Weekday",data5$dow)
data6<-subset(data,data$dow=="Fri") 
data6$d<-gsub("Fri","Weekday",data6$dow)
data7<-subset(data,data$dow=="Sat") 
data7$d<-gsub("Sat","Weekend",data7$dow)
data8<-subset(data,data$dow=="Sun") 
data8$d<-gsub("Sun","Weekend",data8$dow)
datawd<-rbind(data2,data3,data4,data5,data6)
datawe<-rbind(data7,data8)           
```
Calculate mean for each interval for weekdays = datawd2 and for weekends = datawe2


```r
datawd2 <-aggregate(datawd$steps, list(Interval = datawd$interval,Work = datawd$d), mean, na.rm=TRUE)
datawe2 <-aggregate(datawe$steps, list(Interval = datawe$interval,Work = datawe$d), mean, na.rm=TRUE)
stepswk<-rbind(datawd2,datawe2)
dwds1<-c(datawd2$x,datawd2$x,datawd2$x,datawd2$x,datawd2$x,datawd2$x,datawd2$x,datawd2$x,datawd2$x)
datawd2<-cbind(data2,dwds1)
data2stepsrep<-sapply(datawd2$steps,function(x) ifelse(is.na(x), datawd2$dwds1, x))
datawd3<-cbind(data3,dwds1)
data3stepsrep<-sapply(datawd3$steps,function(x) ifelse(is.na(x), datawd3$dwds1, x))
datawd4<-cbind(data4,dwds1)
data4stepsrep<-sapply(datawd4$steps,function(x) ifelse(is.na(x), datawd4$dwds1, x))
datawd5<-cbind(data5,dwds1)
data5stepsrep<-sapply(datawd5$steps,function(x) ifelse(is.na(x), datawd5$dwds1, x))
datawd6<-cbind(data6,dwds1)
data6stepsrep<-sapply(datawd6$steps,function(x) ifelse(is.na(x), datawd6$dwds1, x))
repsteps<-c(data2stepsrep,data3stepsrep,data4stepsrep,data5stepsrep,data6stepsrep)
datawday<-cbind(datawd,repsteps)
rs<-rep(datawe2$x,8)
dwes1<-c(datawe2$x,datawe2$x,datawe2$x,datawe2$x,datawe2$x,datawe2$x,datawe2$x,datawe2$x)
datawe7<-cbind(data7,dwes1)
data7stepsrep<-sapply(datawe7$steps,function(x) ifelse(is.na(x), datawe7$dwes1, x))
datawe8<-cbind(data8,dwes1)
wkendsteps<-sapply(datawe8$steps,function(x) ifelse(is.na(x), datawe8$dwes1, x))

repsteps<-c(data7stepsrep,wkendsteps)
datawwkend<-cbind(datawe,repsteps)


WDayStepsbyInterval <- aggregate(datawday$repsteps, list(Interval = datawday$interval), mean, na.rm=FALSE)
wd<- rep("weekday",288)
WDayStepsbyInterval<-cbind(wd,WDayStepsbyInterval)

WkendStepsbyInterval <- aggregate(datawwkend$repsteps, list(Interval = datawwkend$interval), mean, na.rm=FALSE)
wd<- rep("weekend",288)
WkendStepsbyInterval<-cbind(wd,WkendStepsbyInterval)

stepsbyinterval<-rbind(WDayStepsbyInterval, WkendStepsbyInterval)
```
Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 


```r
stepsplot<-transform(stepsbyinterval,Work=factor(wd),AveSteps=as.numeric(x))
```

```r
par(mar = c(5,4,1,1))
xyplot(AveSteps~Interval|Work, data = stepsplot,,ylab="Number of Steps",xlab = "Interval",type="l", main = "Fig 5. Average Daily Steps per 5-minute Interval",layout = c(1,2))
```

![plot of chunk panelplot](figure/panelplot.png) 

