# Reproducible Research: Peer Assessment 1

Reproducible Research Peer Assessment Assignment - Week #2

## Loading and preprocessing the data

The raw data is pre-processed to allow easy analysis and address questions about the data.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
#Read data from Dataset: Activity monitoring data
adata <- read.csv("./activity.csv")
# # Convert date variable from factor to date, and remove NAs
adata1<- na.omit(adata)
adata1$date <- as.Date(adata1$date)
# Group data by date, and summarize the sum of steps
adata_byday <- group_by(adata1, date)
steps_by_day <- summarise(adata_byday, total = sum(steps))
```

## What is mean total number of steps taken per day?


```r
library(ggplot2)
#=================================
## 2.Histogram of the total number of steps taken each day
#=================================

ggplot(steps_by_day,aes(x=date,y=total),type = "l")+geom_bar(stat="identity") + ylab("Daily Steps") + xlab("Date") + ggtitle("Histogram of total number of steps taken each day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->


## What is the average daily activity pattern?


```r
#=================================
## 3. Mean and median number of steps taken each day
#=================================
meansteps <- mean(steps_by_day$total)
mediansteps <- median(steps_by_day$total)

print (meansteps)
```

```
## [1] 10766.19
```

```r
print (mediansteps)
```
```
## [1] 10765
```

```r
#=================================
## 4. Time series plot of the average number of steps taken
#=================================

avgadata1 <- aggregate(steps ~ interval, adata, mean)
# create a time series plot 
plot(avgadata1$interval, avgadata1$steps, type='l',col=2, 
     main="Average number of steps over all days", xlab="Interval", 
     ylab="Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


```r
#=================================
## 5.The 5-minute interval that, on average, contains the maximum number of steps 
#=================================

max_row<-which.max(avgadata1$steps)
print (avgadata1 [max_row,])
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

```r
#=================================
## 6.Code to describe and show a strategy for imputing missing data 
#=================================


#Visual representation can be obtained using the VIM package as follows

library(VIM)
```

```
## Loading required package: colorspace
```

```
## Loading required package: grid
```

```
## Loading required package: data.table
```

```
## -------------------------------------------------------------------------
```

```
## data.table + dplyr code now lives in dtplyr.
## Please library(dtplyr)!
```

```
## -------------------------------------------------------------------------
```

```
## 
## Attaching package: 'data.table'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     between, first, last
```

```
## VIM is ready to use. 
##  Since version 4.0.0 the GUI is in its own package VIMGUI.
## 
##           Please use the package to use the new (and old) GUI.
```

```
## Suggestions and bug-reports can be submitted at: https://github.com/alexkowa/VIM/issues
```

```
## 
## Attaching package: 'VIM'
```

```
## The following object is masked from 'package:datasets':
## 
##     sleep
```

```r
aggr_plot <- aggr(adata, col=c('blue','red'), numbers=TRUE, sortVars=TRUE, labels=names(adata), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```
## 
##  Variables sorted by number of missings: 
##  Variable     Count
##     steps 0.1311475
##      date 0.0000000
##  interval 0.0000000
```
 


```r
# Find the NA positions
na1 <- which(is.na(adata$steps))

# Create a vector of means
mean_vec <- rep(mean(adata$steps, na.rm=TRUE), times=length(na1))
# Replace the NAs by the means
adata[na1, "steps"] <- mean_vec

# Compute the total number of steps each day (NA values removed)
sum_data <- aggregate(adata$steps, by=list(adata$date), sum)
#=================================
## 7.Histogram of the total number of steps taken each day after missing values are imputed
#=================================

# Rename the attributes and create histogram
names(sum_data) <- c("date", "total")
sum_data$date <- as.Date(sum_data$date)
ggplot(sum_data,aes(x=date,y=total))+geom_bar(stat="identity") + ylab("Daily Steps") + xlab("Date") + ggtitle("Histogram of total number of steps taken each day (NA replaced by mean value)")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

 

The sum_data data frame can be used to calculate the mean and median number of steps per day as follows:

```r
mean(sum_data$total)
```

```
## [1] 10766.19
```
```r
median(sum_data$total)
```

```
## [1] 10766.19
```
Mean and median values are higher after imputing missing data. The reason is that in the original data, there are some days with steps values NA for any interval


## Are there differences in activity patterns between weekdays and weekends?

```r
#=================================
## 8.Histogram of the total number of steps taken each day after missing values are imputed
#=================================
weekday.or.weekend <- function(date) {
        day <- weekdays(date)
        if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
                return("weekday") else if (day %in% c("Saturday", "Sunday")) 
                        return("weekend") else stop("invalid date")
}
adata$date <- as.Date(adata$date)
adata$day <- sapply(adata$date, FUN = weekday.or.weekend)

#Create plot containing plots of average number of steps taken on weekdays and weekends.
averages <- aggregate(steps ~ interval + day, data = adata, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
        xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
 
