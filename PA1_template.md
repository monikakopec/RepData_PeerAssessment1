Reproducible Research: Peer Assessment 1
========================================================



### Loading and preprocessing the data
If we do not have the data, we can download it.

```r
# url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# dest <- "./repdata_data_activity.zip"
# download.file(url, dest)
# unzip(dest)
```

Loading the data (using read.csv()). Missing values are coded as NA. 

```r
data<-read.csv("./activity.csv"
               ,header=TRUE
               ,sep=","
               ,na.strings="NA"
               ,colClasses=c("integer","Date","integer") )
```
The transforming the data into a format suitable for my analysis is not necessary.



### What is mean total number of steps taken per day?

Below you can find a histogram of the total number of steps taken each day

```r
help<-as.data.frame(sapply(split(data$steps, data$date), sum))[,1]
hist(help
     ,main="Histogram of the total number of steps taken each day"
     ,xlab="Total number of steps"
     ,col="coral")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

The code for calculate the mean and median total number of steps taken per day is following:

```r
help_mean<-as.data.frame(sapply(split(data$steps, data$date), mean))[,1]
help_median<-as.data.frame(sapply(split(data$steps, data$date), median))[,1]
```

And the code for report them in graphical form:

```r
par(mfrow=c(2,1))
# row 1 col 1
plot(help_mean
     ,x=unique(data$date)
     ,xlab="Date"
     ,ylab=""
     ,main="Mean total number of steps taken per day"
     ,col="green")
# row 2 col 1
plot(help_median
     ,x=unique(data$date)
     ,xlab="Date"
     ,ylab=""
     ,main="Median total number of steps taken per day"
     ,col="blue")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 




### What is the average daily activity pattern?

At the beginnig I splited the steps into interval and uses fuction sapply() to calculate mean for each of interval.

```r
help_interval<-as.data.frame(sapply(split(data$steps, data$interval), mean, na.rm=TRUE))[,1]
```

Afterthat I made a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).

```r
plot(unique(data$interval)
     ,help_interval
     ,type="l"
     ,main="The average number of steps taken, averaged across all days"
     ,xlab="5-minute interval"
     ,ylab=""
     ,col="brown")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 

The following line of code shows us which 5-minute interval, on average across all the days in the dataset contains the maximum number of steps.

```r
data[which(data$steps==max(data$steps,na.rm=TRUE)),3]
```

```
## [1] 615
```



### Imputing missing values
We can calculate and report the total number of missing values in the dataset using two simple lines.

```r
summary(data)
```

```
##      steps            date               interval   
##  Min.   :  0.0   Min.   :2012-10-01   Min.   :   0  
##  1st Qu.:  0.0   1st Qu.:2012-10-16   1st Qu.: 589  
##  Median :  0.0   Median :2012-10-31   Median :1178  
##  Mean   : 37.4   Mean   :2012-10-31   Mean   :1178  
##  3rd Qu.: 12.0   3rd Qu.:2012-11-15   3rd Qu.:1766  
##  Max.   :806.0   Max.   :2012-11-30   Max.   :2355  
##  NA's   :2304
```

```r
length(data[data$steps=="NA",1])
```

```
## [1] 2304
```
As we see the data has 2304 empty values.

We can fill in these lacks using the mean of all datas for which we know number of steps. Additional I took the floor of the mean, because we fill in the number of steps (it should be integer)

```r
floor(mean(data$steps, na.rm=TRUE))
```

```
## [1] 37
```

I copied the data to data_new and filled in empty values by the floor of mean.

```r
data_new<-data
data_new[is.na(data_new$steps),1]<-floor(mean(data$steps, na.rm=TRUE)) 
```

At the end we can show the histogram of the total number of steps taken each day. For this I created the helpful dataset.

```r
help_new<-as.data.frame(sapply(split(data_new$steps, data_new$date), sum))[,1]
hist(help_new
     ,main="Histogram of the total number of steps taken each day"
     ,xlab="Total number of steps"
     ,col="aquamarine4")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12.png) 

We can compare two dataset using summary().

```r
summary(data$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
##     0.0     0.0     0.0    37.4    12.0   806.0    2304
```

```r
summary(data_new$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##     0.0     0.0     0.0    37.3    37.0   806.0
```
The mean differs a little, because I filled in them the floor of mean. The 3rd Qu is higher, because I filled empty values the floor of mean.






### Are there differences in activity patterns between weekdays and weekends?
I created a new factor variable with two levels � �weekday� and �weekend� indicating whether a given date is a weekday or weekend day and linked this with our data.

```r
part_week<-rep("weekday", length(data_new$date))
for (i in 1:length(data_new$date)) {
  if (weekdays(data_new$date[i]) == "sobota" || weekdays(data_new$date[i]) == "niedziela") {
    part_week[i]<-"weekend"
  }
}
data_new<-cbind(data_new,part_week)
```

To make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis) I created some helpful datasets. I took only rows with "weekend" or "weekday", splited them by interval and calculated mean.

```r
help_weekend<-data_new[data_new$part_week=="weekend",]
help_Weekend_mean<-as.data.frame(sapply(split(help_weekend$steps, help_weekend$interval), mean))[,1]
help_weekday<-data_new[data_new$part_week=="weekday",]
help_Weekday_mean<-as.data.frame(sapply(split(help_weekday$steps, help_weekday$interval), mean))[,1]
```

The following code creates the plot using the basic plotting system.

```r
par(mfrow=c(2,1))
# row 1 col 1
plot(help_Weekend_mean
     ,x=unique(help_weekend$interval)
     ,xlab="Interval"
     ,ylab=""
     ,main="Mean total number of steps taken at weekends"
     ,col="mediumvioletred"
     ,type="l")
# row 2 col 1
plot(help_Weekday_mean
     ,x=unique(help_weekday$interval)
     ,xlab="Interval"
     ,ylab=""
     ,main="Mean total number of steps taken at weekdays"
     ,col="khaki"
     ,type="l")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 