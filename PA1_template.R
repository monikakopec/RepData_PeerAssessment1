# wrkdrc<-"C:/Users/mkopec/Desktop/COURSERA/5 Reproducible Research/pa1"
# setwd(wrkdrc)

###### 0. Downloading and unzipping data
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
dest <- "./repdata_data_activity.zip"
download.file(url, dest)
unzip(dest)

list.files("./") 
# dir()




###### Loading and preprocessing the data
# Show any code that is needed to
# 1. Load the data (i.e. read.csv())
data<-read.csv("./activity.csv", header=TRUE, sep=",", na.strings="NA", colClasses=c("integer","Date","integer") )
# 2. Process/transform the data (if necessary) into a format suitable for your analysis
data_steps <- aggregate(steps ~ date, data = data, sum, na.rm = TRUE)
data_interval <- aggregate(steps ~ interval, data = data, mean, na.rm = TRUE)

###### What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# 1. Make a histogram of the total number of steps taken each day
hist(data_steps$steps
     ,main="Histogram of the total number of steps taken each day"
     ,xlab="Total number of steps"
     ,col="coral")

# 2. Calculate and report the mean and median total number of steps taken per day
mean(data_steps$steps)
median(data_steps$steps)




###### What is the average daily activity pattern?
# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#    and the average number of steps taken, averaged across all days (y-axis)
graphics.off()
plot(data_interval$interval
     ,data_interval$steps
     ,type="l"
     ,main="The average number of steps taken, averaged across all days"
     ,xlab="5-minute interval"
     ,ylab=""
     ,col="brown")

# 2. Which 5-minute interval, on average across all the days in the dataset, 
#    contains the maximum number of steps?
data_interval[which.max(data_interval$steps), ]$interval



###### Imputing missing values
# Note that there are a number of days/intervals where there are missing values (coded as NA). 
# The presence of missing days may introduce bias into some calculations or summaries 
# of the data.
# 1. Calculate and report the total number of missing values in the dataset 
#    (i.e. the total number of rows with NAs)
summary(data)
length(data[data$steps=="NA",1])
sum(is.na(data$steps))

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
#    The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, 
#    or the mean for that 5-minute interval, etc.
floor(mean(data$steps, na.rm=TRUE)) #the floor of the mean 

# 3. Create a new dataset that is equal to the original dataset but with the missing data 
#    filled in.
data_new<-data
data_new[is.na(data_new$steps),1]<-floor(mean(data$steps, na.rm=TRUE)) 

# 4. Make a histogram of the total number of steps taken each day and Calculate and 
#    report the mean and median total number of steps taken per day. Do these values 
#    differ from the estimates from the first part of the assignment? What is 
#    the impact of imputing missing data on the estimates of the total daily number of steps?
# historgram
data_steps_new <- aggregate(steps ~ date, data = data_new, sum, na.rm = TRUE)
hist(data_steps_new$steps
     ,main="Histogram of the total number of steps taken each day"
     ,xlab="Total number of steps"
     ,col="aquamarine4")
# mean and median
mean(data_steps_new$steps)
median(data_steps_new$steps)





###### Are there differences in activity patterns between weekdays and weekends?
# For this part the weekdays() function may be of some help here. 
# Use the dataset with the filled-in missing values for this part.
# 1. Create a new factor variable in the dataset with two levels – “weekday” and 
#    “weekend” indicating whether a given date is a weekday or weekend day.
part_week<-rep("weekday", length(data_new$date))
for (i in 1:length(data_new$date)) {
  if (weekdays(data_new$date[i]) == "sobota" || weekdays(data_new$date[i]) == "niedziela") {
    part_week[i]<-"weekend"
  }
}
data_new<-cbind(data_new,part_week)

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute 
#    interval (x-axis) and the average number of steps taken, averaged across 
#    all weekday days or weekend days (y-axis). See the README file in the GitHub 
#    repository to see an example of what this plot should look like using simulated data.
steps = aggregate(steps ~ interval + part_week, data_new, mean)
steps_weekend = steps[steps$part_week=="weekend",]
steps_weekday = steps[steps$part_week=="weekday",]

par(mfrow=c(2,1))
# row 1 col 1
plot(steps_weekend$steps
     ,x=steps_weekend$interval
     ,xlab="Interval"
     ,ylab=""
     ,main="Mean total number of steps taken at weekends"
     ,col="mediumvioletred"
     ,type="l")
# row 2 col 1
plot(steps_weekday$steps
     ,x=steps_weekday$interval
     ,xlab="Interval"
     ,ylab=""
     ,main="Mean total number of steps taken at weekdays"
     ,col="khaki"
     ,type="l")