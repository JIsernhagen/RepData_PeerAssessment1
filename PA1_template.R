#set up
#"activity.csv" must be loaded in working directory in order for this to function.

#require required packages
require(dplyr)

#1a) Read in the data, process data to render it useful
activity<- read.csv("activity.csv")
activity$interval<-as.factor(activity$interval)

#2a) Calculate the total steps taken per day
#```{r}
by_date <- group_by(activity, date)
activitydaysum <- summarise(by_date, count = n(), stepsum = sum(steps, na.rm = TRUE), stepmean = mean(steps, na.rm = TRUE), stepmedian = median(steps, na.rm = TRUE))
activitydaysum
#```

#2b) Calculate and histogram plot the mean and median of the total number of steps taken each day
par(mfrow = c(1, 2))
with(activitydaysum, {
  hist(stepmean, col="red", xlab = "Mean", main = "Mean of Non-null Steps")
  hist(stepmedian, col="red", xlab = "Median", xlim = c(-2, 2), main = "Median of Non-null Steps")
})

#3a) Make time series plot of 5-minute interval (x-axis) and avg number of steps taken, averaged across all days (y-axis)
#```{r} 
by_interval <- group_by(activity, interval)
activityintervalsum <- summarise(by_interval, count = n(), stepsum = sum(steps, na.rm = TRUE), stepmean = mean(steps, na.rm = TRUE), stepmax = max(steps, na.rm = TRUE))
#```
#```{r} fig.width=7, fig.height=6}
with(activityintervalsum, plot(interval, stepmean, type="1"))
#```

#3b) Which 5-minute interval, on average across all days in the dataset, contains the maximum number of steps?
#```{r}
activityintervalsum<-arrange(activityintervalsum, desc(stepmean))
slice(activityintervalsum, 1)
#```

#4a) Calculate and report the total number of missing values in the dataset
#```{r}
sum(is.na(activity$steps))
#```

#4b) Explain strategy for imputing missing values
#time-of-day and day-of-week effects seem highly pronounced, so the strategy will be to replace nulls with the average for their weekday and time.

#4c) Create new dataframe using this strategy to fill in missing values.
#```{r}
activityfilled <- activity #1) create data frame to fill with imputed step values
activityfilled <- within(activityfilled, date <- as.POSIXct(paste(date))) #2) change date to POXIXct format 
activityfilled <- within(activityfilled, weekday <- weekdays(as.Date(date))) #3) add a day of week variable
activityfilled <- within(activityfilled, weekend <- "weekday") #4) add a weekday/weekend variable, defaulted to weekday
activityfilled$weekend[activityfilled$weekday == "Saturday"] <- "weekend" #5) overwrite "weekend" for Saturdays
activityfilled$weekend[activityfilled$weekday == "Sunday"] <- "weekend" #6) overwrite "weekend" for Sundays
activityfilled <- within(activityfilled, stepsfilled <- steps) #7) fill steps values into the "stepsfilled" column
by_day_interval <- group_by(activityfilled, weekday, interval) #8) create filler table:  group by weekday and interval
filler <- summarise(by_day_interval, stepsfill = mean(steps, na.rm = TRUE)) #9) create filler table:  add mean(steps) by weekday and interval
activityfilled <- inner_join(activityfilled, filler, b = c("weekday","interval")) #10) join data frames
activityfilled$stepsfilled[is.na(activityfilled$stepsfilled)] <- activityfilled$stepsfill[is.na(activityfilled$stepsfilled)] #11) write weekday+interval mean values over nulls
#```

#4e) calculate/report the mean and median total steps taken each day.  Calculate/report difference vs. unimputed values
#```{r}
by_date_new <- group_by(activityfilled, date)
activitydaysumnew <- summarise(by_date_new, count = n(), stepsum = sum(stepsfilled, na.rm = TRUE), stepmean = mean(stepsfilled, na.rm = TRUE), stepmedian = median(stepsfilled, na.rm = TRUE))
activitydaysumnew
#```

#4d) plot histogram of steps taken each day
#```{r}
par(mfrow = c(1, 2))
with(activitydaysumnew, {
  hist(stepmean, col="red", xlab = "Mean", main = "Mean of Non-null Steps")
  hist(stepmedian, col="red", xlab = "Median", xlim = c(-2, 2), main = "Median of Non-null Steps")
})
#```

#5a) add weekend/weekday factor variable to imputed data set (done upstream).

#5b) make a panel plot of time series (type = 1) plot with 5-minute interval on X-axis, avg. steps per weekend/weekday day on y.
#```{r}
weekend<-filter(activityfilled, weekend == "weekend")
weekday<-filter(activityfilled, weekend == "weekday")
by_interval_weekend <- group_by(weekend, interval, weekend)
activityintervalsumday <- summarise(by_interval_weekend, count = n(), stepsum = sum(steps, na.rm = TRUE), stepmean = mean(steps, na.rm = TRUE), stepmax = max(steps, na.rm = TRUE))
by_interval_weekday <- group_by(weekday, interval, weekend)
activityintervalsumend <- summarise(by_interval_weekday, count = n(), stepsum = sum(steps, na.rm = TRUE), stepmean = mean(steps, na.rm = TRUE), stepmax = max(steps, na.rm = TRUE))
par(mfrow = c(1, 2))
  plot(activityintervalsumday$stepmean, col="red", xlab = "Mean", main = "Mean of Impusion-included Weekday Steps")
  plot(activityintervalsumend$stepmean, col="red", xlab = "Mean", main = "Mean of Impusion-included Weekend Steps")
#```




