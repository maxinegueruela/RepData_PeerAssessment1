
library(dplyr)
library(ggplot2)



#Donwload data

fileurl = 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
if (!file.exists('./repdata_data_activity')){
  download.file(fileurl,'./repdata_data_activity', mode = 'wb')
  unzip("repdata_data_activity", exdir = getwd())
}

#Read data from working directory

activity<-read.csv("activity.csv", na.strings = "NA")

#Remove the NAs

activity_complete <- activity[complete.cases(activity),]

mean(activity$steps, na.rm =TRUE)

#Calculate the total number of steps per day

steps_day <- aggregate(steps~date, activity_complete,sum)

#Plot histogram of steps per day
ggplot(steps_day, aes(x=steps)) + geom_histogram(bins=5)

#Mean of total of steps taken per day
mean_steps <- round(mean(steps_day$steps))

#Median of total of steps taken per day
median_steps <- round(median(steps_day$steps))

#Calculate the mean steps per interval
steps_interval <- aggregate(steps ~ interval, activity_complete, mean)

#Plot time series plot (i.e. type = "l"\color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
ggplot(data=steps_interval, aes(y=steps, x=interval)) + geom_line(colour="green")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval <- which.max(steps_interval$steps)
steps_interval[max_interval,]


#Imputing missing values. Calculate the number of rows with missing values in steps column

missing_steps_values <- activity[!complete.cases(activity),]
nrow(missing_steps_values)

#Create new data with NA replaced

activity_replacedNA <- activity

for (i in 1:nrow(activity_replacedNA)) {
  if(is.na(activity_replacedNA$steps[i])) {
    avg_value <- steps_interval$steps[which(steps_interval$interval == activity_replacedNA$interval[i])]
    activity_replacedNA$steps[i] <- avg_value
  }
}


steps_day_filledNA <- aggregate(steps~date, activity_replacedNA,sum)

#Plot histogram of steps per day
ggplot(steps_day_filledNA, aes(x=steps)) + geom_histogram(bins=5, fill="brown") + ggtitle("Histogram")

#Mean of total of steps taken per day
round(mean(steps_day_filledNA$steps))

#Median of total of steps taken per day
round(median(steps_day_filledNA$steps))

#replace days of weekday and weekend

day <- weekdays(as.Date(activity_complete$date))
day_factor <- vector()
for (i in 1:nrow(activity_complete)){
    if (day[i] == "sábado"){
       day_factor[i] == "weekend"
    }
    else if (day[i] == "domingo"){
        day_factor[i] == "weekend"
    }
    else {
        day_factor[i] == "weekday"
    }
}
activity_complete$weekday <- "dato"
activity_complete$weekday <- ifelse(weekdays(as.Date(activity_complete$date))
                                    %in% c("sábado", "domingo"), 
                              "weekend", "weekdays")

steps_day_week <- aggregate(steps ~ interval + weekday, data = activity_complete, mean)

#Plot 

ggplot(steps_day_week, aes(interval, steps)) +
  geom_line(stat = "identity", aes(colour = weekday)) +
  facet_grid(weekday ~ ., scales="fixed", space="fixed") +
  labs(x="Interval", y=expression("No of Steps")) +
  ggtitle("No of steps Per Interval by day type")