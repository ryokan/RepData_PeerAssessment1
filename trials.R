## Loading and preprocessing the data

setwd("~/Dropbox/Projects/DS-Reproducible/RepData_PeerAssessment1")
activity <- read.csv("activity.csv")

## What is mean total number of steps taken per day?

steps.sum <- aggregate(steps ~ date, data = activity, FUN = sum)
steps.sum.mean <- mean(steps.sum$steps)
steps.sum.median <- median(steps.sum$steps)

## What is the average daily activity pattern?

steps.pattern <- aggregate(steps ~ interval, data = activity, FUN = mean)
plot(steps.pattern$interval %/% 100 + (steps.pattern$interval %% 100)/60, steps.pattern$steps, type='l')

steps.max = steps.pattern$interval[ which.max(steps.pattern$steps) ]

## Imputing missing values

# 1. Calculate and report the total number of missing values in th

missing <- length (activity[is.na(activity$steps), 1])

# 2. Devise a strategy for filling in all of the missing values in the dataset. 
# -> Use mean of the inverval

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

activity.filled <- merge(activity, steps.pattern, by = "interval", all.x = TRUE)
# x activity.filled <- apply(activity.filled, 1, function (x){if (is.na(x[2])) x[2] <- x[4] })
for (i in 1:dim(activity.filled)[1]) {
  if (is.na(activity.filled$steps.x[i])) activity.filled$steps.x[i] <- activity.filled$steps.y[i]
}
activity.filled <- activity.filled[ order(activity.filled$date, activity.filled$interval), c(1:3)]
colnames (activity.filled) [2] <- "steps"

# 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

steps.filled.sum <- aggregate(steps ~ date, data = activity.filled, FUN = sum)
steps.filled.sum.mean <- mean(steps.filled.sum$steps)
steps.filled.sum.median <- median(steps.filled.sum$steps)

hist(steps.filled.sum$steps, xlab = "steps", ylab = "days", main = "Steps per day")

## Are there differences in activity patterns between weekdays and weekends?

Sys.setlocale(category="LC_ALL", "en_US")
activity.filled$dayclass <- "weekday"
activity.filled$dayclass <- ifelse ((weekdays(as.Date(activity.filled$date)) == "Sunday" |
                                       weekdays(as.Date(activity.filled$date)) == "Saturday"), 
                                    "weekend", "weekday")
steps.pattern.weekday <- aggregate(steps ~ interval, 
                                   data = activity.filled[activity.filled$dayclass == "weekday",], 
                                   FUN = mean)
steps.pattern.weekend <- aggregate(steps ~ interval, 
                                   data = activity.filled[activity.filled$dayclass == "weekend",], 
                                   FUN = mean)
par(mfrow = c(2, 1))
plot(x = steps.pattern.weekday$interval %/% 100 + (steps.pattern.weekday$interval %% 100)/60, 
     y = steps.pattern.weekday$steps, 
     type='l', xlab = "hours", ylab = "steps", main = "steps per 5 minutes in a day (weekday)")
plot(x = steps.pattern.weekend$interval %/% 100 + (steps.pattern.weekend$interval %% 100)/60, 
     y = steps.pattern.weekend$steps, 
     type='l', xlab = "hours", ylab = "steps", main = "steps per 5 minutes in a day (weekend)")




