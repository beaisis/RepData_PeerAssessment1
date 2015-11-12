## Load Libraries
library(ggplot2)
library(dplyr)
library(lubridate)

#Loading and preprocessing the data
ActivityData <- read.csv("./repdata-data-activity/activity.csv")

#date into dateformat:
ActivityData$date <- ymd(ActivityData$date)

# Calculate the total number of steps taken per day 
StepsByDate <- aggregate(ActivityData$steps, by = list(ActivityData$date), FUN=sum, na.rm=TRUE)
colnames(StepsByDate) <- c("date", "steps")

head(StepsByDate)

# Histogram of the total number of steps taken each day
ggplot(StepsByDate, aes(x=steps)) +
  geom_histogram(binwidth= 1000) +
  labs(title="Steps Taken per Day Histogram", 
       x = "Steps per Day", y = "Frequency") + theme_bw() 

# Mean and median of the total number of steps taken per day
mean(StepsByDate$steps)
median(StepsByDate$steps)


#What is the average daily activity pattern?
------------------------------------------------------------------------
AverageStepsByInterval <- aggregate(ActivityData$steps, by=list(ActivityData$interval),
                      FUN=mean, na.rm=TRUE)
colnames(AverageStepsByInterval) <- c("interval", "AvgSteps")

#Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
ggplot(AverageStepsByInterval, aes(x=interval, y=AvgSteps)) + 
geom_line() + labs(title="Average Steps by Interval", x="Interval", y="Average Steps Taken Across All Days")

# 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps
AverageStepsByInterval[which.max(AverageStepsByInterval$AvgSteps),]


#Imputing missing values

#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(ActivityData$steps))

#Strategy for filling in all of the missing values in the dataset. 
IndexWithNas <- which(is.na(ActivityData$steps))

# Create a new dataset that is equal to the original dataset but with the missing data filled in using
#the mean for that 5-minute interval

ActivityDataWithImputtedValues <- ActivityData

for (i in 1:NROW(IndexWithNas)) {
      IntervalNeeded <- ActivityDataWithImputtedValues$interval[IndexWithNas[i]]
      AvgStepsForInterval <- AverageStepsByInterval$AvgSteps[AverageStepsByInterval$interval == IntervalNeeded]
      ActivityDataWithImputtedValues$steps[IndexWithNas[i]] <- AvgStepsForInterval
      }

head(ActivityDataWithImputtedValues)
sum(is.na(ActivityDataWithImputtedValues$steps))



# Make a histogram of the total number of steps taken each day and Calculate and report the mean and 
#median total number of steps taken per day. Do these values differ from the estimates
#from the first part of the assignment? What is the impact of imputing missing data on the 
# estimates of the total daily number of steps?

StepsByDateFilled <- aggregate(ActivityDataWithImputtedValues$steps, by=list(ActivityDataWithImputtedValues$date),FUN=sum, na.rm=TRUE)
colnames(StepsByDateFilled) <- c("date", "steps")

ggplot(StepsByDateFilled, aes(x=steps)) +
      geom_histogram(binwidth= 1000) +
      labs(title="Steps Taken per Day Histogram", 
           x = "Steps per Day", y = "Frequency") + theme_bw() 

mean(StepsByDateFilled$steps)
median(StepsByDateFilled$steps)


#Are there differences in activity patterns between weekdays and weekends?

#Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#indicating whether a given date is a weekday or weekend day.

ActivityDataWithImputtedValues$Weekday <- ifelse(weekdays(as.Date(ActivityDataWithImputtedValues$date)) %in% c("Saturday","Sunday"), "weekend","weekday")
table(ActivityDataWithImputtedValues$Weekday)

head(ActivityDataWithImputtedValues)

ActivityDataWithImputtedValuesWeekend <- aggregate(ActivityDataWithImputtedValues$steps, by=list(ActivityDataWithImputtedValues$Weekday,
                                                                                                 ActivityDataWithImputtedValues$interval),
                                          FUN=mean, na.rm=TRUE)

#Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#and the average number of steps taken, averaged across all weekday days 
#or weekend days (y-axis). See the README file in the GitHub repository to see a
#n example of what this plot should look like using simulated data.

head(ActivityDataWithImputtedValuesWeekend)
colnames(ActivityDataWithImputtedValuesWeekend) <- c("WeekdayType", "interval", "steps")

ggplot(ActivityDataWithImputtedValuesWeekend, aes(x=interval, y=steps)) +
      facet_wrap(~ WeekdayType, nrow=2, ncol=1) + 
      geom_line()

