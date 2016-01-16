---
output: html_document
---
# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Loading the required package for this data analysis:
```{r}
library(ggplot2)
library(plyr); library(dplyr)
```
The 'activity.csv' dataset I had save into my local hard drive. I read the CSV file from my local drive to R environment and save as 'activity' data frame:
```{r}
activity <- read.csv('D:/DIR_R/activity.csv')
```

The structure of the data set:
```{r}
str(activity)
```
The dimension of the data set:
```{r}
dim(activity)
```
## What is mean total number of steps taken per day?
I use 'complete.cases' to **remove all NA's** before perform descriptive analysis (mean and median).
```{r}
activity <- activity[complete.cases(activity), ]
```

I used plyr package to find the total number of steps taken per day. The value is stored as total_steps.
```{r}
total_steps <- ddply(activity, c("date"), summarise, TotalSteps = sum(steps))
```

The mean and median total number of steps taken per day:
```{r}
mean(total_steps$TotalSteps)
median(total_steps$TotalSteps)
```
Histogram of the total number of steps taken each day:
```{r}
ggplot(total_steps, aes(x = TotalSteps)) +
  geom_histogram() + 
  xlab("Total number of steps taken") +
  ggtitle("Histogram: Total steps taken per day")

```

## What is the average daily activity pattern?

I used plyr package to find the average number of steps taken across all days within 5-minute interval. The value is stored as mean_5m.
```{r}
mean_5m <- ddply(activity, c("interval"), summarise, average = mean(steps))
```
The maximum number of steps, based on average, is:
```{r}
max(mean_5m$average)
```

Below is the time series plot of the 5-minute interval and the average number of steps taken, averaged across all days.
```{r}
ggplot(mean_5m, aes(x = interval,y = average)) +
  geom_line() + xlab("Time interval") +
  ylab("Average steps") + 
  ggtitle("Average number of steps taken, averaged across all days (5-minute interval)")
```

## Imputing missing values

Starting this section, I reload the original data set again because previous section I had removed the NA's in the original data frame. The original data frame is stored as 'activity_2'.
```{r}
activity_2 <- read.csv('D:/DIR_R/activity.csv')
```
Total NA's in the activity data set:
```{r}
sum(is.na(activity_2))
```

The original dataset have some missing value, my strategy for filling in all of the missing values in this dataset is **replace the NA's to 0**. The following code will perform this step:
```{r}
activity_2[is.na(activity_2)] <- 0
```

As usual, I used plyr package to help me calculate the total steps per day and within 5-minute interval:
```{r}
total_steps_2 <- ddply(activity_2, c("date"), summarise, TotalSteps = sum(steps))

mean_5m_2 <- ddply(activity_2, c("interval"), summarise, Mean5m = mean(steps))
```
The mean and median of the data (NA's is replaced with 0).
```{r}
mean(total_steps_2$TotalSteps)
median(total_steps_2$TotalSteps)
```
Here is the histogram of the total number of steps taken each day (NA's is replaced with 0):
```{r}
ggplot(total_steps_2, aes(x = TotalSteps)) +
  geom_histogram() + 
  xlab("Total number of steps taken") +
  ggtitle("Histogram: Total steps taken per day (NA's removed)")
```

## Are there differences in activity patterns between weekdays and weekends?

For this section, I perform some operation on the "date" section to the 'activity_2' dataset.
```{r}
activity_2$date <- as.Date(activity_2$date)

WDays <- c('Monday', 'Tuesday', 
           'Wednesday', 'Thursday', 
           'Friday')

activity_2$WeekDays <-
  factor((weekdays(activity_2$date) %in% WDays),
         levels = c(FALSE, TRUE), 
         labels = c('weekend', 'weekday'))
```
I used dplyr package to help me perform this part of analysis to filter out weekday and weekend on the activity_2 data frame. Later on, I used plyr package to help me compute the mean steps taken, based on 5-minute interval, averaged across all days.
```{r}
data_weekend <- filter(activity_2, 
                       WeekDays == "weekend")
data_weekday <- filter(activity_2, 
                       WeekDays == "weekday")

weekend_sum <- ddply(data_weekend, 
                     c("interval", "WeekDays"),
                     summarise, 
                     TotalSteps = mean(steps))

weekday_sum <- ddply(data_weekday, 
                     c("interval", "WeekDays"),
                     summarise, 
                     TotalSteps = mean(steps))

DF_WeekDay <- rbind(weekday_sum, weekend_sum)
```

Based on the time series plot below, we can see significant difference on the steps, especially at the time interval of 1000-2000.
```{r}
ggplot(DF_WeekDay, aes(x = interval, 
                       y = TotalSteps)) +
  facet_wrap(~WeekDays, nrow = 2, ncol = 1) +
  xlab("Time interval (5-minute)") + 
  ylab("Average steps") + 
  geom_line() + 
  ggtitle("Time series plot: Average steps for each 5 minute interval") 
```