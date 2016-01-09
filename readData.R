require(data.table)
require(lubridate)
require(dplyr)
require(chron)
require(lattice)

###
# Loading and preprocessing the data
##  1. Load the data (i.e. read.csv())
unzip("activity.zip")
dataset  <- "activity.csv"
df <- fread(
  dataset, header = T, na.strings = c("NA")
)

## 2. Process/transform the data (if necessary) into a format suitable for your analysis
df$date     <- ymd(df$date)
df$datetime <- ymd(df$date) + minutes(df$interval)

###
# What is mean total number of steps taken per day?
# For this part of the assignment, you can ignore the missing values in the dataset.
# What is mean total number of steps taken per day?
# 1. Calculate the total number of steps taken per day
daily_summary <- df[complete.cases(df),] %>%
  group_by(date) %>%
  summarise( total_steps = sum(steps))

# 2. Make a histogram of the total number of steps taken each day
hist( daily_summary$total_steps,
      main = "Total number of steps taken each day",
      xlab = "Total number of steps")

# 3. Calculate and report the mean and median of the total number of steps taken per day
mean(daily_summary$total_steps)
median(daily_summary$total_steps)

# Free up memory
rm(daily_summary)

###
# What is the average daily activity pattern?
## 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
interval_summary <- df[complete.cases(df),] %>%
  group_by(interval) %>%
  summarise( avg_steps = mean(steps))

plot(
  interval_summary$interval,
  interval_summary$avg_steps,
  type = "l",
  main = "Average number of steps per 5-min interval",
  xlab = "5 minutes intervals",
  ylab = "Average steps"
)

## 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
max_interval <- filter(interval_summary, avg_steps == max(avg_steps))

# Imputing missing values
# Note that there are a number of days/intervals where there are missing values
# (coded as NA. The presence of missing days may introduce bias into some
# calculations or summaries of the data.
## 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(df$steps))

## 2. Devise a strategy for filling in all of the missing values in the dataset.
## The strategy does not need to be sophisticated. For example, you could use the
## mean/median for that day, or the mean for that 5-minute interval, etc.
## 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
complete_df <- merge(df, interval_summary, by = "interval")
# fill in missing values with interval average
complete_df$steps <-
  ifelse(
    is.na(complete_df$steps),
    round(complete_df$avg_steps),
    complete_df$steps
  )

# remove unneeded column
complete_df <- complete_df[,avg_steps := NULL]
# rearrange column order
setcolorder(complete_df, c("steps", "date", "interval", "datetime"))

## 4. Make a histogram of the total number of steps taken each day...
daily_summary <-
  complete_df    %>%
  group_by(date) %>%
  summarise( total_steps = sum(steps))

hist(
  daily_summary$total_steps,
  main = "Total number of steps taken each day",
  xlab = "Total number of steps"
)

## ...and Calculate and report the mean and median total number of steps taken per day.
mean(daily_summary$total_steps)
median(daily_summary$total_steps)

## Do these values differ from the estimates from the first part of the assignment?
## YES, slightly

## What is the impact of imputing missing data on the estimates of the total daily
## number of steps?
### Missing days are now displayed

# Are there differences in activity patterns between weekdays and weekends?
## 1. Create a new factor variable in the dataset with two levels - "weekday"
## and "weekend" indicating whether a given date is a weekday or weekend day.
complete_df$weekday <-
  factor(
    ifelse(
      is.weekend(complete_df$datetime),
      "weekend",
      "weekday"
    ),
    c("weekday", "weekend")
)

## 2. Make a panel plot containing a time series plot (i.e. type = "l") of the
## 5-minute interval (x-axis) and the average number of steps taken, averaged across
## all weekday days or weekend days (y-axis).
interval_summary <-
  complete_df %>%
  group_by(interval, weekday)    %>%
  summarise( avg_steps = mean(steps))

xyplot(
  avg_steps ~ interval | weekday,
  data = interval_summary,
  pch=19,
  main= "Average number of steps per 5-min interval",
  xlab= "5 minutes intervals",
  ylab= "Average steps",
  layout=c(1,2),
  type=c("l","l")
)