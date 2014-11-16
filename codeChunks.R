# Load raw data.
dataFile <- "./Data/activity.csv"
rawData <- read.csv(dataFile)

## Preprocess raw data into a data frame called procData by adding a POSIXct dateTime 
procData <- within (rawData, {
    dateTime <- as.POSIXct(sprintf("%s %0.4d", date, interval), format = "%Y-%m-%d %H%M")
})


# Histogram of date weighted by steps.  This gives us a historam of total steps/day.
library(ggplot2)
qplot(date, data = procData, weight = steps) +
    labs(title = "Total Steps per Day", x = "Date", y = "Total Steps") +
    geom_bar(fill = "royalblue") +
    
    # De-clutter the x axis by only showing every 7th date   
    scale_x_discrete(breaks = c("2012-10-01","2012-10-08","2012-10-15","2012-10-22","2012-10-29",
                                "2012-11-05","2012-11-12","2012-11-19","2012-11-26"))

# Calculate total, mean and median number of steps per day.  We can drop NA values.
# Note: median = 0 for most days becasue steps = 0 for > 1/2 of intervals.

library(plyr)
dailyData <- ddply(procData, "date", summarise,
                totalSteps = sum(steps, na.rm = TRUE),
                meanSteps = mean(steps, na.rm = TRUE),
                medianSteps = median(steps, na.rm = TRUE))

# Mean of daily total steps
mean(dailyData$totalSteps, na.rm = TRUE)

# Median of daily total steps
median(dailyData$totalSteps, na.rm = TRUE)


# calculate mean by time interval
require(plyr)
intervalMean <- ddply(procData, "interval", summarise, meanSteps = mean(steps, na.rm = TRUE)) 

# Creat plot of meanSteps ~ interval
require(ggplot2)

ggplot(intervalMean, aes(x = interval, y = meanSteps)) +
    geom_line(color = "royalblue") +
    labs(title = "Average Number of Steps per Time Interval", x = "Interval", y = "Number of Steps")

# Find interval with the maximum average number of steps
maxInterval <- intervalMean[intervalMean$meanSteps == max(intervalMean$meanSteps),]
maxInterval



