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

# Find the number of NA values for the steps variable
numNA <- sum(is.na(procData$steps))
numNA

# Impute missing values by breaking the date frame procData into 2 data frames,
# One containing complete observations (dataComp)and on containing NA
# values for steps (dataNA).  We will then merg the dataNA with intervalMean
# and then row bind the merged data frame to dataComp.
library(stats)

# Split data frame
dataComp <- procData[complete.cases(procData),]
dataNA <- procData[!complete.cases(procData),]

#  Megere data frames
dataImputed <- merge(dataNA, intervalMean)

# Round the the mean values so they are integers and assign the value to steps 
dataImputed$steps <- round(dataImputed$meanSteps)

# Drop the meanSteps column
dataImputed <- dataImputed[, -5]

# Put the data frames back together with rbind
dataImputed <- rbind(dataComp, dataImputed)

# Re-order the data frame so it's back in the original order using plyr arrange
dataImputed <- arrange(dataImputed, dateTime)


# Add 2 level factor columen to imputed data set designating whether the day is 
# a weekday or a weekend  

# Function takes a POSIXct vector and retruns a charater vecttor of the same lenght 
# designating the date as a "weekend" or a "weekday"
getDayType <- function(x){
    xOut <- character(length(x))
    for (i in 1:length(x)) {
        if(grepl("^S", weekdays(x[i]))) xOut[i] <- "weekend" else xOut[i] <- "weekday"
    }
    xOut
}

# Add dayType columne to the imputed data set
dayType <- getDayType(dataImputed$dateTime)
dataImputed <- cbind(dataImputed, dayType)

# Split data frame on interval and dayType, take the mean of the intervals
# and unsplit the data frame usning dplyr.library(plyr)
meansByDayType <- ddply(dataImputed, c("dayType", "interval"), summarise,
                   meanSteps = mean(steps))

# Create panel plot of average total steps per time period by day type
library(ggplot2)
qplot(interval, meanSteps, data = meansByDayType, geom = "line", facets = dayType~. ) +
    geom_line(color = "royalblue")  + 
    labs(x = "Interval", y ="Number of Steps")

ggplot(meansByDayType, aes(x = interval, y = meanSteps, facets = dayType~. )) +
    geom_line(color = "royalblue")                   

