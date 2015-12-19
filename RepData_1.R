##Import Data
data <- read.csv(file ="activity.csv",header = TRUE, sep = ",")
### Explore and clean up data
head(data)
clnData <- na.omit(data)
head(clnData)
#calculate the mean total number of steps taken per day
#first calculate total steps per day
totPerDay <- aggregate(steps ~ date, clnData, sum)
head(totPerDay)
#create histogram of steps per day
hist(totPerDay$steps, main = "Total Steps taken per day", xlab = "Steps per day", col = "blue", breaks = 20)
#calculate mean and median steps per day
mean(totPerDay$steps)
median(totPerDay$steps)

#Calculate the average daily activity pattern
##Create a time series plot
avgPerInt <- aggregate (steps ~ interval, clnData, mean)
head(avgPerInt)
plot (avgPerInt, type = "l", col = "red")
##Find which interval has the highest steps on average across all days
avgPerInt[which(grepl(max(avgPerInt$steps), avgPerInt$steps)), ]

#Imputing missing values
##calculate number of rows that contain NAs
sum(is.na(data$steps))

##Strategy to fill in missing values

## Fill in missing values
newdata <- data
for (i in 1:nrow(newdata)) {
    if (is.na(newdata$steps[i])) {
        newdata$steps[i] <-  avgPerInt$steps[which(grepl(newdata$interval[i], avgPerInt$interval))]
    }
}

## Create a new histogram with the updated values and calculate mean and median

totPerDay2 <- aggregate(steps ~ date, newdata, sum)
head(totPerDay2)
hist(totPerDay2$steps, main = "Total Steps taken per day", xlab = "Steps per day", col = "blue", breaks = 20)
mean(totPerDay2$steps)
median(totPerDay2$steps)

#Calculate differences in activity patterns between weekdays and weekends
##Add a column to distingush between Weekends and Weekdays
WkData <- newdata
WkData$WkDay <- (weekdays(as.Date(WkData$date), abbreviate = FALSE))

for (i in 1:nrow(WkData)) {
    if (WkData$WkDay[i] %in% c("Saturday", "Sunday")) {
        WkData$WkDay[i] <-  "Weekend"
    }else
        WkData$WkDay[i] <- "Weekday"
}

##Create a timeseries plot to show average steps per interval grouped by Weekend/Weekday
library(lattice)

WkAvgPerInt  <- aggregate(WkData$steps, list(as.numeric(WkData$interval),WkData$WkDay),FUN = "mean")
names(WkAvgPerInt) <- c("interval","WkDay", "steps")

xyplot(WkAvgPerInt$steps ~ WkAvgPerInt$interval | WkAvgPerInt$WkDay, 
       layout = c(1, 2), type = "l", 
       xlab = "Interval", ylab = "Avg steps")