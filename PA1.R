data <- read.csv("./activity.csv")
# convert date format for later use
data$date_c <- as.POSIXlt(data$date)

# calculate the steps per day, removing NA values
stepsPerDayNoNA <- tapply(data$steps, data$date, sum, na.rm = T)
# creat the histogram
hist(stepsPerDayNoNA, main = "Steps per day (ignore NA)", col = "blue", xlab = "Steps per day")
mean(stepsPerDayNoNA)
median(stepsPerDayNoNA)

# calculate the average steps per interval
avgStepsPer5MinNoNA <- tapply(data$steps, data$interval, mean, na.rm = T)
# plot average daily activity
plot(levels(as.factor(data$interval)), avgStepsPer5MinNoNA, type = "l", main = "Average daily activity pattern", xlab = "Interval", ylab = "Average steps per interval")
which(avgStepsPer5MinNoNA == max(avgStepsPer5MinNoNA))

sum(is.na(data$steps))
# create a function for replacing NA with median
replaceNAwithMedian <- function(x){ 
    m <- median(x, na.rm = TRUE) 
    x[is.na(x)] <- m 
    x 
} 
# replicate data into data1
data1 <- data
# apply the replacing function for steps in each interval
data1$steps <- ave(data$steps, data$interval, FUN = replaceNAwithMedian)
stepsPerDayMeanNA <- tapply(data1$steps, data1$date, sum, na.rm = T)
hist(stepsPerDayMeanNA, main = "Steps per day (mean NA)", col = "red", xlab = "Steps per day")
mean(stepsPerDayMeanNA)
median(stepsPerDayMeanNA)

data1$weekdayFactor <- ifelse(data1$date_c$wday == 0 | data1$date_c$wday == 6, "weekend", "weekday")
library(lattice)
xyplot(steps ~ interval | weekdayFactor, data1, type = "a", layout = c(1, 2), xlab = "Interval", ylab = "Number of steps", ylim = c(min(avgStepsPer5MinNoNA) - 10, max(avgStepsPer5MinNoNA) + 10))