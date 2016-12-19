### Loading and preprocessing the data

options(stringsAsFactors = FALSE)
library(ggplot2)
stepData <- read.csv("D:/RDados/Project1_ReproducibleResearch/activity.csv")

stepData$date <- as.Date(stepData$date)

### What is mean total number of steps taken per day?

stepsPerDay <- with(stepData, tapply(steps, date, sum))

ggplot(as.data.frame(stepsPerDay))+
        geom_histogram(aes(stepsPerDay), fill = "green", col="navyblue", binwidth = 1000) + 
        labs(title='Mean total number of steps taken per day')

print(paste("Mean=", round(mean(stepsPerDay, na.rm = TRUE), 1)))
print(paste("Median = ", round(median(stepsPerDay, na.rm = TRUE), 1)))

### What is the average daily activity pattern?

dailyPattern <- with(stepData, tapply(steps, interval, mean, na.rm = TRUE))
dailyPattern <- data.frame(interval = as.integer(names(dailyPattern)), avgSteps = unname(dailyPattern))

qplot(interval, avgSteps, data = dailyPattern, geom = "line") + 
        labs(title = "Average daily activity pattern")
        
print(subset(dailyPattern, avgSteps == max(dailyPattern$avgSteps))$interval)

### Imputing missing values

sum(is.na(stepData$steps))

missingData <- stepData[is.na(stepData$steps), ]

missingData$steps <- dailyPattern$avgSteps[match(missingData$interval, dailyPattern$interval)]

stepDataComplete <- rbind(stepData[!is.na(stepData$steps), ], missingData)

stepsPerDay2 <- with(stepDataComplete, tapply(steps, date, sum, na.rm = TRUE))
ggplot(as.data.frame(stepsPerDay2)) + 
        geom_histogram(aes(stepsPerDay2), fill = "green", col = "navyblue", binwidth = 1000)+
        labs(title='Mean total number of steps taken per day')

print(paste("Mean =", round(mean(stepsPerDay2, na.rm = TRUE), 1)))
print(paste("Median =", round(median(stepsPerDay2, na.rm = TRUE), 1)))
print("The mean has not been altered, because the mean was used when imputing values. The median has been made equal to the mean, because one of the imputed days is now the median value.")

### Are there differences in activity patterns between weekdays and weekends?

xNew$temp <- weekdays(as.Date(xNew$date))
xNew$weekday <- factor(ifelse((xNew$temp) %in% c('Saturday','Sunday'),'Weekend','Weekday'))
xNew$temp <- NULL

stepDataComplete$weekday <- 
        factor(ifelse(weekdays(stepDataComplete$date) %in% c('Saturday', 'Sunday'), 'Weekend', 'Weekday'))

dailyPattern2 <- as.data.frame(with(stepDataComplete, tapply(steps, list(interval, weekday), 
                                                             mean, na.rm = TRUE)))

dailyPattern2 <- data.frame(
        interval = as.numeric(rownames(dailyPattern2)),
        weekday = c(rep("Weekday", length(dailyPattern2)), rep("Weekend", length(dailyPattern2))), 
        avgSteps = as.numeric(c(dailyPattern2$Weekday, dailyPattern2$Weekend))
)

qplot(interval, avgSteps, data = dailyPattern2, geom = "line", facets = weekday ~ .) + 
        labs(title='Weekdays x Weekends')
