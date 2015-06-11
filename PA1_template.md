Assignment 1
--------------------------------------

Loading the pre-processing data


```r
hist <- function() 
{
	data <- read.csv("activity.csv")
	temp <- aggregate(steps ~ date, data, FUN = "sum")
	hist(temp$steps,breaks =9,main="Histogram of total steps per day", xlab = "steps", ylab = "frequency")
	mean <- mean(temp$steps)
	median <- median(temp$steps)
	lines( c(mean,mean), c(0,20), col = "red", lwd = 2)
	lines(c(median, median), c(0, 20), col = "blue", lwd = 3, lty = 22)
	text(18000, 15, paste("Mean =", round(mean,1), "\n Median =",median))
}

hist()
```

```
## Error in hist(temp$steps, breaks = 9, main = "Histogram of total steps per day", : unused arguments (temp$steps, breaks = 9, main = "Histogram of total steps per day", xlab = "steps", ylab = "frequency")
```

What is the mean total number of steps taken per day?


```r
timeseries <- function()
{
	data <- read.csv("activity.csv")
	activity <- aggregate(steps ~ interval, data, FUN = mean)
	sort <- activity[order(-activity[,2]),]
	plot.ts(x= activity$interval, y=activity$steps, type="l", main = "Time-series of steps per 5-min interval averaged across 2 months", xlab = "5-minute invervals", ylab = "Steps")
	max <- sort[[1,1]]
	text(250, 200, paste("Max =", round(max,0), "-", max+5))
}
timeseries()
```

```
## Error in get(as.character(FUN), mode = "function", envir = envir): object 'FUN' of mode 'function' was not found
```

What is the average daily activity pattern?


```r
nas <- function()
{
	data <- read.csv("activity.csv")
	summ <- summary(data)
	summ[[7,1]]
}

nas()
```

```
## [1] "NA's   :2304  "
```

Inputting missing values


```r
replace <- function()
{
	data <- read.csv("activity.csv")
	activity <- aggregate(steps ~ interval, data, FUN = mean)
	merg <- merge(data[,c("steps","interval")], activity, by="interval")
	for (i in 1:nrow(merg))
	{
		if (is.na(merg[[i,2]]))  merg[[i,2]] <- merg[[i,3]]
	}	
	data[,"steps"] <- merg[,2]
	temp1 <- aggregate(steps ~ date, data, FUN = "sum")
	hist(temp1$steps,breaks =9,main="Amended histogram of total steps per day", xlab = "steps", ylab = "frequency")
	mean <- mean(temp1$steps)
	median <- median(temp1$steps)
	lines( c(mean,mean), c(0,20), col = "red", lwd = 2)
	lines(c(median, median), c(0, 20), col = "blue", lwd = 3, lty = 22)
	text(18000, 15, paste("Mean =", round(mean,1), "\n Median =",median))
}

replace()
```

```
## Error in get(as.character(FUN), mode = "function", envir = envir): object 'FUN' of mode 'function' was not found
```

Are there differences in activity patterns between weekdays and weekends?


```r
week <- function()
{
	library(timeDate)
	data <- read.csv("activity.csv")
	activity <- aggregate(steps ~ interval, data, FUN = mean)
	merg <- merge(data[,c("steps","interval")], activity, by="interval")
	for (i in 1:nrow(merg))
	{
		if (is.na(merg[[i,2]]))  merg[[i,2]] <- merg[[i,3]]
	}	
	data[,"steps"] <- merg[,2]
	weekday <- as.data.frame(isWeekday(data$date))
	data <- cbind(data,weekday)
	colnames(data) <- c("steps","date","interval","weekday")
	weekday <- data[data$weekday=="TRUE",]
	weekend <- data[data$weekday=="FALSE",]
	wd_act <- aggregate(steps ~ interval, weekday, FUN = mean)
	we_act <- aggregate(steps ~ interval, weekend, FUN = mean)
	par(mfrow=c(2,1))
	plot.ts(x= wd_act$interval, y=wd_act$steps, type="l",main = "Time-series of steps per 5-min interval on weekdays averaged across 2 months", xlab = "5-minute invervals", ylab = "Steps")
	plot.ts(x= we_act$interval, y=we_act$steps, type="l",main = "Time-series of steps per 5-min interval on weekends averaged across 2 months", xlab = "5-minute invervals", ylab = "Steps")
}

week()
```

```
## Warning: package 'timeDate' was built under R version 3.2.0
```

```
## Error in get(as.character(FUN), mode = "function", envir = envir): object 'FUN' of mode 'function' was not found
```
