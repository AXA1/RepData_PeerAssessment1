---
title: "Peer-graded Assignment: Course Project 1"
author: "Reproducible Research"
output:
  html_document:
    highlight: haddock
    theme: cerulean
    toc: true
    toc_depth: 4
---
## 1. Load the data

```{r, echo=TRUE}
activity <- read.csv("./activity.csv", stringsAsFactors = FALSE)
str(activity)
```

## 2. Process/transform the data
```{r, echo=TRUE}
activity$date <- as.Date(activity$date)
colSums(is.na(activity))
```

## 3. What is mean total number of steps taken per day?
#### Calculate the total number of steps taken per day

```{r, echo=TRUE}
activity_nomissing <- activity[which(!is.na(activity$steps)),]
aggregate_activity <- tapply(activity_nomissing$steps, activity_nomissing$date, sum)
```

#### Make a histogram of the total number of steps taken each day
```{r, echo=TRUE}
hist(aggregate_activity, 10, main = "Total steps per day", xlab = "", col = "lightblue")
```

#### Calculate and report the mean and median of the total number of steps taken per day
```{r, echo=TRUE}
mean(aggregate_activity)
median(aggregate_activity)
```

#### What is the average daily activity pattern?
```{r, echo=TRUE}
daily_activity<-tapply(activity_nomissing$steps, activity_nomissing$interval, mean)
plot(x = names(daily_activity), y = daily_activity, type = "l", xlab = "5-Minute Intervals", 
     main = "Average daily activity pattern", ylab = "Average number of steps")
daily_activity[daily_activity==max(daily_activity)]
```

## 4. Imputing missing values

```{r, echo=TRUE}
#### Calculate and report the total number of missing values in the dataset
colSums(is.na(activity))
```

#### Devise a strategy for filling in all of the missing values in the dataset

```{r, echo=TRUE}
activity_2 <- activity
activity_2[which(is.na(activity_2$steps)),1] <- daily_activity[as.character(activity_2[which(is.na(activity_2$steps)),3])]
colSums(is.na(activity_2))
```

#### Create a new dataset that is equal to the original dataset but with the missing data filled in

```{r, echo=TRUE}
aggregate_activity_2<-tapply(activity_2$steps, activity_2$date, sum)
hist(aggregate_activity_2, 10, main = "Total steps per day", xlab = "", col = "lightblue")
```

#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r, echo=TRUE}
mean(aggregate_activity_2)
median(aggregate_activity_2)
par(mfrow=c(1,2))
hist(aggregate_activity, 10, main = "Total steps per day", xlab = "", ylim =c(0, 25))
abline(v = median(aggregate_activity), lwd = 3, col="red")
hist(aggregate_activity_2, 10, main = "Total steps per day \n (mean of intervals imputed to \n missing values)", cex.main=1, xlab = "", ylim =c(0, 25))
abline(v = median(aggregate_activity_2), lwd = 3, col="red")
```

## 5. Are there differences in activity patterns between weekdays and weekends?
```{r, echo=TRUE}
activity_2$weekday<-weekdays(activity_2$date); activity_2$Is_weekend<- as.factor(c("weekend", "weekday"))
activity_2[activity_2$weekday == "domenica" | activity_2$weekday == "sabato",5]<- factor("weekend")
activity_2[!(activity_2$weekday == "domenica" | activity_2$weekday == "sabato"),5 ]<- factor("weekday")
```

```{r, echo=TRUE}
head(activity_2, 5)
```

```{r, echo=TRUE}
activity_2_weekend <- subset(activity_2, Is_weekend == "weekend") 
activity_2_weekday <- subset(activity_2, Is_weekend == "weekday") 
daily_activity_weekend<-tapply(activity_2_weekend$steps, activity_2_weekend$interval, mean)
daily_activity_weekday<-tapply(activity_2_weekday$steps, activity_2_weekday$interval, mean)
```

#### New graph
```{r, echo=TRUE}
par(mfrow=c(2,1))
plot(y = daily_activity_weekday, x = names(daily_activity_weekday), type = "l", xlab = "5-Minute Interval", main = "Daily activity pattern - Weekdays", ylab = "Average number of steps", col = 2, ylim =c(0, 250))
plot(y = daily_activity_weekend, x = names(daily_activity_weekend), type = "l", xlab = "5-Minute Interval", main = "Daily activity pattern - Weekends", ylab = "Average number of steps", col = 4, ylim =c(0, 250))
```
