---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
>Including Libraries

```r
library("ggplot2")
library("fields")
```
>Reading Data

```r
unzip("Activity.zip")
data <- read.csv("activity.csv")
```
## What is mean total number of steps taken per day?
>Preparing Data

```r
sumvec <- rowsum(data$steps, data$date)
new.df <- cbind.data.frame(rownames(sumvec), sumvec)
colnames(new.df) <- c("day", "steps")
```
>Plotting Data

```r
m <- ggplot(new.df, aes(x=day, y = steps)) + 
      geom_histogram(stat = "identity") + theme(text = element_text(size=10),
       axis.text.x = element_text(angle=90, vjust=1)) 
print(m)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
>Mean and Median

```r
steps.mean <- mean(sumvec, na.rm = TRUE)
steps.median <- median(sumvec, na.rm = TRUE)
print(paste("Mean is ", steps.mean, " and Median is ", steps.median))
```

```
## [1] "Mean is  10766.1886792453  and Median is  10765"
```
## What is the average daily activity pattern?
>Preparing Data

```r
new.df <- NULL
it <- unique(data$interval)
for(i in 1:length(it))
{
  new.df <- rbind.data.frame(new.df, c(it[i], mean(data$steps[data$interval == it[i]], na.rm = TRUE)))
  
}
colnames(new.df) <- c("Interval", "Average") 
```
>Plotting Data

```r
plot(new.df, type="l")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 
>Maximum Activity

```r
print(paste("Maximum Activity happens at", new.df$Interval[new.df$Average == max(new.df$Average)], "with value ", max(new.df$Average)))
```

```
## [1] "Maximum Activity happens at 835 with value  206.169811320755"
```
## Inputing missing values
>Missing Values

```r
print(paste("Number of Missing Values : ", nrow(data[is.na(data$steps),])))
```

```
## [1] "Number of Missing Values :  2304"
```
>Substituting Values.

The average value of each interval has been substituted for each NA case, rounded off to integer using R's round() function.

```r
data2 <- data
for(i in 1:nrow(data2))
{
  if(is.na(data2$steps[i]))
  {
    data2$steps[i] <- round(new.df$Average[new.df$Interval == data2$interval[i]])
  }
}
```

>Preparing Histogram, and calculating mean and median

```r
sumvec2 <- rowsum(data2$steps, data2$date)
new2.df <- cbind.data.frame(rownames(sumvec2), sumvec2)
colnames(new2.df) <- c("day", "steps")

m2 <- ggplot(new2.df, aes(x=day, y = steps)) + 
      geom_histogram(stat = "identity") + theme(text = element_text(size=10),
       axis.text.x = element_text(angle=90, vjust=1)) 
print(m2)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 

```r
steps2.mean <- mean(sumvec2, na.rm = TRUE)
steps2.median <- median(sumvec2, na.rm = TRUE)
print(paste("Mean is ", steps2.mean, " and Median is ", steps2.median))
```

```
## [1] "Mean is  10765.6393442623  and Median is  10762"
```
Mean and Median changed upon filling the missing values. There was considerable change in the median and slight change in mean.


## Are there differences in activity patterns between weekdays and weekends?


```r
newvec <- numeric(0)
j = 0
for(i in 1:nrow(data))
{
  if(weekdays(strptime(data$date[i], "%Y-%m-%d"))=="Saturday" | weekdays(strptime(data$date[i], "%Y-%m-%d"))=="Sunday")
  {
    newvec[i] = 2
  }
  else
  {
    newvec[i] = 1
  }
    
}

data3 <- cbind.data.frame(data, newvec)

data.week <- data3[data3$newvec == 1,]
data.end <- data3[data3$newvec == 2,]
week.df <- NULL
end.df <- NULL
it <- unique(data$interval)
for(i in 1:length(it))
{
  week.df <- rbind.data.frame(week.df, c(it[i], mean(data.week$steps[data.week$interval == it[i]], na.rm = TRUE)))
  end.df <- rbind.data.frame(end.df, c(it[i], mean(data.end$steps[data.end$interval == it[i]], na.rm = TRUE)))
  
}
colnames(week.df) <- c("Interval", "Average") 
colnames(end.df) <- c("Interval", "Average") 
set.panel(2,1)
```

```
## plot window will lay out plots in a 2 by 1 matrix
```

```r
plot(week.df, type="l")
plot(end.df, type="l")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
