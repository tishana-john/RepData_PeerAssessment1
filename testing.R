#library("ggplot2")

unzip("Activity.zip")
data <- read.csv("activity.csv")

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

#new.df <- NULL
#it <- unique(data$interval)
#for(i in 1:length(it))
#{
#  new.df <- rbind.data.frame(new.df, c(it[i], mean(data$steps[data$interval == it[i]], na.rm = TRUE)))

#}
#colnames(new.df) <- c("Interval", "Average") 
#  
#for(i in 1:nrow(data2))
#{
#  if(is.na(data2$steps[i]))
#  {
#    data2$steps[i] <- round(new.df$Average[new.df$Interval == data2$interval[i]])
#  }
#}

#sumvec <- rowsum(data$steps, data$date)
#new.df <- cbind.data.frame(rownames(sumvec), sumvec)
#colnames(new.df) <- c("day", "steps")

#png("plot.png")
#m <- ggplot(new.df, aes(x=day, y = steps)) + 
#      geom_histogram(stat = "identity") + theme(text = element_text(size=10),
#       axis.text.x = element_text(angle=90, vjust=1)) 
#print(m)
#dev.off()

#steps.mean <- mean(sumvec, na.rm = TRUE)
#steps.median <- median(sumvec, na.rm = TRUE)
