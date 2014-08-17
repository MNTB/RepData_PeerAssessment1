Reproducible Research Assignment 1
==================================
###Loading and Preprocessing the data
1.First downloaded data to my working directory; Open data file
```{r}
Activity<-read.csv("./activity.csv")
```

2. use ddply to look at total steps per day
```{r}
#functions to calculate total steps and sample numbers
totalSteps<-function(data.frame){sum(data.frame$steps)}

SampleNumber<-function(data.frame){nrow(data.frame)}

library(plyr)

#apply functions to dataset, group by date
ddply(Activity, .(date),  c("totalSteps", "SampleNumber"))
```

###What is the mean total number of steps taken per day?

1. assign ddply to variable, plot histogram using ggplot
```{r}
forPlot<-ddply(Activity, .(date),  c("totalSteps", "SampleNumber"))

library(ggplot2)

ggplot(forPlot, aes(x=totalSteps, stat = "bin"))+ theme_bw()+geom_histogram(fill = "light blue", colour = "black")+xlab("") +ggtitle("Histogram of the Total Steps per Day")+theme(plot.title = element_text(size = rel(1.5), face = "bold"))+theme(axis.text.x = element_text(size = rel(1.2), face = "bold.italic"))+theme(axis.text.y = element_text(size = rel(1.2), face = "bold.italic"))+theme(axis.title = element_text(size = rel(1.2)))
```

2.Calculate the mean and median of the total steps taken per day
```{r}
mean(forPlot$totalSteps, na.rm = TRUE)
median(forPlot$totalSteps, na.rm = TRUE)
```

###What is the average daily activity pattern?

1.use ddply to look at average number of steps taken across all intervals, assign to a variable and plot with 
```{r}
#functions to calculate average steps and sample number
aveSteps<-function(data.frame){mean(data.frame$steps, na.rm = TRUE)}

SampleNumber<-function(data.frame){nrow(data.frame)}

#apply functions to dataset, group by time interval (also assign to new data variable)
forPlot2<-ddply(Activity, .(interval), c("aveSteps", "SampleNumber"))

ggplot(forPlot2, aes(x=interval, y = aveSteps))+ theme_bw()+geom_line (colour = "black")+xlab("Time")+ylab("Average Steps Taken") +scale_x_continuous(breaks = seq(from = 0, to=2400, by = 200))+ggtitle("Average Daily Steps taken in Each Five-Minute Interval")+theme(plot.title = element_text(size = rel(1.5), face = "bold"))+theme(axis.text.x = element_text(size = rel(1.2), face = "bold.italic"))+theme(axis.text.y = element_text(size = rel(1.2), face = "bold.italic"))+theme(axis.title = element_text(size = rel(1.2)))
```

2.Find the time when the highest (i.e. the maximum) average daily steps occurs
```{r}
subset(forPlot2, subset = (forPlot2$aveSteps == max(forPlot2$aveSteps)))
```
The interval 835 contains the maximum number of steps


###Imputing missing values

1.Determine the number of rows with NA's
```{r}
#creat logical vector to find Nas
naout<-is.na(Activity[1:nrow(Activity),])

#show when is.na is TRUE
summary(naout)
```
The total number of NAs in the dataset is 2304 

2.How to fill in NA's
```{r}
#first create logical vector to find NA's
no2<-is.na(Activity$steps)

#original dataset
head(Activity, 10)
```

3.create new data set equal to the first data set with missing data filled in

```{r}
#next make a second variable A2, equal to the original vector Activity.  Use the 'no2' vector to find NA's, and where found, substitute in the average steps per time interval from the forPlot2 dataframe (line 45 above)
A2<-Activity

A2$steps[no2]<-forPlot2[,2]

#new dataset with NA's replaced
head(A2, 10)
```

4.New Histogram and mean/median calculations
```{r}

#similar to lines 10-31 above
totalSteps2<-function(data.frame){sum(data.frame$steps)}

SampleNumber<-function(data.frame){nrow(data.frame)}

forPlot3<-ddply(A2, .(date),  c("totalSteps2", "SampleNumber"))

ggplot(forPlot3, aes(x=totalSteps2, stat = "bin"))+ theme_bw()+geom_histogram(fill = "light blue", colour = "black")+xlab("") +ggtitle("Histogram of the Total Steps per Day \n(NA's replaced by mean steps per time interval)")+theme(plot.title = element_text(size = rel(1.5), face = "bold"))+theme(axis.text.x = element_text(size = rel(1.2), face = "bold.italic"))+theme(axis.text.y = element_text(size = rel(1.2), face = "bold.italic"))+theme(axis.title = element_text(size = rel(1.2)))


mean(forPlot3$totalSteps2)
median(forPlot3$totalSteps2)
```
There is almost no impact of imputing the missing data.  The mean remains unchanged, and the median is slightly changed.

###Are there differences in activity patterns between weekdays and weekends?

1.  Create new factor variable with two levels for "weekday" and "weekend"
```{r}

str(A2)

#First change date column to Date class
A2$date<-as.Date(A2$date)

#Then assign new variable indicating weekdays 
A2$weekday<-weekdays(A2$date)
head(A2, 10)

#create two new logical vectors indicating where weekdays and weekends are
wdays<-A2$weekday == "Monday"|A2$weekday == "Tuesday"|A2$weekday == "Wednesday"|A2$weekday == "Thursday"|A2$weekday == "Friday"

wends<-A2$weekday == "Saturday"|A2$weekday == "Sunday"

#replace weekday names with the label "weekday" or "weekend"
A2$weekday[wdays]<-"weekday"

A2$weekday[wends]<-"weekend"

#change class of weekday column from character to factor
A2$weekday<-factor(A2$weekday)

str(A2)
```

2.Panel plot with ggplot2
```{r}
#similar to lines 42-52 above, but add facet_wrap to create panel plot
aveSteps2<-function(data.frame){mean(data.frame$steps)}

SampleNumber<-function(data.frame){nrow(data.frame)}

forPlot4<-ddply(A2, .(interval, weekday), c("aveSteps2", "SampleNumber"))

head(forPlot4)

ggplot(forPlot4, aes(x=interval, y = aveSteps2))+ theme_bw()+geom_line (colour = "black")+facet_wrap(~weekday, ncol = 1)+xlab("Time")+ylab("Average Steps Taken") +scale_x_continuous(breaks = seq(from = 0, to=2400, by = 200))+ggtitle("Average Daily Steps taken in Each Five-Minute Interval:\nComparison of Weekdays and Weekends")+theme(plot.title = element_text(size = rel(1.5), face = "bold"))+theme(axis.text.x = element_text(size = rel(1.2), face = "bold.italic"))+theme(axis.text.y = element_text(size = rel(1.2), face = "bold.italic"))+theme(axis.title = element_text(size = rel(1.2)))
```

There are differences in the activity patterns between weekdays and weekends.

### END

