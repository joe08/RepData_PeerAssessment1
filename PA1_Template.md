---
title: "Reproducible Research: Peer Assessment 1"
author: "Jyothsna"
date: "September 20, 2015"
output: html_document
---

1.Loading the Raw data

```{r}
data1<- read.csv("activity.csv", stringsAsFactors =FALSE)
```

#####Trasforming the data in to a format suitable for analysis
#####Transform the date attribute

```{r}
data1$date<- as.POSIXct(data1$date, format= "%Y-%m-%d")
data1<- tbl_df(data1)
```

2.What is mean total number of steps taken per day?

##### Caluculating the total number of steps taken per day

```{r}
data1_days<- data1%>% group_by(date)%>% summarise(total.steps = sum(steps))
```

##### Histogram of the total number of steps taken each day
```{r echo = False, fig.width =7, fig.height =6}
hist(data1_days$total.steps, breaks = seq(from=0, to=25000, by=2500),col ="burlywood4", xlab ="Total number of steps",ylim=c(0,20),main = "Histogram of total number of steps taken each day")
```

##### Calculate the mean and median of the total number of steps taken per day
```{r}
mean((data1_days$total.steps), na.rm= T)
median((data1_days$total.steps),na.rm=T)
```

3.What is the average daily activity pattern?

##### create a factor of interval-time of day

```{r}
data1$interval.factor<- as.factor(data1$interval)

data1_interval<- data1 %>% group_by(interval.factor) %>% 
  summarise(mean.steps = mean(steps, na.rm=T))
```
##### Making a time series plot
 
```{r}
 data1_interval$interval<- as.numeric(as.character(data1_interval$interval.factor))
 plot(data1_interval$interval, data1_interval$mean.steps,type ="l",col="cornflowerblue",lwd=2, xlab="Interval[Minutes]", ylab="Average number of steps",main = "Time-series of the average number of steps per intervals")
```
##### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
 
```{r}
 max_steps_interval<- which.max(data1_interval$mean.steps)
 print(data1_interval[max_steps_interval,])
```

4.Imputing missing values.

##### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
```{r}
 na_count<- sum(is.na(data1$steps))
```
 
##### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r}
 data1$weekday<- weekdays(as.Date(data1$date))
 data1$weekday<- factor(data1$weekday, levels =c("Monday","Tuesday","Wednesday",
                                                     "Thursday","Friday","Saturday","Sunday"))
 data1_day<- data1 %>% group_by(weekday,interval.factor)%>%
   summarise(mean.steps = mean(steps, na.rm =T))
```
##### creating a new dataset that is equal to original dataset with missing data filled in.
```{r}
data1_impute<- merge(data1, data1_day, by=c("weekday","interval.factor"))
 data1_impute$impute.steps<- ifelse(is.na(data1_impute$steps),
                                      data1_impute$mean.steps,data1_impute$steps)
```
##### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing  data on the estimates of the total daily number of steps?
```{r}
 data1_impute_mean<- data1_impute %>% group_by(date) %>%
   summarise(total.steps = sum(impute.steps))
 hist(data1_impute_mean$total.steps, breaks =25, col ="hotpink",xlab="Total number of steps",ylim=c(0,15),
      main = "Histogram of the toal number of steps taken each day/(NA replaced by Mean value)")
```

##### Calculating the mean total number of steps taken per day
```{r}
 mean(data1_impute_mean$total.steps)
```
##### Calculating the median total number of steps taken per day
```{r} 
 median(data1_impute_mean$total.steps)
```

5. Are there differences in activity patterns between weekdays and weekends?

##### Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicatin whether a given date is a weekday or weekend day.
```{r}
data1_impute<- data1_impute%>% 
  mutate(weekend = ifelse(weekday == "Saturday"|weekday =="Sunday","weekend","weekday"))
```

##### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).
```{r}
data1_impute_mean<- data1_impute %>% group_by(weekend, interval)%>%
  summarise(mean.steps = mean(impute.steps))
xyplot(mean.steps~interval|weekend, data = data1_impute_mean,
       type="l",layout = c(1,2),lwd=2,col="maroon3",xlab="Interval",ylab="Number of steps",
       main ="Average steps by 5-minute Interval for weekends and weekdays")
```

