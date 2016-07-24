# Reproducible Research Project Assignment 1
Ronan1123  
20 July 2016  


## Introduction

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

We aim to answer a number of questions:

1. What is mean total number of steps taken per day?

2. What is the average daily activity pattern?

3. How do we deal with missing values?

4. Are there differences in activity patterns between weekdays and weekends?

We need to write a Report that answers these questions but also allows our results 
to be reproduced.

## Set up the R environment
First we load the required libraries and set the working directory

```r
libraries.need <- c("ggplot2", "dplyr", "knitr")
lapply(libraries.need, require, character.only = TRUE)
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.2.2
```

```
## Loading required package: dplyr
```

```
## Warning: package 'dplyr' was built under R version 3.2.2
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
## 
## Loading required package: knitr
```

```
## Warning: package 'knitr' was built under R version 3.2.2
```

```
## [[1]]
## [1] TRUE
## 
## [[2]]
## [1] TRUE
## 
## [[3]]
## [1] TRUE
```

```r
work_dir <- "C:/Users/User/DataScience/Reproducible Research"
setwd(work_dir)
```



## Loading and preprocessing the data
Now we will read in the data

```r
activity.data <- read.csv('./activity.csv',header=TRUE)
```

Then we'll carry out some preprocessing that will be useful later on, including:

1. change date field from factor to date format and

2. add a weekday/weekend indicator field as a factor


```r
activity.data$date <- as.Date(activity.data$date)
activity.data$weekday <- weekdays(activity.data$date)
weekendday <- c("Saturday", "Sunday")
activity.data$weekendf <-  factor((activity.data$weekday %in% weekendday), 
                                  levels=c(TRUE, FALSE), labels=c('weekend', 'weekday'))
```


## What is the mean total number of steps taken per day?

Lets chart some of the data so that we can see the steps per day. First we
summarise the data by date.  For now we will ignore missing values.


```r
summary.day <- 
                activity.data %>%
                group_by(date) %>%
                summarise(total_steps = sum(steps, na.rm = TRUE))

mean_daily_steps <- mean(summary.day$total_steps, na.rm = TRUE)
median_daily_steps <- median(summary.day$total_steps, na.rm = TRUE)
```

A simple histogram is as follows


```r
qplot(total_steps, data = summary.day, geom = "histogram", fill = I("red"), 
      main = "Histogram: Total Steps per day", 
      xlab = "Total Steps", ylab = "Count")
```

![](Figures/unnamed-chunk-5-1.png) 

We can see a large incidence of "zeroes" perhaps indicating an issue with missing
data. 
The mean number of steps taken per day is 9354.2
The median number of steps taken per day is 10395

## What is the average daily activity pattern?

Lets summarise the data along the 5-minute-interval dimension and then chart
the data to look at some patterns:


```r
summary.5mins <- 
        activity.data %>%
        group_by(interval) %>%
        summarise(total_steps = sum(steps, na.rm = TRUE),
                  mean_steps = mean(steps, na.rm = TRUE))

qplot(data = summary.5mins, x = interval, y = total_steps, 
      geom = "line", main = "Mean Steps per interval", 
      xlab = "Interval", ylab = "Mean Steps")
```

![](Figures/unnamed-chunk-6-1.png) 


The 5 minute interval with the largest mean number of steps is 835

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data. The number of missing values is 2304

Lets create a new dataset that is equal to the original dataset but with the 
missing data filled in.  We replace NAs with the average for that particular 
5-minute interval:


```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
activity.data$steps <- as.numeric(activity.data$steps)

activity.impute <-
        activity.data %>%
                group_by(interval) %>%
                mutate(
                        steps = impute.mean(steps)
                )
```

We can compare differences between the original data-set and the filled in dataset:

```r
summary.day.impute <- 
                activity.impute %>%
                group_by(date) %>%
                summarise(total_steps = sum(steps, na.rm = TRUE))
```

The mean for the "filled in" data set is 10766.2 which compares to the mean before imputing of 9354.2

The median for the "filled in" data set is 10766.2 which compares to the median before imputing of 10395

Lets plot the updated data:

```r
qplot(data = summary.day.impute, x = date, y = total_steps, 
      geom = "bar", stat = "identity", fill = I("blue"), 
      main = "Total Steps per day (imputed)", xlab = "Date", ylab = "Total Steps")
```

![](Figures/unnamed-chunk-9-1.png) 

```r
qplot(total_steps, data = summary.day.impute, geom = "histogram", fill = I("red"), 
      main = "Histogram: Total Steps per day (imputed)", 
      xlab = "Total Steps", ylab = "Count")
```

![](Figures/unnamed-chunk-9-2.png) 
Note now that there are much fewer "zeroes" as the missing observations have been 
imputed


## Are there differences in activity patterns between weekdays and weekends?

We can compare differences between average steps on a weekend and on a weekday


```r
summary.5mins.impute <- 
        activity.impute %>%
        group_by(interval, weekendf) %>%
        summarise(total_steps = sum(steps, na.rm = TRUE),
                  mean_steps = mean(steps, na.rm = TRUE))

qplot(data = summary.5mins.impute, x = interval, y = mean_steps, 
      facets = weekendf ~ .,
      color = weekendf,
      geom = "line", 
      main = "Mean Steps per interval",
      ylab = "Mean Steps")
```

![](Figures/unnamed-chunk-10-1.png) 

On inspection of the charts the users activity levels are quite different on 
weekdays than on the weekends
