Assessment 1
=========================

1. Loading and preprocessing the data
Show any code that is needed to
Load the data (i.e. read.csv())
Process/transform the data (if necessary) into a format suitable for your analysis

```r
library(knitr)
library(dplyr)
library(ggplot2)
library(plyr)
library(doBy)
library(knitr)

knit2html("PA1_template.rmd")
```

```
## 
## 
## processing file: PA1_template.rmd
```

```
##   |                                                                         |                                                                 |   0%  |                                                                         |......                                                           |   9%
##   ordinary text without R code
## 
##   |                                                                         |............                                                     |  18%
## label: unnamed-chunk-6
```

```
##   |                                                                         |..................                                               |  27%
##   ordinary text without R code
## 
##   |                                                                         |........................                                         |  36%
## label: unnamed-chunk-7
```

```
##   |                                                                         |..............................                                   |  45%
##    inline R code fragments
## 
##   |                                                                         |...................................                              |  55%
## label: unnamed-chunk-8
##   |                                                                         |.........................................                        |  64%
##   ordinary text without R code
## 
##   |                                                                         |...............................................                  |  73%
## label: unnamed-chunk-9
```

```
##   |                                                                         |.....................................................            |  82%
##    inline R code fragments
## 
##   |                                                                         |...........................................................      |  91%
## label: unnamed-chunk-10
##   |                                                                         |.................................................................| 100%
##   ordinary text without R code
```

```
## output file: PA1_template.md
```

```r
# 1.Load the data and 2.Process/transform the data (if necessary) into a format suitable for your analysis

# what is the working directory
wd <- getwd()
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

```r
wd
```

```
## [1] "/Volumes/sdata/p/Coursera/5_Reproducible_Research"
```

```r
# read the input file ativity.csv in df - the column steps shall have the format integer
df <- read.csv("activity.csv", header = TRUE, na.strings="NA", colClasses =c("integer", "Date", "integer"), sep=",")

# What is the data structure of df
str(df)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
# first analysis of df
summary(df)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```

```r
pairs(df)
tail(df)
```

```
##       steps       date interval
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

```r
dim(df)
```

```
## [1] 17568     3
```

2. What is mean total number of steps taken per day?
For this part of the assignment, you can ignore the missing values in the dataset.
Calculate the total number of steps taken per day
If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
Calculate and report the mean and median of the total number of steps taken per day


```r
# Calculate the total number of steps taken per day

# copy df to df_new and in the column steps eliminate NA's and use instead 0
df_new <- na.omit(df)
steps_sum <- sum(df_new$steps)

# If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
# hint: because there are a lot of 0-steps xlim is from 1 to 850 and not from 0 to 850!!!
qplot(df_new$steps, geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for steps", 
      xlab = "steps",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,850))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
# Calculate and report the mean and median of the total number of steps taken per day
steps_mean <- mean(df_new$steps)
steps_median <- median(df_new$steps)
```

        The total number of steps per day is 570608
        The mean of the total number of steps taken each day is 37.3825996
        The median of the total number of steps taken each day is 0


3. What is the average daily activity pattern?
Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?



4. Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
#Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
number_complete_df <- sum(complete.cases(df))
number_not_complete_df <- sum(!complete.cases(df))

#Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
#Create a new dataset that is equal to the original dataset but with the missing data filled in.

# take the mean for each 5-minute interval and create a new dataset that is equal to the original dataset but with the missing data filled in
df_tmp <- df
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
df_n <- ddply(df_tmp, ~ interval, transform, steps = impute.mean(steps))

# Make a histogram of the total number of steps taken each day
qplot(df_n$steps, geom="histogram",
      binwidth = 0.5,  
      main = "Histogram for steps", 
      xlab = "steps",  
      fill=I("blue"), 
      col=I("red"), 
      alpha=I(.2),
      xlim=c(1,850))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
# Calculate and report the mean and median of the total number of steps taken per day
steps_mean_n <- mean(df_n$steps)
steps_median_n <- median(df_n$steps)
```

        The total number of rows with NAs is 2304
        The mean of the total number of steps taken each day is 37.3825996
        The median of the total number of steps taken each day is 0
        Do these values differ from the estimates from the first part of the assignment? 
        No, so maybe I made a mistake?
        What is the impact of imputing missing data on the estimates of the total daily number of steps?


5. Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



