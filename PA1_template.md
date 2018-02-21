------------------------------------------------------------------------

### Reproducible Research Course Project 1

    pacman::p_load(plyr, dplyr, tidyr)
    pacman::p_load(readr, haven, ggplot2)
    select<-dplyr::select

Loading and preprocessing the data
----------------------------------

Load the data

    setwd("C:/!KRISTINA/!COURSERA/R_Reproducible Research/Course Project 1")
    if(!file.exists('activity.csv')){
        unzip('activity.zip')
    }
    df <- read.csv('activity.csv')

What is mean total number of steps taken per day?
-------------------------------------------------

1.  Calculate the total number of steps taken per day

<!-- -->

    steps_day <- df %>% group_by(date) %>% 
            summarise(total_steps_by_day=sum(steps, na.rm=TRUE))

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    ggplot(steps_day,  aes(total_steps_by_day)) + geom_histogram() + xlab("total number of steps taken per day")

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-markdown_strict/hist-1.png)

1.  Calculate and report the mean and median of the total number of
    steps taken per day

<!-- -->

    mean_steps <- round(mean(steps_day$total_steps_by_day),1)
    median_steps <- median(steps_day$total_steps_by_day)

The mean = 9354.2 and median = 10395 of the total number of steps taken
per day.

What is the average daily activity pattern?
-------------------------------------------

1.  Time series plot of the 5-minute interval and the average number of
    steps taken, averaged across all days

<!-- -->

    steps_interval <- df %>% group_by(interval) %>% 
            summarise(mean_steps_interval=mean(steps, na.rm=TRUE)) 

    ggplot(data=steps_interval, aes(x=interval, y=mean_steps_interval)) + geom_line()

![](PA1_template_files/figure-markdown_strict/plotmean-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    interval_max_steps<-(steps_interval%>%arrange(desc(mean_steps_interval)))$interval[1] 

The maximum number of steps in interval = 835

Imputing missing values
-----------------------

1.  Calculate and report the total number of missing values in the
    dataset

<!-- -->

    countNA<-dim(df%>%filter(is.na(steps)))[1]

The total number of missing values in the dataset 2304.

1.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    df_imp <- df%>% group_by(interval) %>% 
            mutate_at(vars(steps) , funs(ifelse(is.na(.),mean(.,na.rm=TRUE), .)))

1.  Make a histogram of the total number of steps taken each day and
    Calculate and report the mean and median total number of steps taken
    per day.

<!-- -->

    steps_day_imp <- df_imp %>% group_by(date) %>% 
            summarise(total_steps_by_day=sum(steps, na.rm=TRUE))

    ggplot(steps_day_imp,  aes(total_steps_by_day)) +   geom_histogram()

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](PA1_template_files/figure-markdown_strict/totelstepsimp-1.png)

    mean_steps_imp <- round(mean(steps_day_imp$total_steps_by_day),1)
    median_steps_imp <- median(steps_day_imp$total_steps_by_day)

After missing imputation (NA=mean by date) values are higher the mean =
1.0766210^{4} and median = 1.076618910^{4} of the total number of steps
taken per day. for some days there were no values of the variable steps.
Based on the missing values, the estimates were displaced

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.  Create a new factor variable in the dataset with two levels –
    “weekday” and “weekend” indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    df_imp_type<-df_imp%>% mutate(type = weekdays(as.POSIXlt(date)))  %>% 
            mutate(type=ifelse(type=="суббота"|type=="воскресенье",'weekend', 'weekday'))%>%
            group_by(type, interval)%>%summarise(mean_steps_interval=mean(steps, na.rm=TRUE)) 

1.  Make a panel plot containing a time series plot

<!-- -->

    ggplot(data=df_imp_type, aes(x=interval, y=mean_steps_interval)) + geom_line() +facet_grid(type ~ .)

![](PA1_template_files/figure-markdown_strict/plotcompare-1.png)
