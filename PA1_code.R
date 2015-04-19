## Reproducible Research
## Peer Assessement 1

require(dplyr)
require(xtable)
require(ggplot2)

## Load and process the data
readData <- function(){
  
  ## Load data if data does not exist
  
  if(dfExists('actdata') == FALSE ){
    
    actdata <<- read.csv("activity.csv")
    
  }
}

processData <- function(){
  
  ## rearrange data by date
  actdata <<- arrange(actdata,date)
  
  byday <<- group_by(actdata,date)
  
  byinterval <<- group_by(actdata,interval)
  
}
  ## check if dataframe exists
  dfExists <- function(df_name){
    
    return(exists(df_name) && is.data.frame(get(df_name)))
    
  }

## Create histogram of number of steps taken per day

makehist <- function(){
  
  bydaysteps <- summarize(byday,sum_steps=sum(steps))
  
  hist(x = bydaysteps$sum_steps, main = "Total number of steps taken per day", xlab = "Steps" )
  
}

## Calculate mean number of steps taken per day

calcmeansteps <- function(){
  
  summarize(byday,mean_steps=mean(steps,na.rm=TRUE))
  
}

## Calculate median number of steps taken per day

calcmediansteps <- function(){
  
  summarize(byday,median_steps=median(steps,na.rm=TRUE))
  
}

## Calculate total number of missing values 

calctotalmissingvalues <- function(){
  
 numberofrows = nrow(actdata)
 
 numberofnonNArows = nrow(na.omit(actdata))
 
 return(numberofrows-numberofnonNArows)
 
}


## Calc mean by interval

meanbyinterval <- function() {
  
 summarize(byinterval,mean_steps_by_interval=mean(steps,na.rm=TRUE))
  
}

## Create dataset that is equal to the original dataset but with the missing data filled in.

createimputeddata <- function(){
  
  intervalmeans <- summarize(byinterval,mean_steps_by_interval=mean(steps,na.rm=TRUE))
  imputeddata <<- merge(byday,intervalmeans,by=c("interval"))
  imputedbyday <<- arrange(imputeddata, date)
  imputedbyday <<- group_by(imputedbyday,date)
  imputeddata <<- transform(imputedbyday, steps = ifelse(is.na(steps)==FALSE,steps,mean_steps_by_interval))
  imputeddata <<- group_by(imputeddata,date)
  imputeddata <<- arrange(imputeddata,date)
  
  
}


## Create histogram of total number of steps taken each day for imputed dataset
makehist_imputeddata <- function(){
  
  bydaysteps <- summarize(imputeddata,sum_steps=sum(steps))
  
  hist(x = bydaysteps$sum_steps, main = "Total number of steps taken per day \n if steps = NA then use interval mean", xlab = "Steps" )
  
}

## Calculate mean number of steps taken per day for imputed data

calcmeansteps_imputeddata <- function(){
  
  summarize(imputeddata,mean_steps_imputed=mean(steps,na.rm=TRUE))
  
}

## Calculate median number of steps taken per day for imputed data

calcmediansteps_imputeddata <- function(){
  
  summarize(imputeddata,median_steps_imputed=median(steps,na.rm=TRUE))
  
}



## Create new factor variable indicating wheter given date is a weekday or weekend

weekdayorweekend <- function(thisdate){
  
  thedate = as.Date(thisdate)
  
  weekdayname = weekdays(thedate)
  
  if(weekdayname == "Sunday" || weekdayname == "Saturday"){
    return("weekend")
  }
  else
  {
    return("weekday")
    
  }
}

addweekdayfactor <- function(){
  
  dayfactordataset <<- mutate(imputeddata,dayfactor = weekdayorweekend(date))
  
  dayfactordataset <<- group_by(dayfactordataset,dayfactor,interval)
   
}

## Make a panel plot containing time series plot of the 5 minute interval and average number of steps taken 
## averaged across all weekday days or weekend days

makepanelplot <- function(){
  
  plotdata <- summarize(dayfactordataset,mean_steps = mean(steps))
  
  theplot <- ggplot(plotdata, aes(x=interval, y=mean_steps))
  theplot <- theplot + geom_line()
  theplot <- theplot + facet_wrap(~ dayfactor)
  theplot <- theplot + facet_grid(dayfactor ~ .) 
  theplot <- theplot + xlab("Interval")
  theplot <- theplot + ylab("Number of Steps")
  theplot <- theplot + ggtitle("Activity patterns between weekdays and weekends")
  
  theplot
  
}

## Time series plot of 5-minute interval and number of steps taken averaged across all days

maketimeseriesplot <- function(){
  
  plotdata <- summarize(byinterval,mean_steps = mean(steps, na.rm=TRUE))
  
  theplot <- ggplot(plotdata, aes(x=interval,y=mean_steps))
  theplot <- theplot + geom_line()
  theplot <- theplot + xlab("Interval")
  theplot <- theplot + ylab("Number of Steps")
  theplot <- theplot + ggtitle("5-minute interval and average number of steps taken across all days")
  
  theplot
  
}

## Which 5 minute interval on average across all the days in the data set contains the maximum number of steps?

calcmaxstepinterval <- function(){
  
  plotdata <- summarize(byinterval,mean_steps = mean(steps, na.rm=TRUE))
  
 maxsteprow <- plotdata[plotdata$mean_steps == max(plotdata$mean_steps), ]
 
 return(maxsteprow$interval)
  
  
}


## table summary of the data by day

tablesummary <- function(){
  table(actdata$date, is.na(actdata$steps))
}

