#first we make a seperate column representing if the given day is weekend or weekday
df$date <- as.Date(strptime(df$date, format="%Y-%m-%d"))
df$datetype <- sapply(df$date, function(x) {
  if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
    {y <- "Weekend"} 
  else 
    {y <- "Weekday"}
  y
})
#now we plot panel graphs giving info about steps taken per interval based on weekday/weekend info
library(ggplot2)
activity_by_date <- aggregate(steps~interval + datetype, df, mean, na.rm = TRUE)
g<-qplot(x=interval,y=steps,data = activity_by_date,facets = .~datetype,geom = "line",group=1)
print(g)