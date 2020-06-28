#first we make a seperate column representing if the given day is weekend or weekday
df$date <- as.Date(strptime(df$date, format="%Y-%m-%d"))
df$datetype <- sapply(df$date, function(x) {
  if (weekdays(x) == "Sábado" | weekdays(x) =="Domingo") 
  {y <- "Weekend"} else 
  {y <- "Weekday"}
  y
})
#now we plot panel graphs giving info about steps taken per interval based on weekday/weekend info
library(ggplot2)
activity_by_date <- aggregate(steps~interval + datetype, df, mean, na.rm = TRUE)
png("plot4.p.png")
plot<- ggplot(activity_by_date, aes(x = interval , y = steps, color = datetype)) +
  geom_line() +
  labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
  facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
dev.off()