#To plot the histogram of total number of steps taken per day

# Read the data form the "csv" file "activity.csv" in your wd if you havent already

csvfile<-read.csv("activity.csv")

#unload dplyr package
library(dplyr)
df<- tbl_df(csvfile)
#Find the sum of steps taken each day
moddf<-df%>%group_by(date)%>%summarise(steps=sum(steps))
#plot using base plot technique
png("plot1.h.png")
hist(moddf$steps,col = "red",xlab = "Total no of steps",ylab = "counts",main = "Total no of steps taken each day")
dev.off()