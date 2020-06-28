# Read the data form the "csv" file "activity.csv" in your wd if you havent already

csvfile<-read.csv("activity.csv")

#unload dplyr package
library(dplyr)
df<- tbl_df(csvfile)
#To find the mean and median for each day and prepare a seperate table
moddf<-df%>%group_by(date)%>%summarise(median(steps),mean(steps))