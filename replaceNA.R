#To find NA values in "steps" and impute mean values in place of NA values 

# Read the data form the "csv" file "activity.csv" in your wd if you havent already

csvfile<-read.csv("activity.csv")

#unload dplyr package
library(dplyr)
df<- tbl_df(csvfile)

#Find the total no of NA values in the data "steps"
sum(is.na(df$steps))
#create a set of values to be imputed in place of these NAs
moddf2<-df%>%group_by(date)%>%mutate(mean=mean(steps))
imputed<-moddf2$mean[is.na(moddf2$mean)!= TRUE]
#"imputed" contains a set of values to be filled in place of NAs
#create "replace_df" containing replaced values of "steps"
replace_df<-df%>%group_by(date)%>%mutate(replaced_steps=ifelse(is.na(steps),yes = imputed,no=steps))
#replace_steps column is the imputed version of steps column in replace_df

#Make a histogram of the total number of steps taken each day 
replace_sum<-tapply(replace_df$replaced_steps,replace_df$date,sum)

png("plot2.h.png")
hist(replace_sum,col = "blue",xlab = "Total Steps",main = "Total no of steps taken each day")
dev.off()
#find mean and median
mean(replace_sum)
median(replace_sum)



