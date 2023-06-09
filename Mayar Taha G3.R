
library(ggplot2)
library(mice)
library(tidyverse)

# Read the dataset file
df <- read.csv("G3_sydney_hobart_times.csv", header = TRUE)
df

# Re-code the time less than 3 to true and false
df$Code.Time.less.than.3[df$Time < 3]<-'TRUE'
df$Code.Time.less.than.3[df$Time > 3]<-'FALSE'
df

# Dataset Sense
nrow(df)
ncol(df)
colnames(df)<-c('YEAR', 'TIME', 'F_START',   'F_FINISH', 'TIME<3')
summary(df)

# see the structure of the dataset
str(df)

# Data Indexing & Slicing
head(df)
tail(df)

# Get only the first 25 rows
head(df,25)
# Get only the last 25 rows
tail(df,25)

#Select specific data
new_df<-df[2:10,c(1,2,3)]
new_df

# Remove some columns from dataset
df2<-df[-(10:20),]
df2
df[-(10:50),c(-1,-5)]
df[-(10:50),1:3]

# Cleaning Data
# Remove "day" from the time column & convert it to numeric
df$TIME <- as.numeric(gsub("day", "", df$TIME))
df


#Data Sorting
sort1<-df[order(df$TIME),]
sort1
sort2<-df[order(-df$TIME),]
sort2

#filter dataset
fil1<-df[df$TIME > 3 & df$F_START>10 , ]
fil1
fil2<-df[df$F_FINISH >15  & df$TIME<5 , c(1,2,4)]
fil2

# Data Adding/Re-coding Columns
# Add new column.
df$NEW_COL <- c(1:72)
df

# Set a single value for multiple rows at once
rows_to_update <- c(1, 3, 33, 61)
df[rows_to_update, "TIME"] <- 3
df

# Dealing with Missing Data
# show all the rows that have NA
complete.cases(df)
df[ ! complete.cases(df), ]


#using mice function
df[ ! complete.cases(df), ]
Pre.imputation <- mice(df, m = 5,meth = c("", "pmm","","", "", ""),maxit = 20)
Pre.imputation$imp
newdf <- complete(Pre.imputation, 5)
newdf

#Data Manipulation
is.na(df)
any(is.na(df$TIME<3))

mean(df$F_START)
median(df$F_START,na.rm = T)


# Data Visualization

#histogram
draw_hist <- ggplot(df, aes(x = TIME)) +
geom_histogram(binwidth = 5, color = "black", fill = "blue", alpha = .9)+
ggtitle("TIME Histogram")
draw_hist

#scatter
draw_sc <- ggplot(df, aes(x=F_START, y=F_FINISH))
draw_sc
draw_sc + geom_point() 

# Bar Chart
draw_bar<- ggplot(df,aes(x=F_START, fill = F_FINISH))
draw_bar + geom_bar()
draw_bar + geom_bar() + theme_light()



