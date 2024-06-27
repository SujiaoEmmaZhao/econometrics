# This is a template
# Project      :  
# Author(s)    :  
# Date         :  
# Description  : 


require("knitr")
knitr::opts_chunk$set(echo = TRUE)
## setting working directory
opts_knit$set(root.dir = "C:/Users/bpu058246/Desktop/Teaching/Econometria/Lectures/Data/")


# *********************************************************
#         Open data and summary statistics                #
# *********************************************************

# Specify a library
library(wooldridge)

# sessionInfo()
# install.packages("wooldridge")

# Choose a data set: "wagepan" data
data("wagepan")

# Print the first 6 rows
head(wagepan, 6)

# Number of rows (observations)
nrow(wagepan)

# Number of columns (variables)
ncol(wagepan)

# View chosen data
View(wagepan)

# Describe a data set
library(Hmisc)
di <- describe(wagepan)
di

# Examine the structure
str(di)
# We see that di is a list of lists. 
# We can take it apart by looking at just the first sublist and convert that into a vector.
unlist(di[[1]])

# It is very, very long. Imagine you want the 2nd through 12th elements.
unlist(di[[1]])[2:12]

# Generate summary statisics
summary(wagepan)
summary(wagepan$lwage)

# Mean of lwage and educ
mean(wagepan$lwage)
mean(wagepan$educ)

# Summary statisics in a concise way
library(pastecs)
summary_df <- stat.desc(wagepan)
summary_df


# *********************************************************
#                   Make a plot                           #
# *********************************************************

# plot relationship between educ and lwage
help(plot)
plot(wagepan$educ, wagepan$lwage, 
     main="Labeled Plot", 
     xlab="Education Level", 
     ylab="Log of Wage", 
     col="red")

# *********************************************************
#                   Basic Data Cleaning                   #
# *********************************************************

# create a subdata set with year, educ, lwage

mydata<-cbind(wagepan$year, wagepan$educ, wagepan$lwage)

# add names to data columns
colnames(mydata)<-c("Year", "Education", "Log-Wage")

# limit data to college only, educ >12
#mydata$Education<-ifelse(mydata$Education>12, mydata$Education, NA)
mydata[,2]<-ifelse(mydata[,2]>12, mydata[,2], NA)

mydata<-na.omit(mydata)


# *********************************************************
#        Variable Transformation  & Creation              #
# *********************************************************

mydata <- wagepan
View(mydata)

# Variable transformation
mydata$wage<-exp(mydata$lwage)
wage<-exp(mydata$lwage)

# Creating new variables
mydata$exper_hours<-mydata$exper*mydata$hours

# Dropping variables
help(subset)
mydata<-subset(mydata, select=-c(poorhlth, trad, tra))

# Dummy variables
library(dummies)
educ_dummies<-dummy(mydata$educ)


# *********************************************************
#                            Read & Write files           #
# *********************************************************

# find/set directory
getwd()
setwd("C:/Users/bpu058246/Desktop/Teaching/Econometria/Lectures/Data/")

# write and read CSV files
write.csv(wagepan, file = "wagepan.csv")
mydata2<-read.csv(file="wagepan.csv")

# write and read excel files
# install.packages("readxl")
# install.packages("xlsx")
# install.packages("openxlsx")

