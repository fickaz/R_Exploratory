Survey <- read.csv("C:/Users/mdtar001/SkyDrive/Data Science/Data/2016-FCC-New-Coders-Survey-Data-Modified.csv")
str(Survey)
library(Hmisc)
library(dplyr)
library(data.table)
library(ggplot2)

colnames(Survey)

#Gender
#Let us look at gender. 
#Combines, NA, transgender, agender, and genderqueer as neutral
Survey$Gender <- ifelse(is.na(Survey$Gender),"neutral", as.character(Survey$Gender))
gender<- table(Survey$Gender)
summary(gender)
barplot(gender,
        col = "bisque1",
        main = "Gender of new coders",
        xlab = "Gender")

#put gender barplot in descending order
barplot(gender[order(gender, decreasing = TRUE)])

#Countries
Survey$CountryLive <- ifelse(is.na(Survey$CountryLive),"neutral", as.character(Survey$CountryLive))
country <- table(Survey$CountryLive)
barplot(country)
#There are too many countries such that not all of them
#can fit into the x-plot. I believe the highest occurence 
#is United States
#we can list the first 10 highest
countryordered <- table(country[order(country, decreasing = TRUE)])
barplot(countryordered[1:10])

#we can also list the frequency of the country in proportions
prop.table(countryordered[1:10])

prop.table(country[order(country, decreasing = TRUE)])

#to look at exactly how is the name of each country is spelt.
rownames(country)

#SELECTING CASES, because we don't want to look at all cases.

?hist

#mean of the age from USA, na.rm is used to eliminate NA entries
mean(Survey$Age[Survey$CountryLive == "United States of America"], na.rm = TRUE)
hist(Survey$Age[Survey$CountryLive == "United States of America"], 
     na.rm = TRUE,
#     color = "thistle4",
     xlab = "Age",
     main = "Age of New Coder Survey\n in the United States of America")

#Compare groups on mean of one variable

mean(Survey$Age, na.rm = TRUE)

#Here, we can see the mean of age of each country

mean.Age <- aggregate(Survey$Age ~ Survey$CountryLive, FUN = mean, na.rm = TRUE, decreasing = TRUE)



#we can also see mean of other variables, such as Income and Student loan
aggregate(cbind(Survey$StudentDebtOwe, Survey$Income) ~ Survey$CountryLive, FUN = mean, na.rm = TRUE)

#DESCRIPTIVE STATISTICS

#summary for expected earning
summary(Survey$ExpectedEarning, na.rm = TRUE)

#summary for entire table
summary(Survey, na.rm = TRUE)

#Tukey's five-number summary
fivenum(Survey$ExpectedEarning)

#box plot stats
boxplot.stats(Survey$ExpectedEarning)

#ALTERNATIVE DESCRIPTIVES
require("psych")

#describe provides skew, kurtosis, sd, median, se, etc.
describe(Survey$HoursLearning)

#INFERENTIAL STATISTICS

#one sample t-test
#t.test for Income

StudentDebt <- Survey$StudentDebtOwe
t.test(StudentDebt)
#this t-test assumes mean of 0, which is ridiculous
#We can fix this by one-sided t-test using population mean

t.test(StudentDebt, alternative = "greater", mu = 30000)
#Assumin population mean has a student debt of 30000

#Robust Statistics for univariate analyses
#For data that does not fit into the data set
?NaN

Income <- table(Survey$Income)
str(Income)
summary(Income)

Income1 <- as.numeric(Survey$Income, na.rm = TRUE)
summary(Income1)

?data
hist(Income1)
boxplot(Income1,
        main = "Income of new coders")
boxplot.stats(Income1)

summary(Income1)
#the mean is so far higher than the median

#mean is not robust, we can use trimmed mean to eliminate the outlier
mean(Income1, trim = .05, na.rm = TRUE) #5% for each end, 10% total
mean(Income1, trim = .10, na.rm = TRUE)
mean(Income1, trim = .20, na.rm = TRUE) #closer to median

sd(Income1, na.rm = TRUE) #standard deviation. however not robust.
mad(Income1, na.rm = TRUE) #median absolute deviation
IQR(Income1, na.rm = TRUE) #interquartile range
fivenum(Income1, na.rm = TRUE) #Tukey's hinges


