crime <- read.csv("C:/Users/mdtar001/SkyDrive/Data Science/Data/Offences.Washington.2013.csv")
require("RColorBrewer")

#Change the first column-city-from a variable to just row names
rownames(crime) <- crime[,1]  # Use state names for row names
crime[,1] <- NULL  # Remove state names as variable
crime

#structure of crime
str(crime)

#summary of crime
summary(crime)

#Comparative Statistics in between Selected Cities

crime.cities <- crime[c("Seattle","SeaTac","Bellevue", "Bothell", "Everett", "Olympia3", 
                        "Redmond", "Renton", "Tacoma"), ]

#STATISTICS AND CHARTS FOR ONE VARIABLE 

#create a barplot for robbery, burglary, larctheft, vectheft, and violent
#barplot(crime.cities$Rape
#        color = 
#        )

#STATISTICS AND CHARTS FOR MULTIPLE VARIABLES

#Frequency of each crime for a city (|Ch. 3)

#Frequency of each crime for two or more cities


#STATISTICS AND CHARTS FOR MULTIPLE CATEGORIES

#Scatterplot for Vehicletheft across cities

#Scatterplot for vehicletheft all over washington


#vehicleThect
group.vehichetheft<- table(crime$VehTheft)
group.vehichetheft
round(prop.table(group.vehichetheft), 2) * 100
hist(group.vehichetheft)


#correlation
cor(crime)

cor(crime.cities)


