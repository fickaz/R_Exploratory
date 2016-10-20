Washcrime <- read.csv("C:/Users/mdtar001/SkyDrive/Data Science/Data/Offences.Washington.2013.csv")
require("RColorBrewer")

#Change the first column-city-from a variable to just row names
rownames(Washcrime) <- Washcrime[,1]  # Use state names for row names
Washcrime[,1] <- NULL  # Remove state names as variable
Washcrime

#Remove population from the variable

crime <- Washcrime[,c(2:11)]
crime

#Comparative Statistics in between Selected Cities

crime.cities <- crime[c("Seattle","SeaTac","Bellevue", "Bothell", "Everett", "Olympia3", 
                        "Redmond", "Renton", "Tacoma"), ]

#correlation
cor(crime.cities)
round(cor(crime.cities), 2) #round to 2 decimal places

# Can test one pair of variables at a time
# Gives r, hypothesis test, and confidence interval
cor.test(crime.cities$LarcTheft, crime.cities$VehTheft)

#Get probabilities for entire matrix at once
# Install "Hmisc" package to get p-values for matrix
install.packages("Hmisc")
require("Hmisc")

# Need to coerce from data frame to matrix
# to get correlation matrix and p-values
rcorr(as.matrix(crime.cities))

#BIVARIATE REGRESSION

plot(crime.cities$Robbery, crime.cities$LarcTheft,
     xlab = "Robbery",
     ylab = "Larceny Theft",
     main = "Correlation in between Robbery and Larceny Theft\n in Major Cities")

plot(crime$Robbery, crime$LarcTheft,
     xlab = "Robbery",
     ylab = "Larceny Theft",
     main = "Correlation in between Robbery and Larceny Theft\n in all areas of Washington")

abline(lm(crime$LarcTheft ~ crime$Robbery))

plot(crime$VehTheft, crime$LarcTheft)
abline(lm(crime$LarcTheft ~ crime$VehTheft))



