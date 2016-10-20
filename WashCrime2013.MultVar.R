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

#STATISTICS FOR THREE AND MORE VARIABLES

# Basic multiple regression
reg.VehTheft <- lm(VehTheft ~ Robbery +  + Assault + PropCrime + Burglary + 
             LarcTheft + Assault + Arson,
           data = crime)

reg.VehTheft

#VehTheft data are very small that regression cannot yield any significant values

#Basic multiple regression for crime in entire Wash. State, violent as the outcome (dependent var)

reg <- lm(Violent ~ Robbery +  + Assault + PropCrime + Burglary + 
                     LarcTheft + Assault + Arson,
                   data = crime)

reg # Gives the coefficients only  
summary(reg)  # Much more

# More detailed summaries
anova(reg)
coef(reg)  # Or coefficients(reg1)
confint(reg)  # CI for coefficients
resid(reg)  # Or residuals; Residuals case-by-case
hist(residuals(reg))  # Histogram of residuals
 
# Basic multiple regression for major cities, again, violent as the y var.
reg.cities <- lm(Violent ~ Robbery +  + Assault + PropCrime + Burglary + 
             LarcTheft + VehTheft + Arson,
           data = crime.cities)

reg.cities

# More detailed summaries
anova(reg.cities)
coef(reg.cities)  # Or coefficients(reg1)
confint(reg.cities)  # CI for coefficients
resid(reg.cities)  # Or residuals; Residuals case-by-case
hist(residuals(reg.cities))  # Histogram of residuals

#Hierarchical cluster Analysis for major cities
#Distance matrix (dissimilarity matrix)
d <- dist(crime.cities)
d 

# Use distance matrix for clustering
c <- hclust(d)
c

# Plot dendrogram of clusters
plot(c)


#Hierarchical cluster Analysis for all areas of Wash. State
#Distance matrix (dissimilarity matrix)
d.cities <- dist(crime)
d.cities  # Huge matrix

# Use distance matrix for clustering
c.cities <- hclust(d.cities)
c.cities

# Plot dendrogram of clusters
plot(c.cities)

#PRINCIPAL COMPONENT ANALYSIS of all Variables (crimes) in major cities

pc <- prcomp(crime.cities,
             center = TRUE,  # Centers means to 0 (optional)
             scale = TRUE)  # Sets unit variance (helpful)

# Get summary stats
summary(pc)

# Screeplot
plot(pc)

# Get standard deviations and how variables load on PCs
pc

# See how cases load on PCs
predict(pc)

# Biplot
biplot(pc)

#PRINCIPAL COMPONENT OF ALL VARIABLES IN ALL AREAS
pc1 <- prcomp(crime,
             center = TRUE,  # Centers means to 0 (optional)
             scale = TRUE)  # Sets unit variance (helpful)
biplot(pc1)

