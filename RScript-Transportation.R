# Load package tidyverse
library(tidyverse)

# Read Random csv file
random <- read_csv("PST1Random.csv")
summary(random)

# Filter the random csv file with my student number as username
a <- filter(random, Username == "n10557342")
a 

# Read Data csv file
dataset <- read_csv("PST1Data.csv")

# Filter dataset file with unique origin "PIT" 
data <- filter(dataset, Origin == "PIT")

#Read airport code and airline code csv files
airportcode <- read_csv("PST1AirportCodes.csv")
airlinecode <- read_csv("PST1AirlineCodes.csv")

# Add Airport name by joining "data" and "airportcode" datasets
data <- left_join(data, airportcode, by=c("Dest" = "Airport Code"))
# Add Airline name by joining "data" and "airlinecode" datasets
data <- left_join(data, airlinecode, by=c("Reporting_Airline" = "Airline code"))









############ SECTION A

## A1 DATA STRUCTURE

# A1.1
# Get class for all variables to get a data dictionary
str(data)

# A1.2
# Convert column "Reporting_Airline" into class factor
data$Reporting_Airline <- as.factor(data$Reporting_Airline)

# A1.3
# Summary of the column "DepDelay" from data
summary(data$DepDelay)
sd(data$DepDelay, na.rm = TRUE)

# A1.4
# Group data by airlines
airlines_grouped <- group_by(data, Reporting_Airline)
# Find mean, median, total observations and standard deviation of all airline groups
airlines_summaries <- summarise(airlines_grouped, count=n(), mean=mean(DepDelay, na.rm = T), 
                median=median(DepDelay, na.rm = T), sd=sd(DepDelay, na.rm = T))
# Arrange the data in desceding order to extract the top 5
airlines_summaries <- arrange(airlines_summaries, desc(count))
top5airlines <- head(airlines_summaries, 5)


## A2 GRAPHICAL SUMMARIES

# A2.1
#Plot the relationship between departure delay and arrival delay
ggplot(data=data, aes(x=DepDelay, y=ArrDelay)) +
  geom_point() + 
  geom_smooth() + 
  theme_bw() +
  labs(x="Departure delay (in minutes) ", y="Arrival delay (in minutes)", 
       title="Relationship between departure delay and arrival delay")

# Plot the relationship between departure delay and arrival delay, set the limit on
# x-axis from -30 to 20 for better visualization
ggplot(data=data, aes(x=DepDelay, y=ArrDelay)) +
  geom_point() + 
  geom_smooth() + 
  theme_bw() +
  xlim(-30,30)+ylim(-30,30)+
  labs(x="Departure delay (in minutes)", y="Arrival delay (in minutes)", 
       title="Relationship between departure delay and arrival delay")

# A2.2
# Plot that shows the distribution of departure delays
ggplot(data=data, aes(x=DepDelay)) +
  geom_histogram(color="goldenrod4", fill = "gold2")+
  theme_bw() +
  labs(x= "Departure delay (in minutes) ", y="Number of observations",
       title="Distribution of departure delays")

# A2.3  
# Plot that shows the distribution of arrival delays.
ggplot(data=data, aes(x=ArrDelay)) +
  geom_histogram(color="darkolivegreen4", fill = "darkseagreen3") +
  theme_bw() +
  labs(x= "Arrival delay (in minutes)", y="Number of observations",
       title="Distribution of arrival delays")

# A2.4
#Get top 3 airlines and club other airlines into "Others"
data$Reporting_Airline <- fct_lump_n(data$Reporting_Airline, 3)

#Variation of departure delay by airline
ggplot(data, aes(x = Reporting_Airline, y = DepDelay)) +
  geom_boxplot(fill="thistle") +
  theme_bw() +
scale_y_continuous(limits = c(-20, 20))+     #set limit of x-axis from -20 to 20
  labs(x = "Airline", y="Departure delay (in minutes)", 
       title="Variation of departure delay by airline")

# A2.5
#Get top 3 destinations and club other destinations into "Others"
data$Dest <- fct_lump_n(data$Dest, 3)
#Variation of arrival delay by destination
ggplot(data, aes(x = Dest, y = ArrDelay)) +
  geom_boxplot(fill="lightsteelblue1") +
  theme_bw() +
  scale_y_continuous(limits = c(-40, 40)) +
  labs(x = "Destination airport", y="Arrival delay (in minutes)", 
       title="Variation of arrival delay by destination")






############ SECTION B - HYPOTHESIS TESTING

## B1 SETUP

# B1.1
# Extract two most popular airlines
top2airlines <- filter(data,data$Reporting_Airline == "YX"|data$Reporting_Airline == "WN")
summary(top2airlines)

# Remove NA values
top2airlines <- na.omit(top2airlines)

# Organize data as "Late" or "Not late" depending on the departure delay.
# if depdelay is on-time or early, then "Not Late", if deldelay is >0, then "late".
top2airlines <- top2airlines %>%
  mutate(DepType = ifelse(DepDelay <= 0, "Not late", "Late"))

#Counting late and not late flights for each airline type
c <- group_by(top2airlines, Reporting_Airline)
d <- count(c, DepType)


# B1.4
#Find Expected coiunts
totaldeptype<- summarise(group_by(top2airlines,DepType), count = n())
totalairline <- summarise(group_by(top2airlines,Reporting_Airline), count = n())
totaldeptype <- matrix(totaldeptype$count)
totalairline <- matrix(totalairline$count)
expected <- data.frame(totaldeptype%*%t(totalairline)/sum(totalairline))


## SECTION B2 - HYPOTHESIS TEST AND INTERPRETATION

# B2.1
# Find Observed counts
observed <- table(top2airlines$DepType,top2airlines$Reporting_Airline)
observed

contribution <- (observed - expected)^2/expected
round(contribution,2)

ts <- sum(contribution)  #test statistic

df <- (nrow(observed)-1)*(ncol(observed)-1)  #degrees of freedom

# B2.2
pval <- pchisq(q = ts, df = df, lower.tail = F)  #p-value in chi-sq test
pval





############ SECTION C - LINEAR MODEL

## SECTION C1 - CREATE LINEAR MODEL

# Get linear model for the data for variables "ArrDelay" and "DepDelay"
depdelay_model <- lm(data=data, ArrDelay ~ DepDelay)  
depdelay_model
summary(depdelay_model)

library(broom)  #Load library broom
# Get linear model, confidence intervals, beta0, beta1 and p-value
tidy(depdelay_model, conf.int = T, conf.level = 0.95) %>%
  select(term, estimate, conf.low, conf.high, p.value)


## SECTION C2 - REGRESSION ASSUMPTIONS
# C2.1

# Create dataframe dep.fort to analyse the residuals
dep.fort <- fortify(depdelay_model)
head(dep.fort)

#  Plot residual variation with fitted values
ggplot(data = dep.fort, aes(x = .fitted, y = .resid))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(x = expression(paste("Fitted (",hat(y[i]), ")")), 
       y = expression(paste("Residual (",epsilon[i],")")))


# C2.2
# QQ plot that compares the standardised residuals to a standard normal distribution
ggplot(data=dep.fort, aes(sample=.stdresid)) +
  stat_qq(geom="point") +
  geom_abline(intercept=0, slope=1) +
  coord_equal()+
  theme_bw()

# C2.5
# Plor residual
ggplot(data = dep.fort, aes(x = .resid))+
  geom_histogram(colour = "white", fill = "mistyrose3", aes(y = ..density..))+
  theme_bw()+
  stat_function(fun = "dnorm", args = list(mean = mean(dep.fort$.resid), sd = sd(dep.fort$.resid)))+
  labs(x = "Residual", y = "Density")
 
# Plot standardised residual
#Set the y axis to be the density, rather than the count, 
#and add a normal distribution with mean 0 and standard deviation 1.
 ggplot(data = dep.fort, aes(x = .stdresid))+
  geom_histogram(colour = "white", fill = "mistyrose3", aes(y = ..density..))+
  theme_bw()+
  stat_function(fun = "dnorm", args = list(mean = 0, sd = 1))+
  labs(x = "Standardised Residual", y = "Density")

# Plot CDF for standard normal distribution
ggplot(data = data.frame(x = c(-4,6)), aes( x= x))+
   stat_function(fun = "pnorm", args = list(mean = 0, sd = 1), size = 1)+
   labs(title = "CDF for standard normal distribution") +
   theme_bw()
 
# Plot ecdf 
dep_ecdf <- ecdf(dep.fort$DepDelay)
ggplot(data = data.frame(x = c(-4,6)), aes( x= x))+
     stat_function(fun = "dep_ecdf", size = 1)+
  labs(title = "Emperical CDF") +
     theme_bw()
   

# C2.6
# get cdf for normal distribution
p_cdf <- function(x){return(pnorm(q = x, mean = 100, sd = 15))}

# KS goodness of fit test
ks.test(x = dep.fort$DepDelay, y = "p_cdf")


