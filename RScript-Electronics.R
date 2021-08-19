# Load package tidyverse
library(tidyverse)

# Read the CSV file
data1 <- read.csv("PST2data.csv")

# Removing rows that have my student number in "Student" column
data2 <- subset(data1, Student!="n10557342")

# Remove "Student" variable
data <- data2[c(-6)]

##                                 SECTION A    DATA AND VISUALIZATION
# A1 DATA STRUCTURE

#A1.1
str(data)
summary(data)

#A1.2
# Get the number of years since 1970 in variable "TimeSince"
data <- data %>% mutate(TimeSince = data$Year - 1970)

#A1.3
# Get min, median and max for variables
summary(data, na.rm=TRUE)

# A2 GRAPHICAL SUMMARIES
#A2.1
library(GGally)
#Pairwise plot to show relation between each variable 
ggpairs(data, method="pairwise", title = "Pairwise Plot") 

#A2.2
library(plyr)
# Rename column "Clock (MHz)" to "Clock"
data <- rename(data, c("Clock..MHz." = "Clock"))

# Change y axis to natural logarithm
data$lClock<-log(data$Clock)

# Plot Clock speed over TimeSince
ggplot(data=data, mapping=aes(x=TimeSince, y=lClock)) +
  geom_point(color="coral3", axisLine = "black") + 
  geom_smooth(method=lm, color="black") +
  labs(y = "Clock speed (in natural logarithm)", 
       title="Variability in Clock speed over time since 1970") +
  theme_bw() 

##                               SECTION B  LINEAR REGRESSION

#B1 SETUP
# B1.1
# TimeSince vs Clock
ggplot(data=data, mapping=aes(x=TimeSince, y=Clock)) +
  geom_point(color="coral3", axisLine = "black") + 
  geom_smooth(method=lm, color="black") +
  labs(y = "Clock speed", 
       title="Variability in Clock speed over time since 1970") +
  theme_bw() 

# log(TimeSince) vs Clock
ggplot(data=data, mapping=aes(x=log(TimeSince), y=Clock)) +
  geom_point(color="coral3", axisLine = "black") + 
  geom_smooth(method=lm, color="black") +
  labs(y = "Clock speed", 
       title="Variability in Clock speed over time since 1970") +
  theme_bw() 

# log(TimeSince) vs log(Clock)
ggplot(data=data, mapping=aes(x=log(TimeSince), y=lClock)) +
  geom_point(color="coral3", axisLine = "black") + 
  geom_smooth(method=lm, color="black") +
  labs(y = "Clock speed", 
       title="Variability in Clock speed over time since 1970") +
  theme_bw()  


# B2 LINEAR MODEL
# B2.1
lm1 <- lm(data=data, lClock ~ TimeSince, na.action = na.omit)

library(broom)  #Load library broom
# Get linear model, confidence intervals, beta0, beta1 and p-value
tidy(lm1, conf.int = T, conf.level = 0.95) %>%
  select(term, estimate, conf.low, conf.high, p.value)

# B2.3
summary(lm1)$r.squared


# B3 ANALYSIS OF RESIDUALS

# B3.1
# Create dataframe time.fort to analyse the residuals
lm1.fort <- fortify(lm1)
head(lm1.fort)

#  Plot residual variation with fitted values
ggplot(data = lm1.fort, aes(x = .fitted, y = .resid))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(x = expression(paste("Fitted (",hat(y[i]), ")")), 
       y = expression(paste("Residual (",epsilon[i],")")))


# B3.2
# Rename column "Power Density" to "PowerDensity"
data <- rename(data, c("Power.Density" = "PowerDensity"))

ggplot(data = lm1.fort, aes(x = data$PowerDensity, y = .resid))+
  geom_point()+
  theme_bw()+
  geom_smooth()+
  labs(x = expression(paste("Fitted (",hat(y[i]), ")")), 
       y = expression(paste("Residual (",epsilon[i],")")))

# B3.3
# QQ plot that compares the standardised residuals to a standard normal distribution
ggplot(data=lm1.fort, aes(sample=.stdresid)) +
  stat_qq(geom="point") + geom_abline() +
  xlab("Theoretical (Z ~ N(0,1))") +
  ylab("Sample") + coord_equal() + theme_bw()


##                               SECTION C  ADVANCED REGRESSION

# C1 MULTIPLE EXPLANATORY VARIABLES

#C1.1
# LINEAR MODEL
lm2 <- lm(data=data, lClock ~ TimeSince + PowerDensity)

#fit.tidy <- tidy(fit, conf.int = TRUE)

library(broom)  #Load library broom
# Get linear model, confidence intervals, beta0, beta1 and p-value
tidy(lm2, conf.int = T, conf.level = 0.95) %>%
  select(term, estimate, conf.low, conf.high, p.value)

#C1.3
summary(lm2)$r.squared


# C2 RESIDUAL ANALYSIS

#C2.1
# Create dataframe fit.fort to analyse the residuals
lm2.fort <- fortify(lm2)

ggplot(data=lm2.fort, aes(x=.fitted, y=.resid)) +
  geom_point() + xlab("Fitted") +
  geom_smooth()+
  ylab("Residuals = Observed - Fitted") + theme_bw()

# C2.2
# QQ plot that compares the standardised residuals to a standard normal distribution
ggplot(data=lm2.fort, aes(sample=.stdresid)) +
  stat_qq(geom="point") + geom_abline() +
  xlab("Theoretical (Z ~ N(0,1))") +
  ylab("Sample") + coord_equal() + theme_bw()

# C3 MODEL CHOICE
# Performing F-test
anova(lm1, lm2)

# C4 BEST MODEL
lm3 <- lm(data=data, lClock ~ TimeSince + PowerDensity + Cores  + Transistors)

# Get linear model, confidence intervals, beta0, beta1 and p-value
tidy(lm3, conf.int = T, conf.level = 0.95) %>%
  select(term, estimate, conf.low, conf.high, p.value)

summary(lm3)$r.squared

anova(lm1, lm2, lm3)
