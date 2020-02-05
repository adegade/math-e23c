rm(list = ls()) # clear console

# ADETAYO ADEGOKE
# WEEK 01 Homework - Logic, using Boolean Algebra and Finite Fields
# Due: February 05, 2020
# Mathematics E-23c, Spring 2020



# Questions 1 (a), (b) and (c)

# (p & q) IMPLIES (r | s) is equivalent to [not(p & q) | (r | s)]
# the le column represents this logical expression
eg <- expand.grid(c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE), c(TRUE, FALSE)); eg
ttbl <- data.frame(eg[,c(4:1)]); ttbl # reverse the columns
colnames(ttbl) <- c('p', 'q', 'r', 's'); ttbl #change the column names to match variable names, and display the truth table

# Had trouble with attach :(
# Construct the columns that will be used to compute the logical expression
ttbl$pq <- (ttbl$p & ttbl$q)
ttbl$notpq <- (!(ttbl$p & ttbl$q))
ttbl$rors <- (ttbl$r | ttbl$s)
ttbl$le <- (ttbl$notpq | ttbl$rors); ttbl # logical expression column

# The constructive normal form (aka cnf column below)for the LE looking at the truth table is
# (!p | !q | r | s)
ttbl$cnf <- (!(ttbl$p) | !(ttbl$q) | ttbl$r | ttbl$s); ttbl # voila, column le and cnf match in the truth table!

rm(list = ls()) # clear console


# Questions 2(a)

# setwd("/Users/adegade/OneDrive/Documents/Education/Harvard/Software Engineering/Spring-2020-MATH-E-23C/Homework")
Gssl <- read.csv("GSSLogical.csv") # load the input file
head(Gssl) # test that it and review columns
attach(Gssl)
index <- which(!is.na(Male) & !is.na(Republican) & !is.na(GunOwner) &
                 (Male == "FALSE") & (Republican == "FALSE") & (GunOwner == "TRUE")); index
length(index) # 37 = number of participants in the survey who were female democrats who owned guns

# Clean Up!
detach(Gssl)
rm(list = ls()) # clear console


# Questions 2(b)

# setwd("/Users/adegade/OneDrive/Documents/Education/Harvard/Software Engineering/Spring-2020-MATH-E-23C/Homework")
Redsox <- read.csv("RedSox2013.csv") # load the input file
head(Redsox) # test that it and review columns
attach(Redsox)
TRuns <- R + RA # create a new column that totals up runs
HRuns <- subset(Redsox, TRuns >= 10); HRuns # get a subset of data with for games with 10 runs or more
min(HRuns$Duration)


# Question 2(c)

boxplot(R ~ DayNight) # display box plot of runs in day and night Red Sox games 


# Question 2(d)

tbl <- table(WonLost, Away); tbl # create the table for observed WonLost and Away comparison
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected # create the table for expected WonLost and Away comparison 
chisq.test(tbl, Expected) # there is a 16.73% chance that the discreptancy b/w expected and observed comparison between WonLost and Away
# is not statistically relevant, assuming the null hypothesis of independence.  The dataset does not provide enough evidence to support
#  the null hypothesis of independence (0.1673 > 0.05).  Red Sox did not seem to have an advantage in Home or Away games.

tbl <- table(WonLost, DayNight); tbl # create the table for observed WonLost and Away comparison
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected # create the table for expected WonLost and Away comparison 
chisq.test(tbl, Expected) # there is a 43.8% chance that the discreptancy b/w expected and observed comparison between WonLost and DayNight
# is not statistically relevant, assuming the null hypothesis of independence.  The dataset does not provide enough evidence to support
#  the null hypothesis of independence (0.438 > 0.05).  Red Sox did not seem to have an advantage in Home or Away games.

# Clean Up!
detach(Redsox)
rm(list = ls()) # clear console


# Question 3(a)

attach(airquality) # attach the data frame
head(airquality) # display the first few rows of the data frame
boxplot(Ozone ~ Month) # display box plot of how Ozone depends on Month
boxplot(Solar.R ~ Month) # display box plot of how Solar Radiation depends on Month
boxplot(Wind ~ Month) # display box plot of how Wind depends on Month
boxplot(Temp ~ Month) # display box plot of how Temperature depends on Month


# Question 3(b)

aq <- get("airquality"); head(airquality) # get and display the airquality data frame
attach(aq)

plot(Temp, Solar.R) # create a plot of Temperature versus Solar Radiation
model <- lm(Solar.R ~ Temp) # create a Model
abline(model$coefficients[1], model$coefficients[2]) # draw the regression line on the plot
# data points in the plot are widely dispersed, as such temperature does not seem to be good predictor for solar radiation

plot(Temp, Ozone) # create a plot of Temperature versus Ozone
model <- lm(Ozone ~ Temp) # create a Model
abline(model$coefficients[1], model$coefficients[2]) # draw the regression line on the plot
# data points in the plot are relatively close together with a strong positive linear relationship, as such temperature 
# does seem to be good predictor for solar radiation


# Question 3(c)

# Define new logical vectors while accounting for the NA data points in the set
smoggy <- Ozone > mean(Ozone, na.rm = TRUE); head(smoggy)
windy <- Wind> mean(Wind, na.rm = TRUE); head(windy)
sunny <- Solar.R > mean(Solar.R, na.rm = TRUE); head(sunny)
hot <- Temp > mean(Temp, na.rm = TRUE); head(hot)

# Select the days where all four logical vectors defined above are true
subset(airquality, (smoggy == TRUE) & (windy == TRUE) & (sunny == TRUE) & (hot == TRUE))


# Question 3(d)

chisq.test(smoggy, windy) # there is a 4.29 x 10^(-4)% chance that observed dependencies between the two factors being compared
# happened by chance.  Since this is a lot less than 5%, we will reject the null hypothesis of independence between the
# two variables and propose that they are both probably dependent
# smoggy and windy are probably dependent variables

chisq.test(windy, sunny) # there is a 68.23% chance that observed dependencies between the two factors being compared
# happened by chance.  Since this is a lot more than 5%, we will fail to reject the null hypothesis of independence between the
# two variables and propose that they are both probably independent
# windy and sunny are probably independent variables

#Clean Up!
detach(airquality)
rm(list = ls()) # clear console