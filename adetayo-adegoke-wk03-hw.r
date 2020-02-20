rm(list = ls()) # clear console


# ADETAYO ADEGOKE
# WEEK 03 Homework - Probability for countably infinite sample spaces
# Due: February 19, 2020
# Mathematics E-23c, Spring 2020
# Michael Liotti Office Hour Review


# Questions 1 (a)

# Load the input file
# setwd("/Users/adegade/OneDrive/Documents/Education/Harvard/Software Engineering/Spring-2020-MATH-E-23C/Homework")
Mets <- read.csv("Mets.csv"); head(Mets) 

# Examine wins and lost column from the data set
table(Mets$W.L) # lots of strange entries indeed!

# create a new wins column
wins <- Mets$R > Mets$RA; head(wins)
length(wins)
swins <- sum(wins) # 40!  on the right track

# create index of games that are wins
index <- which(wins == TRUE); index

# Compute the number of wins between games
Y <- numeric(swins)
Y[1] <- index[1]; Y
for (i in 2:swins) {
  Y[i] <- index[i] - index[i - 1]
}
head(Y); head(index)  # checking the computation above for accuracy

table(Y) # checking out the distribution of the data

# creating bins to ensure there are sufficient counts for the chi-square test
observed <- numeric(4) # create the observed vector
for (i in 1:2) {
  observed[i] <- table(Y)[i]
}
observed[3] <- table(Y)[3] + table(Y)[4]; observed
observed[4] <- sum(table(Y)[5:length(table(Y))]); observed
sum(table(Y)) == sum(observed) # ? chec that they both equal 40, which they do

# Note that while we count success, R does not (different ways of parametrizing the geometric distributuion
# relative to R).  Therefore, for our expected vector, we will treat our bins as matching those that are 
# already created: 0,1,2 to 3, >= 4
p <- 1/4
expected <- 40*c(dgeom(0:1, p),
                 dgeom(2,p) + dgeom(3,p),
                 1-pgeom(3,p))
sum(expected) # sums to 40!

barplot(rbind(observed, expected), beside = TRUE, col = c("green", "purple"))

# Define and compute Chi-Squared Statistic
ChiSq <- function(Obs, Exp){
  sum((Obs - Exp) ^ 2 / Exp)
}
chisq <- ChiSq(observed, expected); chisq # value is 1.788889 which is very low
# There are 4 categories; we set the expected total equal to the actual total
# and then we computed p from the data.  Therefore, we have 4-2 = 2 degrees of freedom

# Compute the p-value
pvalue <- pchisq(chisq, df = 2, lower.tail = FALSE); pvalue # 0.4088347
# The p-value is 0.4088347, which means that the the probability of observing a test statistic
# that is as extreme from the relevant Chi-square distribution is 40.88347%.  As such, we can
# say that our data provides insufficient evidence against the null hypothesis which says that
# the data came from a geometric distribution


# Questions 1 (b)




# Questions 2 (a)

rm(list = ls()) # clear console

P <-c((6/pi^2)*(1/(1:999)^2), .0006082); head(P) #create vector
sum(P) # These probabilities sum to 1 as a check
Expt <- sum((1:1000) * P); Expt # 5.158213
Expt2 <- sum((1:1000)^2 * P); Expt2 # 1215.519

# Find the contributionof the tiny probability X = 1000
(1000 ^ 2 * P[1000])/Expt2 # 50.03623% which is just a bit over 50% as desired


# Questions 2 (b)

Z <- sample(x = 1:10^3,
            size = 10^4,
            prob = P,
            replace = TRUE)

hist(Z, breaks = 100, probability = TRUE, col = "purple")


# Questions 2 (c)

# Load the input file
# setwd("/Users/adegade/OneDrive/Documents/Education/Harvard/Software Engineering/Spring-2020-MATH-E-23C/Homework")
FlightDelays <- read.csv("FlightDelays.csv"); head(FlightDelays)

Delay <-FlightDelays$Delay # create a variable for the Delay column of the data set

# Create  a histogram of the Delay column
hist(Delay, breaks = "FD", prob = TRUE, col = "Red") # similar output to the histogram in 2(b) above!


# Questions 2 (d)

X <- 1:10^3 # create a vector

# Create two samples of X and compute their difference
first <- sample(X,
                size = 10^4,
                prob = P,
                replace = TRUE)
second <- sample(X,
                size = 10^4,
                prob = P,
                replace = TRUE)
diff <- first - second

# Create a histogram of the difference
hist(diff, breaks = 100, probability = TRUE, col = "orange")

var(diff) # 2554.294


# Questions 3

rm(list = ls()) # clear console

# install pakcages
library(resampledata)

# Get Lottery dataset
Lottery <- get("Lottery"); head(Lottery)
table(Lottery) # Counts are all greater than 5 so we are good

M <- nrow(Lottery) # number of rows in the Lottery data set

observed <- as.vector(table(Lottery)); observed

# Compute how many numbers we are working on
Length <- length(observed); Length # size of the observed vector
length(table(Lottery)) # size of the table of Lottery data

# There are 39 numbers, and our null hypothesesis is that they are all equally
# likely to be drawn

# Create the expected vectors, with each of its 39 elements haveing a probability of 
# 1/39 of being drawn.  There are M = 500 observations
expected <- M * c(rep(1/Length, Length)); expected
sum(expected); M # check that the sum of the expected equals 500 which it does

# Define and compute Chi-Squared Statistic
ChiSq <- function(Obs, Exp){
  sum((Obs - Exp) ^ 2 / Exp)
}
chisq <- ChiSq(observed, expected); chisq # value is 33.676 which is really large

df <- Length - 1; diff # degrees of freedom is 38
# note that there are N categories.  We made the expected total equal to the actual total number of 
# observations, so that we have N-1 degrees of freedom = 39 - 1 = 38

# Compute the p-value
pvalue <- pchisq(chisq, df, lower.tail = FALSE); pvalue # 0.669616
# The p-value is 0.669616, which means that the the probability of observing a test statistic
# that is as extreme from the relevant Chi-square distribution is 66.9616%.  As such, we can
# say that our data provides insufficient evidence against  the null hypothesis which says that
# the data came from a uniform distribution


# Questions 4

rm(list = ls()) # clear console

# Load the input file
# setwd("/Users/adegade/OneDrive/Documents/Education/Harvard/Software Engineering/Spring-2020-MATH-E-23C/Homework")
Redsox <- read.csv("RedSox2013.csv"); head(Redsox)
Nats <- read.csv("Nats2019.csv"); head(Nats)

# Extracting Runs and Runs Against columns into a single data vector
Y <- c(Redsox$R, Redsox$RA, Nats$R, Nats$RA)
Length <- length(Y) # 680, which is > 600


# Questions 4 (a)

# Bar plot showing the number of runs scored by the teams
barplot(table(Y),
        xlab = "Number of Runs",
        ylab = "Number of Games",
        col = "blue")


# Questions 4 (b)

# Note that if the data above is modeled by a Poisson distribution then lamda will be its mean
lamda <- mean(Y); lamda # value is 4.738235

# Note that for a poisson ramdom variable Y, E(Y) = Var(Y) = lamda
var(Y) # value of variance is 11.14493 which means a poisson is not the way to go here


# Questions 4 (c)

table(Y) 

# Lets us select number of runs sets that occurred in at 5 games as a general rule of thumb 
# for the chi-squared test.  We will pool together data >= 14 so that we have 15 bins in total
# Our bins will be 0,1,2,3,4,5,6,7,8,9,10,11,12,13 and >= 14
observed <- numeric(15) # create an empty vector
for (i in 1:14) {
  observed[i] <- table(Y)[i]
}
observed[15] <- sum(table(Y)[15:20])
observed; table(Y) # check values
sum(observed) == Length # chek that the sum of observed is the same size as Y

# Note that Poisson takes on integer values
# P(Y >= 14) = P(Y > 13)
expected <- Length * c(dpois(0:13, lamda), ppois(13, lamda, lower.tail = FALSE))

# Checking calculations above
sum(dpois(0:13, lamda)) + ppois(13, lamda, lower.tail = FALSE) # sum of probabilities equals to 1
sum(expected) == Length # length of the expected vector and Y are equal as well <- 680

df <- 15-3 # degrees of freedom

# Define and compute Chi-Squared Statistic
ChiSq <- function(Obs, Exp){
  sum((Obs - Exp) ^ 2 / Exp)
}
chisq <- ChiSq(observed, expected); chisq # value is 1101.949 which is really large

pvalue <- pchisq(chisq, df, lower.tail = FALSE); pvalue # 2.213952e-228
# The p-value is really close to zero, which means that the the probability of observing a test statistic
# that is as extreme from the relevant Chi-square distribution is essentially zero.  As such, we can
# reject the null hypothesis that the data came from a Poisson distribution with parameter lamda 

