rm(list = ls()) # clear console


# ADETAYO ADEGOKE
# WEEK 02 Homework - Foundations of probablity for finite sample spaces
# Due: February 10, 2020
# Mathematics E-23c, Spring 2020
# Michael Liotti Office Hour Review


# Questions 1 (a)

# Creating two columns representing the quiz scores 
# Both quizes have 0.25 probablity of getting 100 and 0.75 probablity of getting 0
Q1 <- c(100,0,0,0) 
Q2 <- Q1

# create the dataframe with colmns Q1 and Q2.  It should have 16 rows
ttbl <- expand.grid(Q1, Q2); ttbl 
Q1 < - ttbl$Var1; Q2 <- ttbl$Var2
ttbl <- data.frame(Q1, Q2); ttbl # rename the columns


# Question 1 (b)

# Column X is the random variable the average of the quiz scores: 100, 50 or 0
X = (Q1 + Q2)/2
ttbl$X <- X; ttbl

# Column Y is the improvement rating random variable
# Y is 0 if Quiz 2 score is worse
# Y is 1 if Quiz 1 and 2 scores are the same
# Y is 2 if Quiz 2 score is better
Y <- ifelse(Q2 < Q1, 0, ifelse(Q2==Q1,1,2)); Y
ttbl$Y <- Y; ttbl


# Question 1 (c)

# X and Y are uncorrelated if E(XY) - E(X)E(Y) = 0
# Check to see if this is true
(mean(X*Y) - (mean(X) * mean(Y))) == 0 # True, hence X and Y random variables are uncorrelated


# Question 1 (d)

# X and Y are independent if E(X^2 * Y^2) - E(X^2)E(Y^2) = 0
# Check to see if this is true
(mean((X^2) * (Y^2)) - (mean(X^2) * mean(Y^2))) == 0 # False, hence X and Y random variables are not independent


# Question 1 (e)

A <- ifelse(X < Q2, 1, 0) # create an indicator random variable involving X
B <- ifelse((Y == 0) | (Y == 1), 1, 0) # create an indicator random variable involving Y
ttbl$A <- A; ttbl$B <-B; ttbl # add these variables to the dataframe
PAandB <- sum(A*B) / nrow(ttbl); PAandB
PA <- sum(A) / nrow(ttbl); PA # find the probability of A
PB <- sum(B) / nrow(ttbl); PB # find the probability of B
PAandB == (PA * PB) # False, hence X and Y random variables are not independent


# Question 2 (a)

rm(list = ls()) # clear console

# Checking question 4 in the pencil and paper homework (pnp hw)
# m = 6, n = 20, k = 13

# Checking P(X = 3) using dhyper() and comparing with our computation in pnp hw
dhyper(3, 6, 20, 13) # result is 0.3552795
(choose(6, 3) * choose(20, 10)) / choose(26, 13) # result is 0.3552795
# We have a match!

# Checking P(X = 2) using dhyper() and comparing with our computation in pnp hw
dhyper(2, 6, 20, 13) # result is 0.242236
(choose(6, 2) * choose(20, 11)) / choose(26, 13) # result is 0.242236
# We have a match!

# Checking P(X = 1) using dhyper() and comparing with our computation in pnp hw
dhyper(1, 6, 20, 13) # result is 0.07267081
(choose(6, 1) * choose(20, 12)) / choose(26, 13) # result is 0.07267081
# We have a match!

# Checking P(X = 0) using dhyper() and comparing with our computation in pnp hw
dhyper(0, 6, 20, 13) # result is 0.007453416
(choose(6, 0) * choose(20, 13)) / choose(26, 13) # result is 0.007453416
# We have a match!


# Question 2 (b)

m <- 6 # number of trump cards - type 1 objects
n <- 20 # number of non-trump cards - type 2 objects
k <- 13 # number of cards in hand

Player <- c(rep("EAST", k), rep("WEST", m + n - k)); Player # create the ownership column
Trumps <- c(rep(TRUE, 6), rep(FALSE, 20)); Trumps

# Set up key variables for running the simulation to estimate P(X = 3)
N <- 10000 
X <- numeric(N)

# Execute the simulation
for (i in 1:N){
  scramble <- sample(Trumps, m + n, replace = FALSE)  #permute the trumps
  X[i] <- sum((Player == "EAST") & scramble)
}
(table(X)[4])/N    # 0.3562, which is really close to results computed earlier on of 0.3552795


# Question 2 (c)

# Setup the second column so that East has two trumps
East <- c(rep(TRUE, k), rep(FALSE, m + n - k)); East # create the ownership column
Trumps <- c(rep(TRUE, 2), rep(FALSE, 20), rep(TRUE, 4))

# Contigency Table
table(East, Trumps)

# Running the Fisher exact testto find the probability that East has two or more trumps
fisher.test(East, Trumps, alternative = "g")
# The test has a p-value of 0.9199, which is the probablity that East has two or more trumps

# Computing dhyper() to compare results above to
# Note that P(X >= 2) = 1 - P(X < 2) = 1 - P(X <= 1) since X must be an integer
1 - phyper(1, 6, 20, 13) # computest to 0.9198758 which is close to the the fisher.test results above of 0.9199


# Question 3 (a)

rm(list = ls()) # clear console

# load the input file
setwd("/Users/adegade/OneDrive/Documents/Education/Harvard/Software Engineering/Spring-2020-MATH-E-23C/Homework")
NC <- read.csv("NCBirths2004.csv"); head(NC) 

idx <- which(NC$Gender == "Male") # create an index for extracting gender from the data set
mmbw <- mean(NC$Weight[idx]); mmbw # compute male babies mean birth weight - 3501.58

# doing a similar exercise for female babies
# another way to do this is fmbw <- mean(NC$Weight[-idx])
idx <- which(NC$Gender == "Female") # create an index for extracting gender from the data set
fmbw <- mean(NC$Weight[idx]); fmbw # compute female babies mean birth weight - 3398.317

wdiff <- mmbw - fmbw; diff # mean birth weight difference is 103.2632, just over 100

# Execute a permutation test to determine if the difference in mean shown above is statistically significant
N <- 10000 # number of experiements to run
diff <- numeric(N) # empty vector to store results

for (i in 1:N) {
  gender <- sample(NC$Gender) # permute the gender labels
  mbavg <- sum(NC$Weight * (gender == "Male")) / sum(gender == "Male"); mbavg # average weight for males
  fbavg <- sum(NC$Weight * (gender == "Female")) / sum(gender == "Female"); fbavg # average weight for females
  diff[i] <- mbavg - fbavg # store the average weight difference in the vector
}

mean(diff) # -0.06855603, which is close to 0
hist(diff, breaks = "FD")
abline(v= wdiff, col = "red") # observed difference is way out on the tail

pv1t <- (sum(diff >= wdiff) + 1) / (N + 1); pv1t # 1-tailed p-value is 0.00019998
pv2t <- pv1t * 2; pv2t # 2-tailed p value is 0.00039996

# The two-tailed p-value indicates that there is a very low chance that we could come by the observed differnece
# in mean birth weights by chance (only a probability of 0.00039996 which is a lot less than .05).  As such, we 
# have sufficient evidence against the null hypothesis which is there no difference in the mean birth weights
# and instead suggest that the alternative hypothesis is true which is that there is indeed a difference in the 
# mean birth weights.  The one-tailed p-value suggests that male babies do have a larger mean baby weight than
# female babies


# Question 3 (b)

# Determine whether tobacco use by the mother has a statisticallysignificant effect on birth weight

tb <- NC$Tobacco # extract the tobacco column
bw <- NC$Weight # extract the baby weight column

# Compute the average weight of babies whose mothers used tobacco
idxt <- which(NC$Tobacco == "Yes") # create an index for extracting tobacco use from the data set
bwt <- mean(bw[idxt]); mmbwt # compute the babies mean birth weight whose mothers used tobacco - 3256.91

# doing a similar exercise for female babies
# another way to do this is bwnt <- mean(bw[-idxt])
idxnt <- which(NC$Tobacco == "No") # create an index for extracting tobaco use from the data set
bwnt <- mean(bw[idxnt]); bwnt # compute the babies mean birth weight whose mothers did not use tobacco - 3471.912

wdifft <- bwnt - bwt; wdifft # mean birth weight difference is 215.0021

# Execute a permutation test to determine if the difference in mean shown above is statistically significant
N <- 10000 # number of experiements to run
difft <- numeric(N) # empty vector to store results
for (i in 1:N) {
  tobacco <- sample(tb) # permute the tobacco use labels
  tavg <- sum(bw * (tobacco == "Yes")) / sum(tobacco == "Yes"); tavg # average weight for babies whose mothers used tobacco
  ntavg <- sum(bw * (tobacco == "No")) / sum(tobacco == "No"); ntavg # average weight for babies whose mothers did not use tobacco
  difft[i] <- ntavg - tavg # store the average weight difference in the vector
}

mean(difft) # 0.6645301
hist(difft, breaks = "FD")
abline(v= wdifft, col = "blue") # observed difference is way out on the right tail

pv1tt <- (sum(difft >= wdifft) + 1) / (N + 1); pv1tt # 1-tailed p-value is 9.999e-05
pv2tt <- pv1tt * 2; pv2tt # 2-tailed p value is 0.00019998

# The two-tailed p-value indicates that there is a very low chance that we could come by the observed differnece
# in mean birth weights by chance (only a probability of 0.00019998 which is a lot less than .05).  As such, we 
# have sufficient evidence against the null hypothesis which is there no difference in the mean birth weights
# and instead suggest that the alternative hypothesis is true which is that there is indeed a difference in the 
# mean birth weights based on tobacco usage.  The one-tailed p-value suggests that babies whose mothers do not use
# tobacco have a mean baby weight that is larger than babies whose mothers do uses tobacco


# Question 4

rm(list = ls()) # clear console

# load the input file
setwd("/Users/adegade/OneDrive/Documents/Education/Harvard/Software Engineering/Spring-2020-MATH-E-23C/Homework")
Battle <- read.csv("Battle.csv"); head(Battle) 

champs <- "MEZU"
swords <- "BRIG"
sset <- subset(Battle, (Battle$Abbr == champs | Battle$Abbr == swords))
score <- sset$Kills - sset$Lost

idxchamps <- which(sset$Abbr == champs) # create an index for extracting MEZU aka champs rows from the data set

msdiff <- mean(score[idxchamps]) - mean(score[-idxchamps]); msdiff # -14.69173 - mean score difference between champs and swordsmen

# Execute a permutation test to determine if the difference in mean shown above is statistically significant
N <- 10000 # number of experiements to run
diffms <- numeric(N) # empty vector to store results
numchamps <- sum(sset$Abbr == champs); numchamps

for (i in 1:N) {
  idxchamps <- sample(nrow(sset), numchamps)
  diffms[i] <- mean(score[idxchamps]) - mean(score[-idxchamps])
}

mean(diffms) # -0.03305469 which is close to 0
hist(diffms, breaks = "FD")
abline(v= msdiff, col = "purple") 

pv1ms <- (sum(diffms <= msdiff) + 1) / (N + 1); pv1ms # 1-tailed p-value is 0.01689831
pv2ms <- pv1ms * 2; pv2ms # 2-tailed p value is 0.03379662

# The two-tailed p-value indicates that there is a low chance that we could come by the observed differnece
# in mean score (a probability of 0.03379662 which is less than .05).  As such, we have sufficient evidence 
# against the null hypothesis which is there no difference in the unit superiority  and instead suggest that 
# the alternative hypothesis is true which is that there is indeed a difference in the # unit superiority.  
# The one-tailed p-value suggests that babies the swordsmen unit are more skilled than the champions unit in 
# terms of kills - losses
