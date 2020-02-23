rm(list = ls()) # clear console


# ADETAYO ADEGOKE
# Data wrangling 03 - A permutation test: we have a sample but know nothing about the population
# Due: March 6, 2020
# Mathematics E-23c, Spring 2020


# (3) Find on the Internet some data in which you can identify two groups, com-pare means or medians for some 
#     numeric quantity, and carry out a permu-tation test to assess whether or not the difference is statistically 
#     significant. Save a .cvs file that includes just the data that you are analyzing. The prototype is 
#     the analysis of Beerwings.csv in script 2D.


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Data Wrangling 03 using the male and female  character words in movie scripts
# https://github.com/matthewfdaniels/scripts/blob/graphs/character_list5.csv.
# More dataset background here: https://pudding.cool/2017/03/film-dialogue/
# This data set included 5 variables.  There descriptions are shown below 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# 
# (1) script_id (film script ID)                                    (2) imdb_character_name (character name from IMDB)
# (3) words (number of words assigned to character in film script)  (4) gender (gender of character)
# (5) age (age of character)
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Load the input file
# setwd("/Users/adegade/OneDrive/Documents/Education/Harvard/Software Engineering/Spring-2020-MATH-E-23C/Data Wrangling")
Moviechar <- read.csv("character_list5.csv"); head(Moviechar) 


# STARTED compare means or medians for some numeric quantity, and carry out a permu-tation test to assess whether 
# or not the difference is statistically significant. Save a .cvs file that includes just the data that you are analyzing

attach(Moviechar)

# Check to see if there are rows of data with missing values
Moviechar[!complete.cases(Moviechar),] # Returns 0 rows, which confirms that the data set is complete without NA or blank entries
index <- which(age != "NULL" & gender != "?"); head(index) # select rows where the age column has numeric data and not NULL text
length(index)    # how many rows did we select? 18,623
MoviecharCleaned <- Moviechar[index,]

# Generate slimmed down file with just data being analyzed
Words <- MoviecharCleaned$words
Gender <- MoviecharCleaned$gender
MovieCharacterFile <- data.frame(Gender, Words); head(MovieCharacterFile)
write.csv(MovieCharacterFile,"MovieCharacters.csv")  # save csv file to disk

detach(Moviechar)

attach(MoviecharCleaned)

sum(gender == "m") # 12763. number of male characters in roughly 2000 films
sum(gender == "f") # 5498. number of male characters in roughly 2000 films
sum(gender == "m") + sum(gender == "f") == length(index) # sanity check to see if m or f was populated for all rows in clean data set

# Calculate the observed words spoken by film script difference by gender
MaleAvg <- sum(words* (gender == "m")) / sum(gender == "m"); MaleAvg # 1079.046
FemaleAvg <- sum(words * (gender == "f")) / sum(gender == "f"); FemaleAvg # 981.0862
observed <- MaleAvg - FemaleAvg; observed     # male characters spoke more words: 97.96025

# Now replace Male with a random sample of 500 characters
Gender <- sample(gender, 500, replace = TRUE); head(Gender)   # permuted gender column
sum(Gender == "m")  # 333 men but they will match up with random beer consumption
sum(Gender == "m") + sum(Gender == "f") == 500
MaleAvg <- sum(words* (Gender == "m")) / sum(Gender == "m"); MaleAvg # 38351.09
FemaleAvg <- sum(words * (Gender == "f")) / sum(Gender == "f"); FemaleAvg # 38293.23
MaleAvg - FemaleAvg    # 57.86255. As likely to be negative or positive

N <- 10^4 # Repeat 10000 times
diffs <- numeric(N)
for (i in 1:N) {
  Gender <- sample(gender)   #permuted gender column
  MaleAvg <- sum(words* (Gender == "m")) / sum(Gender == "m")
  FemaleAvg <- sum(words * (Gender == "f")) / sum(Gender == "f")
  diffs[i] <- MaleAvg - FemaleAvg    # as likely to be negative or positive
}
mean(diffs) # should be close to zero.  0.1099083

hist(diffs, breaks = "FD", col = "blue")

# Now display the observed difference on the histogram
abline(v = observed, col = "purple") # at the far right of the histogram so line might not display

# What is the probability (the P value) that a difference this small
# could have arisen with a random subset?
pv1t <- (sum(diffs >= observed)+1)/(N+1); pv1t # 9.999e-05
pv2t <- pv1t * 2; pv2t # 2-tailed p value is 0.00019998

# The two-tailed p-value indicates that there is a very low chance that we could come by the observed difference
# between the mean number of words spoken by male and female film characters by chance (only a probability of 
# 0.00019998 which is a lot less than .05).  As such, we have sufficient evidence against the null hypothesis 
# which is there no difference in the mean number of words spoken by male and female film characters
# and instead suggest that the alternative hypothesis is true which is that there is indeed a difference in the 
# mean number of words spoken by male and female film characters.  The one-tailed p-value suggests that male film
# characters do say more words in films than female film characters. 

detach(MoviecharCleaned)

# COMPLETED compare means or medians for some numeric quantity, and carry out a permu-tation test to assess whether 
# or not the difference is statistically significant. Save a .cvs file that includes just the data that you are analyzing
