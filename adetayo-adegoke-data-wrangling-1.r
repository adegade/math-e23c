rm(list = ls()) # clear console


# ADETAYO ADEGOKE
# Data wrangling 01 - Building and analyzing a data frame with nothing but logical columns
# Due: February 19, 2020
# Mathematics E-23c, Spring 2020


# (1) Download from the Internet the General Social Survey (GSS) for a year other than 2002. 
#     Extract from it a dataframe with four to six factor or logical columns and save it to a .csv file. 
#     Then, as in script 1D, topic 1, find or count rows with specified features, 
#     and form and apply a chi-square test to a couple of 2 times 2 contingency tables


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Data Wrangling 01 using the GSS data set for 2018 https://gssdataexplorer.norc.org/projects
# 23 variables were selected for my data set and are defined as follows using my data labels for simplicity
# and using the same description in brackets as the original 2018 GSS data set from norc.org: 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# (01) Ballot (Ballot used for interview),                    (02) ID (Respondent id number), 
# (03) Year (Gss year for this respondent),                   (04) Region (Region of interview), 
# (05) Gender (Respondents gender),                           (06) Race (Race of respondent), 
# (07) Education (Respondents highest degree),                (08) Marital (Marital status), 
# (09) Religion (Respondents religious preference),           (10) Happy (General happiness), 
# (11) Income (Total family income),                          (12) PolParty (Political party affiliation),
# (13) Politics (Think of self as liberal or conservative),   (14) OwnGun (Have gun in home), 
# (15) GunLaw (Favor or oppose gun permits),                  (16) SpendMilitary (Military, armaments, and defense),
# (17) SpendEduc (Improving nations education system),        (18) SpendEnv (Improving & protecting environment), 
# (19) SpendSci (Supporting scientific research)              (20) SpendSpace (Space exploration program),
# (21) SpendDrug (Dealing with drug addiction),               (22) Pres12 (Vote obama or romney),
# (23) Postlife (Belief in life after death)
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Load the input file
# setwd("/Users/adegade/OneDrive/Documents/Education/Harvard/Software Engineering/Spring-2020-MATH-E-23C/Data Wrangling")
GSS <- read.csv("GSS2018.csv"); head(GSS) 


# COMPLETED Extract from it a dataframe with four to six factor or logical columns and save it to a .csv file. 

attach(GSS)

# Identify the rows with non-empty entries for Gender, Marital, Religion, PolParty, OwnGun, and Pres12
index <- which(!is.na(Gender) & !is.na(Marital)& !is.na(Religion)&
               !is.na(PolParty)& !is.na(OwnGun)& !(OwnGun=="Refused") & 
               !is.na(Pres12) & ((Pres12 == "Obama")|(Pres12 == "Romney"))); head(index)

# Check the number of row collected that have non empty entries for the specified variables
# is it the same length as or shorter than the number of rows in GSS.  Sanity check ... yes it is!
length(index) # 1375
nrow(GSS) # 2348
length(index) <= nrow(GSS)

GSSClean <- GSS[index,] # get the cleaned up data

# Extract the six desired columns as vectors, keeping just the selected rows
Gender2 <- Gender[index] 
Marital2 <- Marital[index] 
Religion2 <- Religion[index] 
PolParty2 <- PolParty[index] 
OwnGun2 <- OwnGun[index] 
Pres122 <- Pres12[index] 

detach(GSS)

# Using the vectorized == operator to create six logical columns
Male <- Gender2 == "Male"; head(Gender2); head(Male)  #check that it is working
Married <- Marital2 == "Married"
Protestant <- Religion2 == "Protestant"
GunOwner <- OwnGun2 == "Yes"
Obama <- Pres122 == "Obama"

# Extracting party is tricky
PolParty2    # there are three flavors of Republican
Republican <- (PolParty2=="Strong republican") | (PolParty2=="Ind,near rep") |
  (PolParty2=="Not str republican")
head(PolParty2); head(Republican) # check that it is working

# Combine the six logical vectors into a data frame
GSSLogical <- data.frame(Male, Married, Protestant, Republican, GunOwner, Obama)
head(GSSLogical)

#Save in a file for possible use on the homework
write.csv(GSSLogical,"GSSLogical.csv")  # look at this in Excel or as a text file

# COMPLETED Extract from it a dataframe with four to six factor or logical columns and save it to a .csv file. 


# STARTED Then, as in script 1D, topic 1, find or count rows with specified features

attach(GSSLogical)

# Now we can use logical operations to search for rows
Guns <- which(GunOwner); head(Guns); length(Guns)  # all gun owners.  346 in total
GunsObama <- which(GunOwner & Obama); head(GunsObama); length(GunsObama) # use AND to find gun owners who voted for Obama.  194 in total
GunsRomney <- which(GunOwner & !Obama); head(GunsRomney); length(GunsRomney) # use AND along with NOT to find gun owners who voted for Romney.  152 in total
# More gun owners voted for Obama than Romney.  Interesting!
(length(GunsObama) + length(GunsRomney)) == length(Guns) # sanity check to see if people that are gun owners that voted for Obama and Romney 
# equals to the total number of gun owners.  Accurate check => (194 + 152) = 346

# COMPLETED Then, as in script 1D, topic 1, find or count rows with specified features


# STARTED form and apply a chi-square test to a couple of 2 times 2 contingency tables

# First example: did gun owners vote for Obama?
tbl <- table(GunOwner, Obama); tbl # show all four alternatives in a contingency table
Expected <- outer(rowSums(tbl), colSums(tbl)) / sum(tbl); Expected # compare with the table that would be expected if the factors were independent
# these tables look quite different. Is the difference significant?  Lets examine the differnece using the chisq.test function
chisq.test(GunOwner, Obama)
# The low p-value means there is about 8.508 chance in 10,000,000 that the difference betwen the tables above arose by chance

# Second example: did Republicans vote for Obama?
tbl <- table(Republican, Obama); tbl # show all four alternatives in a contingency table
Expected <- outer(rowSums(tbl), colSums(tbl)) / sum(tbl); Expected # compare with the table that would be expected if the factors were independent
# these tables look quite different. Is the difference significant?  Lets examine the differnece using the chisq.test function
chisq.test(Republican, Obama) # even more extreme!
# The low p-value means there is about 2.2 chance in 10,000,000,000,000,000 that the difference betwen the tables above arose by chance

# Third example: is protestantism independent of gender?
tbl <- table(Protestant, Male); tbl # show all four alternatives in a contingency table
Expected <- outer(rowSums(tbl), colSums(tbl)) / sum(tbl); Expected # compare with the table that would be expected if the factors were independent
# these tables look quite different. Is the difference significant?  Lets examine the differnece using the chisq.test function
chisq.test(Protestant, Male) # 4.1% p-value
# The high p-value means there is about 41 chance in 1,000 that the difference betwen the tables above arose by chance

# Fourth example: is marriage status of "Married" independent of gender?
tbl <- table(Married, Male); tbl # show all four alternatives in a contingency table
Expected <- outer(rowSums(tbl), colSums(tbl)) / sum(tbl); Expected # compare with the table that would be expected if the factors were independent
# these tables look quite different. Is the difference significant?  Lets examine the differnece using the chisq.test function
chisq.test(Married, Male) # 0.1773% p-value
# The high p-value means there is about 1.773 chance in 1,000 that the difference betwen the tables above arose by chance

detach(GSSLogical)

# COMPLETED form and apply a chi-square test to a couple of 2 times 2 contingency tables

