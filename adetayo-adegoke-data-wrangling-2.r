rm(list = ls()) # clear console


# ADETAYO ADEGOKE
# Data wrangling 02 - Building and analyzing a data frame with logical and numeric columns
# Due: March 6, 2020
# Mathematics E-23c, Spring 2020


# (2) Find  on  the  Internet  some  data  about  the  sport  of  your  choice.  Create a data frame with some factor columns, 
#     some numeric columns, and some logical columns, and save it as a .csv file.  Calculate some statistics in the style  of
#     Script 1D, topic 2  (the Red Sox data).   If you do this later in the course, you can probably do some more sophisticated 
#     analysis, but the requirement is simply to create a useful .csv file and do a bit of analysis on it.


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Data Wrangling 02 using the 1995-96 Regular Season Chicago Bulls Game Log data set from https://www.basketball-reference.com/teams/CHI/1996/gamelog/
# This data set included 39 variables.  There descriptions are shown below 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#
# Chicago Bulls Team Variables
#
# (01) Rk (Rank)                    (02) G (Season Game)                        (03) Opp (Opponent)                           (04) Tm (Points scored)
# (05) Opp (Opponent Points)        (06) FG (Field Goals)                       (07) FGA (Field Goal Attempts)                (08) FGP (Field Goal Percentage)
# (09) 3P (3-Point Field Goals)     (10) 3PA (3-Point Field Goal Attempts)      (11) 3PP (3-Point Field Goal Percentage)      (12) FT (Free Throws)
# (13) FTA (Free Throw Attempts)    (14) FTP (Free Throw Percentage)            (15) ORB (Offensive Rebounds)                 (16) ORB (Offensive Rebounds)
# (17) TRB (Total Rebounds)         (18) AST (Assists)                          (19) STL (Steals)                             (20) BLK (Blocks)
# (21) TOV (Turnovers)              (22) PF (Personal Fouls)                     
# 
# Opponent Team Variables
# 
# (23) OFG (Opponent Field Goals)             (24) OFGA (Opponent Field Goal Attempts)              (25) OFGP (Opponent Field Goal Percentage)
# (26) O3P (Opponent 3-Point Field Goals)     (27) O3PA (Opponent 3-Point Field Goal Attempts)      (28) O3PP (Opponent 3-Point Field Goal Percentage)      (29) OFT (Opponent Free Throws)
# (30) OFTA (Opponent Free Throw Attempts)    (31) OFTP (Opponent Free Throw Percentage)            (32) OORB (Opponent Offensive Rebounds)                 (33) OORB (Opponent Offensive Rebounds)
# (34) OTRB (Opponent Total Rebounds)         (35) OAST (Opponent Assists)                          (36) OSTL (Opponent Steals)                             (37) OBLK (Opponent Blocks)
# (38) OTOV (Opponent Turnovers)              (39) OPF (Opponent Personal Fouls)
# 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# Load the input file
# setwd("/Users/adegade/OneDrive/Documents/Education/Harvard/Software Engineering/Spring-2020-MATH-E-23C/Data Wrangling")
Bulls <- read.csv("ChiBulls96.csv"); head(Bulls) 


# STARTED Create a data frame with some factor columns, some numeric columns, and some logical columns, and save it as a .csv file

attach(Bulls)

# Check to see if there are rows of data with missing values
Bulls[!complete.cases(Bulls),] # Returns 0 rows, which confirms that the data set is complete without NA or blank entries

nrow(Bulls) # There were 82 games for the Chicago Bulls in the 1995-1996 Regular Season

# Use the vectorized ==, >, >=, < operators to make logical columns from numeric ones
Won <- WonLost == "W"; head(WonLost); head(Won)
ScoreMore100 <- Tm > 100; head(Tm); head(ScoreMore100)
LessTurnOver <- TOV < OTOV; head(TOV); head(OTOV); head(LessTurnOver)
DecentGoalAttempt <- FGP >= 0.5; head(FGAP); head(DecentGoalAttempt)

# Numeric columns are G (Season Game #), FTP (Free Throw Percentage) and PF (Personal Fouls)
# Factor column is Opp (Opponent Team Name/Label)
# Create another factor called winning streak
WinningStreak <- c(rep("Not Init", nrow(Bulls))); head(WinningStreak)
for (i in nrow(Bulls)) {
  WinningStreak <- ifelse(Won == TRUE, "Winning Streak", "No Winning Streak") # as soon as the Bulls lose a game winning streak is over :(
}
head(WinningStreak)

# Create the Bulls Logical data frame and save it to a file
BullsLogical <- data.frame(Won, ScoreMore100, LessTurnOver, DecentGoalAttempt, 
                         G, FTP, PF, 
                         Opp, WinningStreak); head(BullsLogical)
write.csv(BullsLogical,"BullsLogical")  # save csv file to disk

detach(Bulls)

# COMPLETED Create a data frame with some factor columns, some numeric columns, and some logical columns, and save it as a .csv file


# STARTED Calculate some statistics in the style  of Script 1D, topic 2  (the Red Sox data)

attach(BullsLogical)

# Search for  games lost in spite of scoring more than 100 points and a free throw success percentage greater than or equal to 50%
which(ScoreMore100 & (FTP >= 0.5) & !Won) # Lost Game 54 and 68 under these conditions

# Look at one of these rows as a check
BullsLogical[54,]$Won # verified!  Won column for Game 54 is false
BullsLogical[68,]$Won # verified!  Won column for Game 68 is false

# By exploiting the fact that TRUE = 1, FALSE = 0 we can learn a lot

# Number of games with Bulls scoring > 100 points
sum(ScoreMore100)   # 55

# Number of games with Bulls successful field goal attempt % > 50%
sum(DecentGoalAttempt) # 30 

# Number of games with Bulls having less turnovers than the opposing team
sum(LessTurnOver) # 58.  

# Proportion of games won
mean(Won)   # 0.8780488

# Proportion of games with less turnover than the opposing team
mean(LessTurnOver) # 0.7073171

# Find the proportion of games won when scoring > 100 points
sum(Won * ScoreMore100) / sum(ScoreMore100) # 0.9636364

# Find the proportion of games won with less turnover than the opposing team
sum(Won * LessTurnOver) / sum(LessTurnOver) # 0.9137931

# create index of games that are wins
index <- which(Won == TRUE); index

# Compute the number of wins between games
swins <- sum(Won); swins
Y <- numeric(nrow(BullsLogical))
Y[1] <- index[1]; Y
for (i in 2:swins) {
  Y[i] <- index[i] - index[i - 1]
}
head(Y); head(index)  # checking the computation above for accuracy

table(Y) # checking out the distribution of the data

detach(BullsLogical)

# COMPLETED Calculate some statistics in the style  of Script 1D, topic 2  (the Red Sox data)

