rm(list = ls()) # clear console


# ADETAYO ADEGOKE
# WEEK 04 Homework - The Riemann integral and its generalizations
# Due: February 26, 2020
# Mathematics E-23c, Spring 2020
# Michael Liotti Office Hour Review


# Questions 1 (a)
# use the integrate() function to show that for a normal distribution the kurtosis is 0

sigma <- 2 # variance is 4
mu <- 0 # set the mean to 0

# define function for the fourth central element
f <- function(x) {
  (x-mu) ^ 4 * dnorm(x, mean = mu, sd = sigma)
}
fcm <- integrate(f, -Inf, Inf)$value # integrated to get the fourth central moment
fcm # 48

fcmk <- fcm/(sigma^4) - 3 # using the kurtosis formula from class for computation
round(fcmk) # the kurtosis for a normal distribution is very close to 0!


# Questions 1 (b)
# use the integrate function to find var(x) and kurtosis for a uniform distribution

# Let us consider a uniform random variable over [-1,1].
# density is 1/(b-a) = 1/(1-(-1)) = 1/2 for -1 <= x <= 1
# Var(X) = E(X^2) - [E(X)]^2

# Get the second moment first
scm <- function(x) {
  (x ^ 2) * dunif(x, min = -1, max = 1)
}
EX2 <- integrate(scm, -1, 1)$value; EX2 # and then integrate to get E(X^2)
# we get a value 0.3333333

# Next, we compute the mean mean, and integrate to get E(X)
m <- function(x) {
  x * dunif(x, min = -1, max = 1)
}
EX <- integrate(m, -1, 1)$value # integrate to compute the expected value E(X)
EX # value is 0

(-1+1)/2 # checks out with the computation above

# Lets compute the variance next
VarX <- EX2 - EX^2; VarX # 0.3333333

# Lets recompute the fourth central moment again
f <- function(x) {
  (x - mu) ^4 * dunif(x, min = -1, max = 1) # define the interval [-1,1]
}
fcm.uni <- integrate(f, -Inf, Inf)$value # integrated to get the fourth central moment
fcm.uni # 0.2

# compute the kurtosis
fcm.uni/(VarX)^2 - 3 # -1.2


# Questions 2 (a)

rm(list = ls()) # clear console

# install.packages("resampledata")
library(resampledata)
head(Service)

# Extract time columns
Times <- Service$Times

# Plot the Histogram of Wait Times
hist(Times, probability = TRUE, col = "blue", breaks = "FD")


# Questions 2 (b)

# Define gamma parameters shape r and rate lambda
r <- 2.65
lambda <- 3.81

# overlay a graph of the density curve for the gamma distribution over the histogram plot
curve(dgamma(x, shape = r, rate = lambda), add = TRUE)


# Questions 2 (c)

mean(Times) # mean is 0.6949234
var(Times) #  variance is 0.1819025

# define function to compute expectation E(X) of this gamma distribution
f <- function(x) {
  x * dgamma(x, shape = r, rate = lambda)
}
EX <- integrate(f, 0, Inf)$value # integrate to get expectation
EX # 0.6955381
mean(Times) # 0.6949234
# Expectation and the mean for this gamma distribution matches the data quite well

# Var(X) = E(X^2) - [E(X)]^2
# Define function to compute E(X^2) of this gamma distribution
f.2 <- function(x) {
  x^2 * dgamma(x, shape = r, rate = lambda)
}
EX2 <- integrate(f.2, 0, Inf)$value; EX2 # 0.6663291 is the second moment

# Compute the variances
EX2-(EX)^2 # 0.1825559
var(Times) # 0.1819025
# Expectation and the variance for this gamma distribution matches the data quite well


# Questions 2 (d)

# save proposed gamma parameters to objects
r <- 2.65; lambda <- 3.81

# create the bin division points with deciles which guarantees that our counts are sufficiently large
bins <- qgamma(0.1 * (0:10), r, lambda); bins

# determine which bins the data belongs to
bincode <- cut(Times, bins, labels = FALSE); bincode

# construct obeserved data
observed <- as.vector(table(bincode)); observed

# construct expected data
expected <- rep(sum(observed)/10, 10); expected

# compute the chi-squared statistic
chisq <- sum((observed-expected)^2/expected); chisq # 1.977011
# We got 1.977011 Hence there is 97.70% probability of observing a test statistic this extreme from 
# the relevant chi-square distribution.  Hence, we have insufficient evidence against the null hypothesis 
# is that our data came from a gamma distribution


# Questions 3 (a)
# Make a histogram of delta and estimate its expectation and variance with 10K trials

rm(list = ls()) # clear console

N <- 10^4 # trials
delta <- numeric(N) # store the mesh

for (i in 1:N) {
  P <- sort(c(0, 1, runif(3))) # generate the partition
  delta[i] <- max(P[2]-P[1],
                  P[3]-P[2],
                  P[4]-P[3],
                  P[5]-P[4]) # compute the mesh and save it to delta
}

# Plot the Histogram of Delta
hist(delta, probability = TRUE, col = "orange")

# Compute the expectation of Delta
mean(delta) # 0.5227536

# Compute the Variance of Delta
var(delta) # 0.01669584


# Questions 3 (b)

# Create the function f(x) = x^2
f <- function(x) {
  x^2
}

# Create a function for the left Riemann sum
RiemannLeft <- function(P) {
  # Note that the input is some partition P
  result <- 0
  # loop over the intervals
  for (i in 1:4) {
    result <- result + (f(P[i]) * (P[i+1]-P[i]))
  }
  # return the result
  return(result)
}

# create a function for the right Riemann sum
RiemannRight <- function(P) {
  # Note that the input is some partition P
  result <- 0
  # loop over the intervals
  for (i in 1:4) {
    result <- result + (f(P[i+1]) * (P[i+1]-P[i]))
  }
  # return the result
  return(result)
}

# create a function for the midpoint riemann sums
MidRiemann <- function(P) {
  # Input is some partition P
  result <- 0
  # loop over the intervals
  for (i in 1:4) {
    result <- result + (f(P[i] + P[i+1])/2) * (P[i+1]-P[i])
  }
  # return the result
  return(result)
}

# For 10K trials, compute the midpoint riemann sum and create histogram

N <- 10^4 # number of trials
Y <- numeric(N) # store the mesh

for (i in 1:N) {
  # Generate  and sort division/partition points
  P <- sort(c(0, 1, runif(3)))
  # compute the mesh and save it to delta
  Y[i] <- MidRiemann(P)
}

# create histogram
hist(Y, probability = TRUE, breaks = "FD", col = "brown")

# write a function for the simpson's rule
SimpsonRsum <- function(P) {
  # Note that the input for this function is partition P
  TrapRsum <- (RiemannLeft(P) + RiemannRight(P))/2 # Compute trapexzoidal rsum
  # Simpson's rule assigns weight of 1/3 to trapezoidal 
  # Riemann sum and 2/3 to midpoint Riemann sum
  return((1/3) * TrapRsum + (2/3)*MidRiemann(P))
}

# Check randomly drawn partition
P <- sort(c(0,1, runif(3)))
SimpsonRsum(P) # 0.5490807
N <- 10^4 # number of trials
X <- numeric(N) # store the mesh
for (i in 1:N) {
  # generate  and sort division/partition points
  P <- sort(c(0, 1, runif(3)))
  # compute the mesh and save it to delta
  X[i] <- SimpsonRsum(P)
}
head(table(X))
# Simposns rule is exact for polynomials of degree less than or equal to 2


# Questions 4 (a)

rm(list = ls()) # clear console

# Verify that the specified gamma relationship is valid
(gamma(3/4) * gamma(5/4)) == (pi*sqrt(2)/4) # True!  Both sides of the equation evaluate to the same value


# Questions 4 (b)

# We are now going to use the built-in integrate() function next
# Note that the integrand of the gamma(r) function is x^(r-1) e(-x)

# Define our function to help compute gamma(3/4)
f1 <- function(x) {
  x ^ (3/4 - 1) * exp(-x)
}

# Define our function to help compute gamma(5/4)
f2 <- function(x) {
  x ^ (5/4 - 1) * exp(-x)
}

# Comparing values to check for equality ....
g34 <- integrate(f1, 0, Inf)$value 
g34 # 1.225417
gamma(3/4) # 1.225417
# values are similar!

# Comparing values to check for equality ...
g54 <- integrate(f2, 0, Inf)$value 
g54 # 0.9064025
gamma(5/4) # 0.9064025
# values are similar!

# verify again
round(g34 * g54 - (sqrt(2) * pi) / 4) # Computes to 0


# Questions 4 (c)

# Using Monte-Carlo from 4D-DensityFunctions.R ...
RiemannMonteCarlo <- function(f, a, b, N){
  ff <- function(x) f(x)*((x >=a)&(x <=b))   # make the function zero outside [a,b]
  aInt <- floor(a); bInt <- ceiling(b)
  Rsum <- 0
  for (i in 1:((bInt-aInt)*2^N)) {
    xEval <- runif(1,min=aInt,max=bInt)
    Rsum <- Rsum + ff(xEval)/(2^N)   # evaluate at a random point
  }
  return(Rsum)
}

# Let b -10^4 and N to be 5
b <- 10^4; N <- 5

# Approximating gamma(3/4)
X <- RiemannMonteCarlo(f1, 0, b, N); X; gamma(3/4)
Y <- RiemannMonteCarlo(f2, 0, b, N); Y; gamma(5/4)
# the results above are once again pretty close

# checking equality
round(X*Y-(sqrt(2)*pi)/4) # computes to 0
