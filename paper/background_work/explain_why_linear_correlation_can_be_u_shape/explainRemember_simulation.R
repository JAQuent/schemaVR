# This simulation tries to elucidate why we get U shape in trial-based and a linear relationship in average analysis. 
# Background:
# We hypothesise that the spread (i.e. variation in expectancy rating) is asymmetrical for congruent and incongruent objects.

# We create simulated data by:
# Give 20 objects true value from uniform between 100 and -100. For each person a random value with the mean of the object with standard deviation (SD) 
# that scales with mean, asymmetrically, capped at 100 and -100 (i.e. higher values are replaced by 100 and - 100. 

# Libraries
library(ggplot2)
library(plyr)

# Set up
set.seed(1241)

# Parameters
numObj  <- 20
numSub  <- 75
sdScale <- -1

# Give 20 obj true values from uniform distribution between -100 and 100.
trueObj_val      <- runif(numObj, -100, 100)

# Have an SD that scales with the true obj values
sd_scaled_by_val <- sdScale*trueObj_val - sdScale*100

plot(trueObj_val, sd_scaled_by_val)

# For each subject draw a random value with the mean of the object with sd that scales with this value
subjVal <- matrix(NA, numSub, numObj)

for(i in 1:numSub){
  for(j in 1:numObj){
    subjVal[i, j] <- rnorm(1, trueObj_val[j], sd_scaled_by_val[j])
  }
}

# Capping at -100 and 100
subjVal[subjVal > 100]  <- 100
subjVal[subjVal < -100] <- -100

# Create DF and also scale x 
df <- data.frame(sub  = rep(1:numSub, numObj),
                 obj  = rep(1:numObj, each = numSub),
                 trueObj_val = rep(trueObj_val, each = numSub),
                 x    = c(subjVal))

# Scale
df$s_trueObj_val <- scale(df$trueObj_val)
df$s_x <- scale(df$x)

# Predicting memory
beta0 <- 0
beta1 <- 1.2
beta2 <- 1.2
z     <- beta0 + beta1*df$s_x + beta2*df$s_x*df$s_x # Regression in log odds
pr    <- 1/(1+exp(-z)) # Convert to probability.
df$y  <- rbinom(numObj*numSub, 1, pr)


# Plot results
ggplot(df, aes(x = s_x, y = y)) + 
  geom_jitter(width = 0, height =  0.1) +
  geom_smooth(colour = 'blue') +
  geom_smooth(aes(x = s_trueObj_val), colour = 'red')

# calculate shift
rightShift <- abs(-100 - df$trueObj_val[df$x == -100])
leftShift  <- abs(100 - df$trueObj_val[df$x == 100])

# T Test for shift
t.test(leftShift, rightShift, var.equal = FALSE)