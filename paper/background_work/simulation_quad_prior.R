library(plyr)
library(ggplot2)

x1 <- runif(1000, min = -272, max = 5000)
x2 <- rnorm(1000, 5, 12)

s_x1 <- scale(x1)/2
s_x2 <- scale(x2)/2



# Relationship between beta an p-value
## Params
nIter     <- 10000
n         <- 40
betas1    <- seq(from = 0, to = 1.5, length.out = 100)
betas2    <- seq(from = 0, to = 1.5, length.out = 100)
intercept <- 0

# Result df
sim_result <- data.frame(b1 = rep(betas1, each = nIter),
                         b2 = rep(betas2, each = nIter),
                         lin_p = NA,
                         qua_p = NA)

# Loop
for(j in 1:length(betas2)){
  for(i in 1:nIter){
    # Random data
    data   <- data.frame(x = rnorm(n))
    data$y <- intercept + betas1[j]*data$x + betas2[j]*data$x^2 + rnorm(n)
    
    # Quadratic
    temp_sum <- summary(lm(y ~ x + I(x^2), data = data))
    sim_result$lin_p[(j-1)*nIter + i] <- temp_sum$coefficients[2,4]
    sim_result$qua_p[(j-1)*nIter + i] <- temp_sum$coefficients[3,4]
  }  
}

sim_results_agg <- ddply(sim_result, c('b1'), summarise, lin_power = mean(lin_p < 0.05), qua_power = mean(qua_p < 0.05))

ggplot() + geom_line(data = sim_results_agg, mapping = aes(x = b1, y= lin_power), colour = 'red')+
  geom_line(data = sim_results_agg, mapping = aes(x = b1, y= qua_power), colour = 'green')