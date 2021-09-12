# /* 
# ----------------------------- Prior & define grid---------------------------
# */
n_points <- 20
# define  initial flat prior
prior <- rep(1/n_points , n_points)

# Define grid
p_grid <- seq(from = 0 , to = 1 , length.out = n_points)

# /* 
# ----------------------------- Sequential: Data 1 ---------------------------
# */
# compute likelihood from data 1 at each value in grid
likelihood1 <- dbinom(6, size = 9 , prob = p_grid)
likelihood1 <- likelihood1/sum(likelihood1)
# compute product of likelihood and prior
unstd.posterior1 <- likelihood1 * prior
# standardize the posterior, so it sums to 1
posterior1 <- unstd.posterior1 / sum(unstd.posterior1)

# /* 
# ----------------------------- Sequential: Data 2 ---------------------------
# */
# compute likelihood from data 2 at each value in grid
likelihood2 <- dbinom(3, size = 9 , prob = p_grid)
likelihood2 <- likelihood2/sum(likelihood2)
# compute product of likelihood and prior
unstd.posterior2 <- likelihood2 * posterior1
# standardize the posterior, so it sums to 1
posterior2 <- unstd.posterior2 / sum(unstd.posterior2)

# /* 
# ----------------------------- Pooled ---------------------------
# */
# compute likelihood from data 2 at each value in grid
likelihood_pooled <- dbinom(9, size = 18 , prob = p_grid)
likelihood_pooled <- likelihood_pooledsum(likelihood_pooled)
# compute product of likelihood and prior
unstd.posterior_pooled <- likelihood_pooled * prior
# standardize the posterior, so it sums to 1
posterior_pooled <- unstd.posterior_pooled / sum(unstd.posterior_pooled)


# /* 
# ----------------------------- Plot ---------------------------
# */
# Create data frame
results1 <- data.frame(Distribution = c(rep('prior', n_points), rep('likelihood', n_points), rep('posterior', n_points)),
                       Probability  = rep(p_grid, 3),
                       Density      = c(prior, likelihood1, posterior1))
results2 <- data.frame(Distribution = c(rep('prior', n_points), rep('likelihood', n_points), rep('posterior', n_points)),
                       Probability  = rep(p_grid, 3),
                       Density      = c(posterior1, likelihood2, posterior2))
results_pooled <- data.frame(Distribution = c(rep('prior', n_points), rep('likelihood', n_points), rep('posterior', n_points)),
                       Probability  = rep(p_grid, 3),
                       Density      = c(prior, likelihood_pooled, posterior_pooled))

# Libraries for plotting
library(ggplot2)
library(cowplot)

pl1 <- ggplot(results1, aes(x = Probability, y = Density, colour = Distribution)) + 
  geom_line() + 
  labs(title = 'Data 1') +
  coord_cartesian(ylim = c(0, 0.3))

legend <- get_legend(pl1)
pl1 <- pl1 + theme(legend.position = 'none')

pl2 <- ggplot(results2, aes(x = Probability, y = Density, colour = Distribution)) + 
  geom_line() + 
  labs(title = 'Data 2') +
  coord_cartesian(ylim = c(0, 0.3)) + 
  theme(legend.position = 'none')
pl3 <- ggplot(results_pooled, aes(x = Probability, y = Density, colour = Distribution)) + 
  geom_line() + 
  labs(title = 'Data pooled') +
  coord_cartesian(ylim = c(0, 0.3))  + 
  theme(legend.position = 'none')

plot_grid(pl1, pl2, legend, pl3, ncol = 2, align = "hv")
