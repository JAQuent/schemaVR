# The aim of this script is to show that sequantial analysis (using psoteriors as priors) is equivalent to 
# combining datasets and running in one go.

# Libraies
library(brms)
library(assortedRFunctions)
cores2use <- 4

# Generating data 1
n1      <- 100
beta1.0 <- 0
beta1.1 <- 0.5
x       <- runif(n1, -1, 1)
y       <- beta1.0 + beta1.1*x + rnorm(n1, 0, 1)
data1 <- data.frame(x, y)

# Generating data 2
n2      <- 90
beta2.0 <- 0.1
beta2.1 <- 0.3
x       <- runif(n2, -1, 1)
y       <- beta2.0 + beta2.1*x + rnorm(n2, 0, 1)
data2 <- data.frame(scax, y)

# Combining data sets
data_combined <- rbind(data1, data2)

# Sequential analysis
# Data 1
initial_priors <- c(prior(normal(0, 1), class = "Intercept"),
                    prior(normal(0, 1), class = "b")) 
m_data1 <- brm(y ~ x, 
               data = data1,
               prior = initial_priors,
               cores = cores2use,
               save_all_pars = TRUE,
               sample_prior = TRUE)

# Data 2
m_data1_fixef <- fixef(m_data1)
priors_data2  <- c(set_prior(priorString_normal(m_data1_fixef[1, 1], m_data1_fixef[1, 2]),
                                       class = "Intercept"),
                             set_prior(priorString_normal(m_data1_fixef[2, 1], m_data1_fixef[2, 2]),
                                       class = "b",
                                       coef = "x"))
m_data2 <- brm(y ~ x, 
               data = data2,
               prior = priors_data2,
               cores = cores2use,
               save_all_pars = TRUE,
               sample_prior = TRUE)

# Combined analysis
m_data_combined <- brm(y ~ x, 
               data = data_combined ,
               prior = initial_priors,
               cores = cores2use,
               save_all_pars = TRUE,
               sample_prior = TRUE)

# Saving results
save.image(datedFileNam('scriptsOutput/ demonstration_sequential_vs_combined', '.RData'))

