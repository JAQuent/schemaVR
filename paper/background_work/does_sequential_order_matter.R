# Seed
set.seed(20210323)

# Libraries
library(brms)
library(assortedRFunctions) # devtools::install_github("JAQuent/assortedRFunctions", upgrade = 'never')
cores2use <- 4
samples_for_priors <- 20000

# Generating data 1
n1      <- 100
beta1.0 <- 0
beta1.1 <- 0.2
x       <- runif(n1, -1, 1)
y       <- beta1.0 + beta1.1*x + rnorm(n1, 0, 1)
data1 <- data.frame(x, y)

# Generating data 2
n2      <- 50
beta2.0 <- 0.2
beta2.1 <- 0.3
x       <- runif(n2, -1, 1)
y       <- beta2.0 + beta2.1*x + rnorm(n2, 0, 1)
data2   <- data.frame(x, y)

# Combining data sets
data_combined <- rbind(data1, data2)

# Sequential analysis
# Data 1
initial_priors <- c(prior(normal(0, 1), class = "Intercept"),
                    prior(normal(0, 1), class = "b")) 

initial_prior_null <- c(prior(normal(0, 1), class = "Intercept")) 

m_data1 <- brm(y ~ x, 
               data = data1,
               prior = initial_priors,
               chains = 8,
               warmup = 2000,
               iter   = 26000,
               cores = cores2use,
               save_all_pars = TRUE,
               sample_prior = TRUE)

m_data1_null <- brm(y ~ 1, 
                    data = data1,
                    prior = initial_prior_null,
                    chains = 8,
                    warmup = 2000,
                    iter   = 26000,
                    cores = cores2use,
                    save_all_pars = TRUE,
                    sample_prior = TRUE)

# Getting priors for Data 2
# Full model
# Get posterior
postDists <- posterior_samples(m_data1)
samples   <- nrow(postDists)
postDists <- postDists[sample(samples, samples_for_priors),]

# Fit model to get distribution
b_Intercept <- brm(b_Intercept ~ 1,
                   data = postDists,
                   cores = cores2use,
                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Fit model to get distribution
b_x <- brm(b_x~ 1,
           data = postDists,
           cores = cores2use,
           family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Fit model to get distribution
sigma <- brm(sigma ~ 1,
             data = postDists,
             cores = cores2use,
             family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Fit model to get distribution
priors_data2  <- c(set_prior(priorString_student(b_Intercept), 
                             class = "Intercept"),
                   set_prior(priorString_student(b_x), 
                             class = "b", 
                             coef = "x"),
                   set_prior(priorString_student(sigma),
                             class = 'sigma'))

# Null model
# Get posterior
postDists <- posterior_samples(m_data1_null)
samples   <- nrow(postDists)
postDists <- postDists[sample(samples, samples_for_priors),]

# Fit model to get distribution
b_Intercept <- brm(b_Intercept ~ 1,
                   data = postDists,
                   cores = cores2use,
                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Fit model to get distribution
sigma <- brm(sigma ~ 1,
             data = postDists,
             cores = cores2use,
             family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Fit model to get distribution
priors_data2_null  <- c(set_prior(priorString_student(b_Intercept), 
                                  class = "Intercept"),
                        set_prior(priorString_student(sigma),
                                  class = 'sigma'))

# Run full model for data 2
m_data2 <- brm(y ~ x, 
               data = data2,
               prior = priors_data2,
               chains = 8,
               warmup = 2000,
               iter   = 26000,
               cores = cores2use,
               save_all_pars = TRUE,
               sample_prior = TRUE)

# Run null model for data 2
m_data2_null <- brm(y ~ 1, 
                    data = data2,
                    prior = priors_data2_null,
                    chains = 8,
                    warmup = 2000,
                    iter   = 26000,
                    cores = cores2use,
                    save_all_pars = TRUE,
                    sample_prior = TRUE)


# /*
# ----------------------------- Reverse order --------------------------
# */
m_data2_reversed  <- brm(y ~ x, 
                        data = data2,
                        prior = initial_priors,
                        chains = 8,
                        warmup = 2000,
                        iter   = 26000,
                        cores = cores2use,
                        save_all_pars = TRUE,
                        sample_prior = TRUE)

# Getting priors for Data 1
# Full model
# Get posterior
postDists <- posterior_samples(m_data2_reversed)
samples   <- nrow(postDists)
postDists <- postDists[sample(samples, samples_for_priors),]

# Fit model to get distribution
b_Intercept <- brm(b_Intercept ~ 1,
                   data = postDists,
                   cores = cores2use,
                   family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Fit model to get distribution
b_x <- brm(b_x~ 1,
           data = postDists,
           cores = cores2use,
           family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Fit model to get distribution
sigma <- brm(sigma ~ 1,
             data = postDists,
             cores = cores2use,
             family = student(link = "identity", link_sigma = "log", link_nu = "logm1"))

# Fit model to get distribution
priors_data1  <- c(set_prior(priorString_student(b_Intercept), 
                             class = "Intercept"),
                   set_prior(priorString_student(b_x), 
                             class = "b", 
                             coef = "x"),
                   set_prior(priorString_student(sigma),
                             class = 'sigma'))


m_data1_reversed <- brm(y ~ x, 
                        data = data1,
                        prior = priors_data1,
                        chains = 8,
                        warmup = 2000,
                        iter   = 26000,
                        cores = cores2use,
                        save_all_pars = TRUE,
                        sample_prior = TRUE)

# /*
# ----------------------------- Comparison --------------------------
# */
summary(m_data1_reversed)
summary(m_data2)

bf_hypo_1_reversed <- 1/hypothesis(m_data1_reversed, 'x = 0')$hypothesis$Evid.Ratio
bf_hypo_2_reversed <- 1/hypothesis(m_data2_reversed, 'x = 0')$hypothesis$Evid.Ratio

bf_hypo_1_reversed*bf_hypo_2_reversed
bf_hypo_1*bf_hypo_2

# The order at least doesn't seem to mater as 
# > bf_hypo_1_reversed*bf_hypo_2_reversed
# [1] 1.73663
# > bf_hypo_1*bf_hypo_2
# [1] 1.726615

