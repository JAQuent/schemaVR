library(brms)

cores2use <- 3

n <- 40
x <- runif(n)
beta0  <- 0.5
beta1  <- 2

y   <- beta0 + beta1 * x + rnorm(n, 0, 1) 
df  <- data.frame(x = scale(x), y = scale(y))

# get_prior(y ~ x,
#           data = df)

priors <- c(prior(normal(0, 1), class = "Intercept"),
            prior(normal(0, 1), class = "b")) 

m1     <- brm(y ~ x,
              data = df,
              cores = cores2use,
              prior = priors, 
              save_all_pars = TRUE,
              sample_prior = TRUE,
              control = list(adapt_delta = 0.95))

summary(m1)
summary(lm(y ~ x, data = df))
hypothesis(m1, "x = 0")
plot(hypothesis(m1, "x = 0"))
plot(marginal_effects(m1), points = TRUE, rug = TRUE)

