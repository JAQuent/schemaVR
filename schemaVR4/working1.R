library(brms)

fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient), 
            data = epilepsy, family = poisson())

summary(fit1)

plot(fit1, pars = c("Trt", "zBase")) 

launch_shinystan(fit1)

plot(marginal_effects(fit1, effects = "zBase:Trt")) 
# brms:::marginal_effects() will give you the predicted means of the response conditional on 
# all other predictors (whose values you can specify via the conditions argument). There is 
# not much more happening there.

newdata <- data.frame(Trt = c(0, 1), zAge = 0, zBase = 0)
predict(fit1, newdata = newdata, re_formula = NA)
fitted(fit1, newdata = newdata, re_formula = NA)

fit2 <- brm(count ~ zAge + zBase * Trt + (1|patient) + (1|obs), 
            data = epilepsy, family = poisson())

# https://www.rensvandeschoot.com/tutorials/brms-priors/
library(brms) # for the analysis
library(haven) # to load the SPSS .sav file
library(tidyverse) # needed for data manipulation.
library(RColorBrewer) # needed for some extra colours in one of the graphs
library(ggmcmc)
library(ggthemes)
library(lme4)


popular2data<- read_sav(file ="https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/blob/master/chapter%202/popularity/SPSS/popular2.sav?raw=true")

popular2data<-select(popular2data, pupil, class, extrav, sex, texp, popular) # we select just the variables we will use
head(popular2data) # we have a look at the first 6 observations

get_prior(popular~1 + sex + extrav + texp+ extrav:texp+(1+extrav|class), data=popular2data)

prior1 <- c(set_prior("normal(-10,100)", class = "b", coef= "extrav"),
            set_prior("normal(10,100)", class = "b", coef= "extrav:texp"),
            set_prior("normal(-5,100)", class = "b", coef= "sex"),
            set_prior("normal(-5,100)", class = "b", coef= "texp"),
            set_prior("normal(10,100)", class = "Intercept"))


model6<-brm(popular~1 + sex + extrav + texp+ extrav:texp+(1+extrav|class), 
            data=popular2data, 
            warmup = 1000, 
            iter =3000, 
            chains = 2, 
            inits= "random", 
            control = list(adapt_delta = 0.96), 
            prior=prior1,  cores= 2, 
            sample_prior = TRUE) # the cores function tells STAN to make use of 3 CPU cores simultaneously instead of just 1.

prior_summary(model6)
summary(model6)


prior2 <- c(set_prior("normal(.8,.1)", class = "b", coef= "extrav"),
            set_prior("normal(-.025,.1)", class = "b", coef= "extrav:texp"),
            set_prior("normal(1.25,.1)", class = "b", coef= "sex"),
            set_prior("normal(.23,.1)", class = "b", coef= "texp"),
            set_prior("normal(-1.21,.1)", class = "Intercept"))

model7<-brm(popular~1 + sex + extrav + texp+ extrav:texp+(1+extrav|class), data=popular2data, warmup = 1000, iter =3000, chains = 2, inits= "random", control = list(adapt_delta = 0.96, max_treedepth=20), prior=prior2,  cores= 2) 


prior3 <- c(set_prior("normal(-1,.1)", class = "b", coef= "extrav"),
            set_prior("normal(3, 1)", class = "b", coef= "extrav:texp"),
            set_prior("normal(-3,1)", class = "b", coef= "sex"),
            set_prior("normal(-3,1)", class = "b", coef= "texp"),
            set_prior("normal(0,5)", class = "Intercept"))

model8<-brm(popular~1 + sex + extrav + texp+ extrav:texp+(1+extrav|class), data=popular2data, warmup = 1000, iter =3000, chains = 2, inits= "random", control = list(adapt_delta = 0.96, max_treedepth=20), prior=prior3,  cores= 2, sample_prior = TRUE) 