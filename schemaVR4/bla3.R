# Parameters
numObj  <- 20
numSub  <- 1000
e_SD    <- 0
gamma00 <- -0.5104374 
beta1   <- 0.1454255
beta2   <- 0.2890990
w1_SD   <- 2
w2_SD   <- 2
u01_SD  <- 2
u02_SD  <- 2




recall_case1_data <- genDat_recall(numObj, numSub, e_SD, gamma00, beta1, beta2, w1_SD, w2_SD, u01_SD, u02_SD)



recall_case1_model_lmer <- glmer(accRecall ~  objLocTargetRating +
                                   I(objLocTargetRating*objLocTargetRating) + 
                                   (1 | subNum) +
                                   (1 | objNum),
                                 family = binomial,
                                 data = recall_case1_data,
                                 control = glmerControl(optimizer = "bobyqa"),
                                 nAGQ = 1)

summary(recall_case1_model_lmer)

# Calculate Cohen's d based on https://www.researchgate.net/post/How_can_I_calculate_an_effect_size_cohens_d_preferably_from_a_linear_random_effects_model_beta
  SD <- sqrt(recall_case1_model_lmer@pp$LamtUt@Dim[2]) * summary(recall_case1_model_lmer)$coefficients[3,2]
  d  <- summary(recall_case1_model_lmer)$coefficients[3,1]/SD
  d