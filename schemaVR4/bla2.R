# Concatenating data
combinedData_2 <- data.frame(accRecall = c(dataSchemaVR1_recall$accRecall,
                                         dataSchemaVR2_recall$accRecall),
                           objLocTargetRating = c(dataSchemaVR1_recall$objLocTargetRating,
                                                  dataSchemaVR2_recall$objLocTargetRating),
                           subNum = c(dataSchemaVR1_recall$subNum,
                                      dataSchemaVR2_recall$subNum),
                           objNum = c(dataSchemaVR1_recall$objNum,
                                      dataSchemaVR2_recall$objNum),
                           experiment = c(rep('1', length(dataSchemaVR1_recall$accRecall)),
                                          rep('2', length(dataSchemaVR2_recall$accRecall))))

# Choice of prior see here https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# As suggested data is scale free -> weakly informative prior
combinedData_2_priors <- c(prior(normal(0, 1e6), class = "Intercept"),
                           prior(normal(0, 1e6), class = "b")) 


combinedData_2model  <- brm(accRecall ~ objLocTargetRating +  
                             I(objLocTargetRating*objLocTargetRating) +
                             (objLocTargetRating + I(objLocTargetRating*objLocTargetRating) | experiment) +
                             (1 | subNum) +
                             (1 | objNum),
                           data = combinedData_2,
                           family = bernoulli(),
                           prior = combinedData_2_priors,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           iter = 4000, # Because no convergence with default
                           seed = 1298,
                           control = list(adapt_delta = 0.98)) # Increased because of divergent transitions

summary(combinedData_2model)
hypothesis(combinedData_2model, "IobjLocTargetRatingMUobjLocTargetRating = 0")
plot(hypothesis(combinedData_2model, "IobjLocTargetRatingMUobjLocTargetRating = 0"))
plot(marginal_effects(combinedData_2model), points = TRUE, rug = TRUE)
