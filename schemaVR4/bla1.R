combinedData <- data.frame(accRecall = c(dataSchemaVR1_recall$accRecall,
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
combinedData_priors <- c(prior(student_t(5, 0, 2.5), class = "Intercept"),
                         prior(student_t(5, 0, 2.5), class = "b")) 


combinedData_model  <- brm(accRecall ~ objLocTargetRating +  
                             I(objLocTargetRating*objLocTargetRating) +
                             (objLocTargetRating + I(objLocTargetRating*objLocTargetRating) | experiment),
                           data = combinedData,
                           family = bernoulli(),
                           prior = combinedData_priors,
                           cores = cores2use,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           iter = 4000, # Because no convergence with default
                           seed = 1298,
                           control = list(adapt_delta = 0.98)) # Increased because of divergent transitions

summary(combinedData_model)
hypothesis(combinedData_model, "IobjLocTargetRatingMUobjLocTargetRating = 0")
plot(hypothesis(combinedData_model, "IobjLocTargetRatingMUobjLocTargetRating = 0"))
plot(marginal_effects(combinedData_model), points = TRUE, rug = TRUE)