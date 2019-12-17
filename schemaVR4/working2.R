load("U:/Projects/schemaVR/schemaVR3/analysis/dataForBayesianReanalysis.RData") # Cluster

## 
schemaVR1_recall_effM <- brm(accRecall ~  objLocTargetRating +  
                               I(objLocTargetRating*objLocTargetRating) +
                               (1 | subNum) +
                               (1 | objNum),
                             data = dataSchemaVR1_recall,
                             cores = 3,
                             family = bernoulli(),
                             save_all_pars = TRUE)

schemaVR1_recall_effM_postDen  <- posterior_samples(schemaVR1_recall_effM)





get_prior(accRecall ~  objLocTargetRating +  
            I(objLocTargetRating*objLocTargetRating) +
            (1 | subNum) +
            (1 | objNum),
          data = dataSchemaVR1_recall)
prior1 <- set_prior(paste('normal(',
                          mean(schemaVR1_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating),
                          ',', 
                          sd(schemaVR1_recall_effM_postDen$b_IobjLocTargetRatingMUobjLocTargetRating),
                          ')',
                          sep = ''),
                    class = "b",
                    coef = "IobjLocTargetRatingMUobjLocTargetRating")
schemaVR2_recall_priors_eff <- c(prior1)

# Running model
schemaVR2_recall_effM <- brm(accRecall ~ objLocTargetRating +  
                               I(objLocTargetRating*objLocTargetRating) +
                               (1 | subNum) +
                               (1 | objNum),
                             data = dataSchemaVR2_recall,
                             cores = 10,
                             family = bernoulli(),
                             save_all_pars = TRUE,
                             prior = schemaVR2_recall_priors_eff)
