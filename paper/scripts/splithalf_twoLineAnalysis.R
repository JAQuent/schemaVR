# Script performance a split half two line analysis for recall: Part 1
# Version 1.0
# Date: 13/11/2019
# Author: Joern Alexander Quent
# /*
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed and cores
set.seed(1544)

# Libraries
library(brms)
library(rslurm)

# /*
# ----------------------------- Loading data ---------------------------
# */
# Loading data
load("data/dataSchemaVR1_cleaned.RData")
load("data/dataSchemaVR2_cleaned.RData")
load("data/dataSchemaVR3_cleaned.RData")

# Recall data
# Combine data to one data frame
combinedData_recall <- data.frame(Experiment = c(rep('1', length(dataSchemaVR1_recall$subNum)),
                                                 rep('2', length(dataSchemaVR2_recall$subNum)),
                                                 rep('3', length(dataSchemaVR3_recall$subNum))),
                                  subNum = c(as.character(dataSchemaVR1_recall$subNum),
                                             as.character(dataSchemaVR2_recall$subNum),
                                             as.character(dataSchemaVR3_recall$subNum)),
                                  objNum = c(dataSchemaVR1_recall$objNum,
                                             dataSchemaVR2_recall$objNum,
                                             dataSchemaVR3_recall$objNum),
                                  objLocTargetRating = c(dataSchemaVR1_recall$objLocTargetRating,
                                                         dataSchemaVR2_recall$objLocTargetRating,
                                                         dataSchemaVR3_recall$objLocTargetRating),
                                  accRecall = c(dataSchemaVR1_recall$accRecall,
                                                dataSchemaVR2_recall$accRecall,
                                                dataSchemaVR3_recall$accRecall),
                                  euclideanDist = c(dataSchemaVR1_recall$euclideanDist,
                                                    dataSchemaVR2_recall$euclideanDist,
                                                    dataSchemaVR3_recall$euclideanDist))
combinedData_recall$Exp  <- combinedData_recall$objLocTargetRating 

# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_recall$sExp <- (combinedData_recall$Exp - mean(combinedData_recall$Exp ))/(sd(combinedData_recall$Exp)/0.5)

# /*
# ----------------------------- Splitting data in half ---------------------------
# */
# Get participants
subjects    <- unique(combinedData_recall$subNum)
s_subjects  <- sample(subjects)
group1      <- s_subjects[1:30]
group2      <- s_subjects[31:61]
recallData1 <- combinedData_recall[combinedData_recall$subNum %in% group1, ]
recallData2 <- combinedData_recall[combinedData_recall$subNum %in% group2, ]

# /*
# ----------------------------- Parameters for analysis ---------------------------
# */
# Search parameters
nBr       <- 35
q15      <- quantile(recallData1$sExp, c(0.15, 0.85))[1]
q85      <- quantile(recallData1$sExp, c(0.15, 0.85))[2]
br_recall <- seq(q15, q85,  length.out =  nBr)

# BRMS parameters
chains       <- 1
nRuns        <- 8
iterPerChain <- 4000
seeds        <- sample(99999, nRuns*nBr) # nRuns per each breaking point (nBR)

# /*
# ----------------------------- Functions for analysis ---------------------------
# */
# Parameter data frame 
pars <- data.frame(i = 1:(nRuns*nBr),
                   run = rep(1:nRuns, nBr),
                   br = rep(br_recall, each = nRuns),
                   seed = seeds)

# Priors for all runs
prior_recall  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
                   prior(student_t(3, 0, 10) , class = "b", coef = 'high'),
                   prior(student_t(3, 0, 2.5) , class = "b", coef = 'xlow'),
                   prior(student_t(3, 0, 2.5)  , class = "b", coef = 'xhigh')) 

# Extract x-values for all runs
x                 <- recallData1$sExp 
recallData1$xlow  <- ifelse(x <= pars[1,3], x - pars[1,3], 0)
recallData1$xhigh <- ifelse(x > pars[1,3], x - pars[1,3], 0)     
recallData1$high  <- ifelse(x > pars[1,3], 1, 0)

# Run 1 for model compilation
model_recall <- brm(accRecall ~  xlow + xhigh + high +
                               (1 | subNum) +
                               (1 | objNum),
                    data = recallData1,
                    prior = prior_recall,
                    family = bernoulli(),
                    cores = 1,
                    chains = 1,
                    iter = iterPerChain,
                    seed = pars[1,4],
                    save_all_pars = TRUE,
                    sample_prior = TRUE)


# Analysis function
twoLine_searchLight <- function(i, run, br, seed){
  if(i == 1){
    return(list(i = i,
                run = run,
                br = br,
                seed = seed,
                model = model_recall))
  } else {
    x                 <- recallData1$sExp 
    recallData1$xlow  <- ifelse(x <= br, x - br, 0)
    recallData1$xhigh <- ifelse(x > br, x - br, 0)     
    recallData1$high  <- ifelse(x > br, 1, 0)
    
    model_recall <- update(model_recall,
                           newdata = recallData1,
                           recompile = FALSE,
                           cores = 1,
                           chains = 1,
                           iter = iterPerChain,
                           save_all_pars = TRUE,
                           sample_prior = TRUE,
                           seed = seed)
    return(list(i = i,
           run = run,
           br = br,
           seed = seed,
           model = model_recall))
  }
  
}

# /*
# ----------------------------- Saving image ---------------------------
# */
save.image('splithalf_recall_prep.RData')

# /*
# ----------------------------- Create rslurm job --------------------------
# */
# Job parameters
n_nodes       <- 2
cpus_per_node <- nRuns # 1 cpu per run

sjob <- slurm_apply(twoLine_searchLight, pars, jobname = 'splithalf_recall',
                    add_objects = c("model_recall", 'recallData1', 'prior_recall', 'iterPerChain'),
                    nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)

