# Script performance a split half two line analysis for AFC: Part 1
# Version 1.0
# Date: 13/11/2019
# Author: Joern Alexander Quent
# /*
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed and cores
set.seed(4544)

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

# AFC data
# Combine data to one data frame
# 3AFC
# Combine data to one data frame
combinedData_AFC <- data.frame(Experiment = c(rep('1', length(dataSchemaVR1_AFC$subNum)),
                                              rep('2', length(dataSchemaVR2_AFC$subNum)),
                                              rep('3', length(dataSchemaVR3_AFC$subNum))),
                               subNum = c(as.character(dataSchemaVR1_AFC$subNum),
                                          as.character(dataSchemaVR2_AFC$subNum),
                                          as.character(dataSchemaVR3_AFC$subNum)),
                               objNum = c(dataSchemaVR1_AFC$objNum,
                                          dataSchemaVR2_AFC$objNum,
                                          dataSchemaVR3_AFC$objNum),
                               objLocTargetRating = c(dataSchemaVR1_AFC$objLocTargetRating,
                                                      dataSchemaVR2_AFC$objLocTargetRating,
                                                      dataSchemaVR3_AFC$objLocTargetRating),
                               accAFC = c(dataSchemaVR1_AFC$accAFC,
                                          dataSchemaVR2_AFC$accAFC,
                                          dataSchemaVR3_AFC$accAFC))
combinedData_AFC$Exp    <- combinedData_AFC$objLocTargetRating 
combinedData_AFC$subNum <- as.character(combinedData_AFC$subNum)


# Scaling based on Gelman et al. (2008) and https://github.com/stan-dev/stan/wiki/Prior-Choice-Recommendations
# Mean = 0 and SD = 0.5
combinedData_AFC$sExp <- (combinedData_AFC$Exp - mean(combinedData_AFC$Exp ))/(sd(combinedData_AFC$Exp)/0.5)

# /*
# ----------------------------- Splitting data in half ---------------------------
# */
# Get participants
subjects    <- unique(combinedData_AFC$subNum) 
# Split the data by odd/even to ensure same number of participants across experiments
group1      <- subjects[seq(2, 61, 2)] # even participants
group2      <- subjects[seq(1, 61, 2)] # odd participants
AFCData1 <- combinedData_AFC[combinedData_AFC$subNum %in% group1, ]
AFCData2 <- combinedData_AFC[combinedData_AFC$subNum %in% group2, ]

# /*
# ----------------------------- Parameters for analysis ---------------------------
# */
# Search parameters
nBr      <- 35
q15      <- quantile(AFCData1$sExp, c(0.15, 0.85))[1]
q85      <- quantile(AFCData1$sExp, c(0.15, 0.85))[2]
br_AFC   <- seq(q15, q85,  length.out =  nBr)

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
                   br = rep(br_AFC, each = nRuns),
                   seed = seeds)

# Priors for all runs
prior_AFC  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
                prior(student_t(3, 0, 10) , class = "b", coef = 'high'),
                prior(student_t(3, 0, 2.5) , class = "b", coef = 'xlow'),
                prior(student_t(3, 0, 2.5)  , class = "b", coef = 'xhigh')) 

# Extract x-values for all runs
x              <- AFCData1$sExp 
AFCData1$xlow  <- ifelse(x <= pars[1,3], x - pars[1,3], 0)
AFCData1$xhigh <- ifelse(x > pars[1,3], x - pars[1,3], 0)     
AFCData1$high  <- ifelse(x > pars[1,3], 1, 0)

# Run 1 for model compilation
model_AFC <- brm(accAFC ~  xlow + xhigh + high +
                               (1 | subNum) +
                               (1 | objNum),
                    data = AFCData1,
                    prior = prior_AFC,
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
                model = model_AFC))
  } else {
    x                 <- AFCData1$sExp 
    AFCData1$xlow  <- ifelse(x <= br, x - br, 0)
    AFCData1$xhigh <- ifelse(x > br, x - br, 0)     
    AFCData1$high  <- ifelse(x > br, 1, 0)
    
    model_AFC <- update(model_AFC,
                           newdata = AFCData1,
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
           model = model_AFC))
  }
  
}

# /*
# ----------------------------- Saving image ---------------------------
# */
save.image('splithalf_AFC_prep.RData')

# /*
# ----------------------------- Create rslurm job --------------------------
# */
# Job parameters
n_nodes       <- 2
cpus_per_node <- nRuns # 1 cpu per run

sjob <- slurm_apply(twoLine_searchLight, pars, jobname = 'splithalf_AFC1',
                    add_objects = c("model_AFC", 'AFCData1', 'prior_AFC', 'iterPerChain'),
                    nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)