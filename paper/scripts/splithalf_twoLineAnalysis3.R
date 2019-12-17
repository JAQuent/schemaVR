# Script performance a split half two line analysis for recall: Part 3
# Version 1.0
# Date: 14/11/2019
# Author: Joern Alexander Quent
# /*
# ----------------------------- Libraries, settings and functions ---------------------------
# */
# Setting seed and cores
set.seed(2544)

# Load auxilliary data to get second half of data
load("splithalf_recall_prep.RData")

# Libraries
library(brms)
library(rslurm)

# Extrat br
br <- br_recall[8] # based on results in Part 2

# /*
# ----------------------------- Parameters for analysis ---------------------------
# */
# BRMS parameters
chains       <- 1
nRuns        <- 8
iterPerChain <- 4000
seeds        <- sample(99999, nRuns) 

# /*
# ----------------------------- Functions for analysis ---------------------------
# */
# Parameter data frame 
pars <- data.frame(i = 1:nRuns,
                   run = 1:nRuns,
                   br = br,
                   seed = seeds)

# Priors for all runs
prior_recall  <- c(prior(student_t(3, 0, 10) , class = "Intercept"),
                   prior(student_t(3, 0, 10) , class = "b", coef = 'high'),
                   prior(student_t(3, 0, 2.5) , class = "b", coef = 'xlow'),
                   prior(student_t(3, 0, 2.5)  , class = "b", coef = 'xhigh')) 

# Extract x-values for all runs
x                 <- recallData2$sExp 
recallData2$xlow  <- ifelse(x <= pars[1,3], x - pars[1,3], 0)
recallData2$xhigh <- ifelse(x > pars[1,3], x - pars[1,3], 0)     
recallData2$high  <- ifelse(x > pars[1,3], 1, 0)

# Run 1 for model compilation
model_recall <- brm(accRecall ~  xlow + xhigh + high +
                      (1 | subNum) +
                      (1 | objNum),
                    data = recallData2,
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
    model_recall <- update(model_recall,
                           newdata = recallData2,
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
# ----------------------------- Create rslurm job --------------------------
# */
# Job parameters
n_nodes       <- 1
cpus_per_node <- nRuns # 1 cpu per run

sjob <- slurm_apply(twoLine_searchLight, pars, jobname = 'splithalf_recall2',
                    add_objects = c("model_recall", 'recallData2', 'prior_recall', 'iterPerChain'),
                    nodes = n_nodes, cpus_per_node = cpus_per_node, submit = FALSE)
