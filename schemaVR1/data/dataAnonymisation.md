Anonymising data of schemaVR1
================

Aim of this document
====================

Library
=======

``` r
library(assortedRFunctions)
library(plyr)
library(knitr)
```

The package can be downloaded by using:

``` r
devtools::install_github("JAQuent/assortedRFunctions")
```

Note that this packages is just a random collection of functions I've written and do not come with any documentation but they are necessary to run the scripts I create.

Anonymising data
================

To able to reproduce the anonymisation, it is important to set a seed.

``` r
set.seed(3728)
```

In the next step, I am loading the already preprocessed data that was created with the updateDataset.R script.

``` r
load("U:/Projects/schemaVR/schemaVR1/ignore_rawData/preprocessed/exp1Data.RData")

# Renaming data frame and deleting old data frame
dataSchemaVR1 <- combData
rm(combData)
```

The data frame contains all data from all trials from all sub-tasks. In addition to trial, information the data contains information which is unneccesary for analysis but might enable people to identify other participants. This information includes the subject number, date of session, gender and age. For instance, if participants remember their own subject number, they can guess other people's idenity and hence performance. The same is true for the date of the session. All of this can be especially problematic in cases where participants are mostly drawn from a local student population.

Replacing subject number
------------------------

In the first step, I therefore anonymise the subject number. For this, I've written a function that can be found anonymise() for my own use.

``` r
fileName             <- "U:/Projects/schemaVR/schemaVR1/ignore_rawData/preprocessed/schemaVR1_anonKey"
anonSubNum           <- anonymise(dataSchemaVR1$subNum, fileName = fileName)
dataSchemaVR1$subNum <- anonSubNum
```

The function takes the subject idenitifer in this case running integers and replaces it with alpha numeric strings.

``` r
levels(anonSubNum)
```

    ##  [1] "VIXTL5" "SNK2ML" "AWQ44N" "BOIPOW" "306HM8" "KHNHVI" "FTSIQ0"
    ##  [8] "RY4X56" "3OMEDJ" "RZ0GTE" "0X3ZM0" "V1M5U1" "ENEPJG" "51TTOJ"
    ## [15] "JMK85L" "VSGC7Q" "74OG80"

The function also saves a key as .csv file, with which the data can be de-anonmysed if that is necessary. Obviously, it is very important not to share this information. Therefore, I save it into folder that is not tracked by git.

Period of data collection and demographic
=========================================

The dates and demographic information are only important as summaries for the current analysis.

``` r
firstSession  <- as.character(min(dataSchemaVR1$date))
firstSession  <- as.Date(firstSession, '%Y%m%d')

lastSession   <- as.character(max(dataSchemaVR1$date))
lastSession   <- as.Date(lastSession, '%Y%m%d')

testingPeriod <- as.numeric(lastSession - firstSession)

# Delete information from data frame
dataSchemaVR1$date <- NULL
```

All testing was done between 2018-03-21 and 2018-05-01, hence the testing period of this experiment was 41 days.

In the next step, I derive the summary measures for gender and age.

``` r
# 1 Subject is excluded from analysis because sligthly different instructions for no-memory trial
dataSchemaVR1_sub <- subset(dataSchemaVR1, subNum != 'FTSIQ0')

# Aggregating to get values for each subject
dataSchemaVR1_demo <- ddply(dataSchemaVR1_sub, c('subNum'), summarise, age = age[1], gender = gender[1])
# Deleting information
dataSchemaVR1_demo$subNum <- NULL # To prevent guesses based on age and gender
dataSchemaVR1$age         <- NULL
dataSchemaVR1$gender      <- NULL

# Calculating summary for age
ageMean <- mean(dataSchemaVR1_demo$age)
ageMin  <- min(dataSchemaVR1_demo$age)
ageMax  <- max(dataSchemaVR1_demo$age)
ageSD   <- sd(dataSchemaVR1_demo$age)

# Calculating gender distribution
kable(table(dataSchemaVR1_demo$gender))
```

| Var1   |  Freq|
|:-------|-----:|
| female |     8|
| male   |     8|

The age range of the participants (N = 16) was between 21 and 33 (Mean = 26.38, SD = 3.52).

Saving data
===========

``` r
save.image('dataSchemaVR1.RData')
```
