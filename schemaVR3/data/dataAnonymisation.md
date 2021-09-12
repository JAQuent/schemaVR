Anonymising data of schemaVR3
================

# Aim of this document

# Library

``` r
library(assortedRFunctions)
library(plyr)
library(knitr)
```

The package can be downloaded by using:

``` r
devtools::install_github("JAQuent/assortedRFunctions")
```

Note that this packages is just a random collection of functions I’ve
written and do not come with any documentation but they are necessary to
run the scripts I create.

# Anonymising data

To able to reproduce the anonymisation, it is important to set a seed.

``` r
set.seed(3343)
```

In the next step, I am loading the already preprocessed data that was
created with the updateDataset.R script.

``` r
load("C:/Users/aq01/Desktop/schemaVR/schemaVR3/ignore_rawData/preprocessed/dataSchemaVR3.RData")
```

The data frame contains all data from all trials from all sub-tasks. In
addition to trial, information the data contains information which is
unneccesary for analysis but might enable people to identify other
participants. This information includes the subject number, date of
session, gender and age. For instance, if participants remember their
own subject number, they can guess other people’s idenity and hence
performance. The same is true for the date of the session. All of this
can be especially problematic in cases where participants are mostly
drawn from a local student population.

## Replacing subject number

In the first step, I therefore anonymise the subject number. For this,
I’ve written a function that can be found anonymise() for my own use.

``` r
fileName             <- "C:/Users/aq01/Desktop/schemaVR/schemaVR3/ignore_rawData/preprocessed/schemaVR3_anonKey"
anonSubNum           <- anonymise(dataSchemaVR3$subNum, fileName = fileName)
```

    ##  Filename created: C:/Users/aq01/Desktop/schemaVR/schemaVR3/ignore_rawData/preprocessed/schemaVR3_anonKey_20210516_152401.txt

``` r
dataSchemaVR3$subNum <- anonSubNum
```

The function takes the subject idenitifer in this case running integers
and replaces it with alpha numeric strings.

``` r
levels(anonSubNum)
```

    ##  [1] "N1JVH4" "1L0C5A" "HY54A2" "SDSAO7" "RHSRDV" "34G0FK" "QDE1JE" "V15COT"
    ##  [9] "IUY62M" "XTH754" "28I8OU" "FM3G2N" "VVIX6F" "IRANSN" "EMSJOM" "KRR5S2"
    ## [17] "2WPJ6K" "5MJ39C" "ORU4BJ" "DLZM32" "MA4VYJ" "G9IHS7" "YSRVT1" "EXT58G"
    ## [25] "MVHP3C"

The function also saves a key as .csv file, with which the data can be
de-anonmysed if that is necessary. Obviously, it is very important not
to share this information. Therefore, I save it into folder that is not
tracked by git.

# Period of data collection and demographic

The dates and demographic information are only important as summaries
for the current analysis.

``` r
firstSession  <- as.character(min(dataSchemaVR3$date))
firstSession  <- as.Date(firstSession, '%Y%m%d')

lastSession   <- as.character(max(dataSchemaVR3$date))
lastSession   <- as.Date(lastSession, '%Y%m%d')

testingPeriod <- as.numeric(lastSession - firstSession)

# Delete information from data frame
dataSchemaVR3$date <- NULL
```

All testing was done between NA and NA, hence the testing period of this
experiment was NA days.

In the next step, I load the demographic files and bind them to a data
frame and calculate the necessary values. For this, one participant is
already excluded by altering the file name.

``` r
prefix    <- 'demographic_'
location  <- 'C:/Users/aq01/Desktop/schemaVR/schemaVR3/ignore_rawData/'
allFiles  <- list.files(paste(location, sep = ''))
demoFiles <- allFiles[grep(prefix, allFiles)]
demoPaths <- paste(location, demoFiles, sep = '')

dataSchemaVR3_demo        <- do.call(rbind,lapply(demoPaths, header = FALSE, sep = ' ',read.csv))
names(dataSchemaVR3_demo) <- c('subNum', 'set', 'gender', 'age', 'date', 'startTime', 'endTime')
dataSchemaVR3_demo        <- dataSchemaVR3_demo[order(dataSchemaVR3_demo$subNum), ]

# Deleting information
dataSchemaVR3_demo$subNum <- NULL # To prevent guesses based on age and gender
dataSchemaVR3$age         <- NULL
dataSchemaVR3$gender      <- NULL

# Calculating summary for age
ageMean <- mean(dataSchemaVR3_demo$age)
ageMin  <- min(dataSchemaVR3_demo$age)
ageMax  <- max(dataSchemaVR3_demo$age)
ageSD   <- sd(dataSchemaVR3_demo$age)

# Calculating gender distribution
kable(table(dataSchemaVR3_demo$gender))
```

| Var1   | Freq |
| :----- | ---: |
| female |   11 |
| male   |   13 |

The age range of the participants (N = 24) was between 18 and 32 (Mean =
24.96, SD = 3.71).

# Saving data

``` r
save.image('dataSchemaVR3.RData')
```
