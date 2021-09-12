Anonymising data of schemaVR2
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
set.seed(1348)
```

In the next step, I am loading the already preprocessed data that was
created with the updateDataset.R script.

``` r
load("D:/Alex/Laptop/Desktop/schemaVR/schemaVR2/ignore_rawData/preprocessed/exp2Data.RData")

# Renaming data frame and deleting old data frame
dataSchemaVR2 <- combData
rm(combData)
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
fileName             <- "D:/Alex/Laptop/Desktop/schemaVR/schemaVR2/ignore_rawData/preprocessed/schemaVR2_anonKey"
anonSubNum           <- anonymise(dataSchemaVR2$subNum, fileName = fileName)
```

    ##  Filename created: D:/Alex/Laptop/Desktop/schemaVR/schemaVR2/ignore_rawData/preprocessed/schemaVR2_anonKey_20210623_155307.txt

``` r
dataSchemaVR2$subNum <- anonSubNum
```

The function takes the subject idenitifer in this case running integers
and replaces it with alpha numeric strings.

``` r
levels(anonSubNum)
```

    ##  [1] "1FKR4K" "EI10V0" "LXM4GF" "UGJMCX" "X7B9BG" "TVD74X" "SZQUCC" "3XA0LV"
    ##  [9] "TJR7BK" "GX8SQE" "ZS283F" "4XITTR" "MHHN51" "B5FCPK" "D1YZHO" "9VNH0W"
    ## [17] "XMF0LL" "DSV33X" "DCJM8V" "EC060U" "V4IN2I" "KMZ8ER" "YKK4WQ" "L3MJXL"
    ## [25] "PO74PI" "HX66P7" "OJPXSM" "6W1976" "62SFTH" "59YUE8"

The function also saves a key as .csv file, with which the data can be
de-anonmysed if that is necessary. Obviously, it is very important not
to share this information. Therefore, I save it into folder that is not
tracked by git.

# Period of data collection and demographic

The dates and demographic information are only important as summaries
for the current analysis.

``` r
firstSession  <- as.character(min(dataSchemaVR2$date))
firstSession  <- as.Date(firstSession, '%Y%m%d')

lastSession   <- as.character(max(dataSchemaVR2$date))
lastSession   <- as.Date(lastSession, '%Y%m%d')

testingPeriod <- as.numeric(lastSession - firstSession)

# Delete information from data frame
dataSchemaVR2$date <- NULL
```

All testing was done between 2018-05-03 and 2021-06-02, hence the
testing period of this experiment was 1126 days.

In the next step, I derive the summary measures for gender and age.

``` r
# 1 Subject is excluded from analysis because slightly different instructions for no-memory trial
dataSchemaVR2_sub <- subset(dataSchemaVR2, subNum != 'EC060U' & 
                                           subNum != 'DSV33X' &
                                           subNum != 'L3MJXL' &
                                           subNum != 'UGJMCX' &
                                           subNum != '6W1976')

# Aggregating to get values for each subject
dataSchemaVR2_demo <- ddply(dataSchemaVR2_sub, c('subNum'), summarise, age = age[1], gender = gender[1])
# Deleting information
dataSchemaVR2_demo$subNum <- NULL # To prevent guesses based on age and gender
dataSchemaVR2$age         <- NULL
dataSchemaVR2$gender      <- NULL

# Calculating summary for age
ageMean <- mean(dataSchemaVR2_demo$age)
ageMin  <- min(dataSchemaVR2_demo$age)
ageMax  <- max(dataSchemaVR2_demo$age)
ageSD   <- sd(dataSchemaVR2_demo$age)

# Calculating gender distribution
kable(table(dataSchemaVR2_demo$gender))
```

| Var1       | Freq |
|:-----------|-----:|
| female     |   18 |
| male       |    6 |
| non-binary |    1 |

The age range of the participants (N = 25) was between 21 and 32 (Mean =
24.52, SD = 2.83).

# Saving data

``` r
save.image('dataSchemaVR2.RData')
```
