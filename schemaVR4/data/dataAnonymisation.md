Anonymising data of schemaVR4
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
created with the schemaVR4\_prepareDataset.R script.

``` r
load("C:/Users/aq01/Desktop/schemaVR/schemaVR4/ignore_rawData/preprocessed/dataSchemaVR4.RData")
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
fileName             <- "C:/Users/aq01/Desktop/schemaVR/schemaVR4/ignore_rawData/preprocessed/schemaVR4_anonKey"
anonSubNum           <- anonymise(dataSchemaVR4$subNum, fileName = fileName)
dataSchemaVR4$subNum <- anonSubNum
```

The function takes the subject idenitifer in this case running integers
and replaces it with alpha numeric strings.

``` r
levels(anonSubNum)
```

    ##  [1] "1B2SZL" "FM3G2N" "YSRVT1" "16QS7V" "G9IHS7" "HY54A2" "RHSRDV" "EXT58G"
    ##  [9] "KRR5S2" "LOJ4BC" "MVHP3C" "N1JVH4" "2WPJ6K" "28I8OU" "ORU4BJ" "ZOPIZW"
    ## [17] "EMSJOM" "MA4VYJ" "QDE1JE" "IRANSN" "V1FBFX" "IUY62M" "G0VKJN" "DLZM32"
    ## [25] "0YVQR6" "XTH754" "1L0C5A" "V15COT" "34G0FK" "E3Y5L8" "SDSAO7" "Z8JE0A"
    ## [33] "5MJ39C" "VVIX6F"

The function also saves a key as .csv file, with which the data can be
de-anonmysed if that is necessary. Obviously, it is very important not
to share this information. Therefore, I save it into folder that is not
tracked by git.

# Period of data collection and demographic

The dates and demographic information are only important as summaries
for the current analysis.

``` r
firstSession  <- as.character(min(dataSchemaVR4$date))
firstSession  <- as.Date(firstSession, '%Y%m%d')

lastSession   <- as.character(max(dataSchemaVR4$date))
lastSession   <- as.Date(lastSession, '%Y%m%d')

testingPeriod <- as.numeric(lastSession - firstSession)

# Delete information from data frame
dataSchemaVR4$date <- NULL
```

All testing was done between NA and NA, hence the testing period of this
experiment was NA days.

In the next step, I load the demographic files and bind them to a data
frame and calculate the necessary values. For this, one participant is
already excluded by altering the file name.

``` r
# Load demo data
load("C:/Users/aq01/Desktop/schemaVR/schemaVR4/ignore_rawData/preprocessed/dataSchemaVR4_demo.RData")

# Rename
dataSchemaVR4_demo        <- demographics
names(dataSchemaVR4_demo) <- c('subNum', 'set', 'gender', 'age', 'date', 'startTime', 'endTime')
dataSchemaVR4_demo        <- dataSchemaVR4_demo[order(dataSchemaVR4_demo$subNum), ]


# Calculating summary for age
ageMean <- mean(dataSchemaVR4_demo$age)
ageMin  <- min(dataSchemaVR4_demo$age)
ageMax  <- max(dataSchemaVR4_demo$age)
ageSD   <- sd(dataSchemaVR4_demo$age)

# Calculating gender distribution
kable(table(dataSchemaVR4_demo$gender))
```

| Var1   | Freq |
| :----- | ---: |
| female |   26 |
| male   |    8 |

The age range of the participants (N = 34) was between 18 and 39 (Mean =
25.76, SD = 5.91).

# Saving data

``` r
# Delete demo that could lead to identification
dataSchemaVR4_demo$subNum <- NULL
dataSchemaVR4_demo$date   <- NULL

# Save new image
save.image('dataSchemaVR4.RData')
```
