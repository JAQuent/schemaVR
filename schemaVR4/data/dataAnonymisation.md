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
load("D:/Alex/Laptop/Desktop/schemaVR/schemaVR4/ignore_rawData/preprocessed/dataSchemaVR4.RData")
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
fileName             <- "D:/Alex/Laptop/Desktop/schemaVR/schemaVR4/ignore_rawData/preprocessed/schemaVR4_anonKey"
anonSubNum           <- anonymise(dataSchemaVR4$subNum, fileName = fileName)
```

    ##  Filename created: D:/Alex/Laptop/Desktop/schemaVR/schemaVR4/ignore_rawData/preprocessed/schemaVR4_anonKey_20210628_111642.txt

``` r
dataSchemaVR4$subNum <- anonSubNum
```

The function takes the subject identifier in this case running integers
and replaces it with alpha numeric strings.

``` r
levels(anonSubNum)
```

    ##  [1] "EMSJOM" "MA4VYJ" "THIPRM" "0YVQR6" "FM3G2N" "4RVW7W" "34G0FK" "LOJ4BC"
    ##  [9] "HF4VF4" "KRR5S2" "0LON2N" "AO2OUX" "LDH64F" "CHUE9C" "V1FBFX" "28I8OU"
    ## [17] "2WPJ6K" "MVHP3C" "5MJ39C" "H0614B" "YX3UOC" "P5BDRA" "V15COT" "RZRY01"
    ## [25] "3M9BPC" "ZOPIZW" "CZDF7Z" "RHSRDV" "FZP5RJ" "Z8JE0A" "IJEISG" "MK58X8"
    ## [33] "N1JVH4" "HY54A2" "3B6M7E" "EXT58G" "QDE1JE" "PRN9GQ" "20LKXT" "SDSAO7"
    ## [41] "IUY62M" "GABN4L" "16QS7V" "762HC1" "MXPB1W" "BIJLM4" "U5Y454" "2BE42E"
    ## [49] "4MGPEO" "MZYA7N" "DLZM32" "IRANSN" "VVIX6F" "6JZSA4" "E3Y5L8" "Q4MFFK"
    ## [57] "6EJJKQ" "G9IHS7" "B8VVUD" "ELAY56" "O2BJ3O" "1L0C5A" "ZZ2E77" "YSRVT1"
    ## [65] "2CVXEF" "QW0EG8" "G0VKJN" "5LOQL7" "XTH754" "1B2SZL" "ORU4BJ" "8W4FDN"

The function also saves a key as .csv file, with which the data can be
de-anonmysed if that is necessary. Obviously, it is very important not
to share this information. Therefore, I save it into folder that is not
tracked by git.

# Demographics

In the next step, I load the demographic files and bind them to a data
frame and calculate the necessary values. For this, one participant is
already excluded by altering the file name.

``` r
# Load demo data
load("D:/Alex/Laptop/Desktop/schemaVR/schemaVR4/ignore_rawData/preprocessed/dataSchemaVR4_demo.RData")

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

| Var1       | Freq |
|:-----------|-----:|
| female     |   50 |
| male       |   21 |
| non-binary |    1 |

The age range of the participants (N = 72) was between 18 and 57 (Mean =
26.12, SD = 6.53).

# Period of data collection and demographic

The dates and demographic information are only important as summaries
for the current analysis.

``` r
# Convert numbers into dates
data_string <- as.character(dataSchemaVR4_demo$date)
dates       <- as.Date(data_string, format = '%Y%m%d')

firstSession  <- min(dates, na.rm = TRUE)


lastSession   <- max(dates, na.rm = TRUE)


testingPeriod <- as.numeric(lastSession - firstSession)
```

All testing was done between 2020-02-03 and 2021-06-01, hence the
testing period of this experiment was 484 days.

# Saving data

``` r
# Delete demo that could lead to identification
dataSchemaVR4_demo$subNum <- NULL
dataSchemaVR4_demo$date   <- NULL

# Save new image
save.image('dataSchemaVR4.RData')
```
