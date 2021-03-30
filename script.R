## Use libraries
library(tidyverse)
library(lme4)

###### Prepare data

### Import all the data
GLenEng <- read_csv("C:\\Users\\guozh\\Desktop\\Scopal-Ambiguity-English-and-Japanese\\data\\GestureLengthEnglish.csv")
GLenJap <- read_csv("C:\\Users\\guozh\\Desktop\\Scopal-Ambiguity-English-and-Japanese\\data\\GestureLengthJapanese.csv")
SLenEng <- read_csv("C:\\Users\\guozh\\Desktop\\Scopal-Ambiguity-English-and-Japanese\\data\\SentenceLengthEnglish.csv")
SLenJap <- read_csv("C:\\Users\\guozh\\Desktop\\Scopal-Ambiguity-English-and-Japanese\\data\\SentenceLengthJapanese.csv")


### Clean column names, check each of their type and change them if needed

## 2 of Length data sets
uniformed.colnames <- c("subject", "sentence", "context", "sentence_length", "maxF0", "meanF0")
colnames(SLenEng) <- uniformed.colnames
colnames(SLenJap) <- uniformed.colnames

sapply(SLenEng, class)
sapply(SLenJap, class)

cols  <- c("subject", "sentence", "context")
SLenEng[cols] <- lapply(SLenEng[cols], as.factor)
SLenJap[cols] <- lapply(SLenJap[cols], as.factor)

str(SLenEng)
str(SLenJap)



###### Run Models

### Mixed Effect for Length data sets, using "lmer" function as the dependent variable is continuous


