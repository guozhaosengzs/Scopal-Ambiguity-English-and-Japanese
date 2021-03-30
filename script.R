## Use libraries
library(tidyverse)
library(lme4)

###### Prepare data
## Import all the data
GLenEng <- read_csv("C:\\Users\\guozh\\Desktop\\Scopal-Ambiguity-English-and-Japanese\\data\\GestureLengthEnglish.csv")
GLenJap <- read_csv("C:\\Users\\guozh\\Desktop\\Scopal-Ambiguity-English-and-Japanese\\data\\GestureLengthJapanese.csv")
SLenEng <- read_csv("C:\\Users\\guozh\\Desktop\\Scopal-Ambiguity-English-and-Japanese\\data\\SentenceLengthEnglish.csv")
SLenJap <- read_csv("C:\\Users\\guozh\\Desktop\\Scopal-Ambiguity-English-and-Japanese\\data\\SentenceLengthJapanese.csv")


## Check column factor types, and change them accordingly
# Length data
sapply(SLenEng, class)
sapply(SLenJap, class)

cols <- c("subjects", "sentences", "context")
SLenEng[cols] <- lapply(SLenEng[cols], factor)

SLenEng$subjects = as.factor(SLenEng$subjects)






str(SLenEng)
str(SLenJap)


plot(SLenEng$maxF0, SLenEng$meanF0)
