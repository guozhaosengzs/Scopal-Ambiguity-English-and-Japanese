## Use libraries
library(tidyverse)
library(lme4)
library(lmerTest)

###### Prepare data

### Import all the data
SLenEng <- read_csv("data\\SentenceLengthEnglish.csv")
SLenJap <- read_csv("data\\SentenceLengthJapanese.csv")

GLenEng <- read_csv("data\\GestureLengthEnglish.csv")
GLenJap <- read_csv("data\\GestureLengthJapanese.csv")


### Clean column names, check each of their type and change them if needed

## 2 of Length data sets
uniformed.colnames <- c("subject", "sentence", "context", "sentence_length", "maxF0", "meanF0", "keyword")
colnames(SLenEng) <- uniformed.colnames
colnames(SLenJap) <- uniformed.colnames

sapply(SLenEng, class)
sapply(SLenJap, class)

col_target <- c("subject", "sentence", "context", "keyword")
SLenEng[col_target] <- lapply(SLenEng[col_target], as.factor)
SLenJap[col_target] <- lapply(SLenJap[col_target], as.factor)

str(SLenEng)
str(SLenJap)



###### Run Models

### Mixed Effect for Length data sets, using "lmer" function as the dependent variable is continuous

## English Sentence Length 
#Random Slope and Random Intercept

# Sentence Length as R.V.

sapply(SLenEng, levels)

# model.EngSent.Len <- lmer(sentence_length ~ context  + (1|keyword)  + (1|subject), data = SLenEng)
# model.EngSent.Len <- lmer(sentence_length ~ context  + (1 + context|keyword/subject), data = SLenEng)

model.EngSent.Len <- lmer(sentence_length ~ context  + (1 + context||keyword)  + (1 + context||subject), 
                          data = SLenEng, 
                          REML = FALSE,
                          control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))


summary(model.EngSent.Len)

# maxF0 as R.V.
model.EngSent.maxF0 <- lmer(maxF0 ~ context  + (1 + context||keyword)  + (1 + context||subject), 
                          data = SLenEng, 
                          REML = FALSE,
                          control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

summary(model.EngSent.maxF0)

# meanF0 as R.V.
model.EngSent.meanF0 <- lmer(meanF0 ~ context  + (1 + context||keyword)  + (1 + context||subject), 
                            data = SLenEng, 
                            REML = FALSE,
                            control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
summary(model.EngSent.meanF0)


## Japanese Sentence Length 

# Sentence_length as R.V.
model.JapSent.Len <- lmer(sentence_length ~ context  + (1 + context||keyword)  + (1 + context||subject), 
                          data = SLenJap, 
                          REML = FALSE,
                          control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))


summary(model.JapSent.Len)

# maxF0 as R.V.
model.JapSent.maxF0 <- lmer(maxF0 ~ context  + (1 + context||keyword)  + (1 + context||subject), 
                            data = SLenJap, 
                            REML = FALSE,
                            control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

summary(model.JapSent.maxF0)

# meanF0 as R.V.
model.JapSent.meanF0 <- lmer(meanF0 ~ context  + (1 + context||keyword)  + (1 + context||subject), 
                             data = SLenJap, 
                             REML = FALSE,
                             control=lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))

summary(model.JapSent.meanF0)


