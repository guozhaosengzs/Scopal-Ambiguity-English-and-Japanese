## Use libraries ----
library(tidyverse)
library(lme4)
library(lmerTest)


## Import all the data ----
GLenEng <- read_csv("data/GestureLengthEnglish.csv")
GLenJap <- read_csv("data/GestureLengthJapanese.csv")

SLenEng <- read_csv("data/SentenceLengthEnglish.csv")
SLenJap <- read_csv("data/SentenceLengthJapanese.csv")

## Clean 2 of the Gesture Length data sets ----
# Remove used columns
GLenEng <- GLenEng[1:(length(GLenEng)-2)]
GLenJap <- GLenJap[1:(length(GLenJap)-2)]

# Correct the types of the variable
factor_cols <- c("subject", "keyword", "context")
GLenEng[factor_cols] <- lapply(GLenEng[factor_cols], as.factor)
GLenJap[factor_cols] <- lapply(GLenJap[factor_cols], as.factor)

numeric_cols <- c("duration", "meanF0", "maxF0")
GLenEng[numeric_cols] <- lapply(GLenEng[numeric_cols], as.numeric)
GLenJap[numeric_cols] <- lapply(GLenJap[numeric_cols], as.numeric)

str(GLenEng)
str(GLenJap)

## Clean 2 of the Sentence Length data sets ----
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


## English Gesture Length ----

# Gesture Duration as R.V.
model.EngGest.Dur <- lmer(duration ~ context + 
                              (1|keyword) + 
                              (1|subject), 
                          data = GLenEng)
summary(model.EngGest.Dur)

# Mean Fundamental Frequency as R.V.
model.EngGest.meanF0 <- lmer(meanF0 ~ context + 
                              (1|keyword) + 
                              (1|subject), 
                          data = GLenEng)
summary(model.EngGest.meanF0)

# Max Fundamental Frequency as R.V.
model.EngGest.maxF0 <- lmer(maxF0 ~ context + 
                                 (1|keyword) + 
                                 (1|subject), 
                             data = GLenEng)
summary(model.EngGest.maxF0)

## Japanese Gesture Length ----

# Gesture Duration as R.V.
model.JapGest.Dur <- lmer(duration ~ context + 
                              (1|keyword) + 
                              (1|subject), 
                          data = GLenJap)
summary(model.JapGest.Dur)

# Mean Fundamental Frequency as R.V.
model.JapGest.meanF0 <- lmer(meanF0 ~ context + 
                                 (1|keyword) + 
                                 (1|subject), 
                             data = GLenJap)
summary(model.JapGest.meanF0)

# Max Fundamental Frequency as R.V.
model.JapGest.maxF0 <- lmer(maxF0 ~ context + 
                                (1|keyword) + 
                                (1|subject), 
                            data = GLenJap)
summary(model.JapGest.maxF0)


## English Sentence Length ----
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


## Japanese Sentence Length ----

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




## Last Word Length as R.V. in English ----
LastLenEng <- read_csv("data/LastLenEng.csv")

LastLenEng[c("subject", "context", "keyword")] <- lapply(LastLenEng[c("subject", "context", "keyword")], as.factor)

model.LastLenEng <- lmer(last_word_len ~ context + 
                              (1|keyword) + 
                              (1|subject), 
                          data = LastLenEng)

summary(model.LastLenEng)


## Last Word Length as R.V. in Japanese ----
LastLenJap <- read_csv("data/LastLenJap.csv")

LastLenJap[c("subject", "context", "keyword")] <- lapply(LastLenJap[c("subject", "context", "keyword")], as.factor)

model.LastLenJap <- lmer(last_word_len ~ context + 
                             (1|keyword) + 
                             (1|subject), 
                         data = LastLenJap)

summary(model.LastLenJap)
