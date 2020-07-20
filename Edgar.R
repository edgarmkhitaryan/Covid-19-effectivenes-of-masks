rm(list = ls())
library(data.table)
library(tidyverse)
library(openxlsx)
gc()


# Read Data ---------------------------------------------------------------

path <- "/Users/mrmkhitaryan/Desktop/individual_proj/"

df <- read.xlsx(paste0(path, "covid-06-27-2020.xlsx"), detectDates = T)


# Manipulations -----------------------------------------------------------


dim(df)
names(df)
colSums(is.na(df))
str(df)

# Change variable 'age' name to be proper
names(df)[1] <- 'age'
setDT(df)

# Remove unnecessary letters
df[, age2 := gsub("interval_", "", age)]
df[, 1] <- NULL

# Remove unnecessary variables
df[, `:=`(userAgent = NULL,
          guid = NULL,
          ip = NULL)]

# Check Variable types
sapply(df, class) %>% as.data.frame()

df[, zipCode := as.numeric(zipCode)]

df %>% group_by(antibodyTest) %>% summarize(n())
df %>% group_by(exposureLevel) %>% summarize(n())
df %>% group_by(faceCovering) %>% summarize(n())
df %>% group_by(gender) %>% summarize(n())
df %>% group_by(healthIssues) %>% summarize(n())
df %>% group_by(leftHomeTimes) %>% summarize(n())
df <- df %>% 
  mutate(leftHomeTimes = case_when(leftHomeTimes %in% c("didNotLeft", "didNotLeave") ~ '0',
                                   leftHomeTimes == "oneTime" ~ '1',
                                   leftHomeTimes == "twoTimesOrMore" ~ '>2'))
df %>% group_by(leftHomeTimes) %>% summarize(n())

df %>% group_by(mentalHealthImpact) %>% summarize(n())
df <- df %>% 
  mutate(mentalHealthImpact = gsub(pattern = "Impact", replacement = "", mentalHealthImpact))
df %>% group_by(mentalHealthImpact) %>% summarize(n())

df %>% group_by(virusTest) %>% summarize(n())
df %>% group_by(age2) %>% summarize(n())


# Change base level of 'healthIssues' variable
df$healthIssues <- relevel(df$healthIssues, ref = "noIssues")
df <- df %>% 
  mutate(gender2 = case_when(gender %in% c('other', 'notWantToShare', 'notShared') ~ 'other',
                             TRUE ~ gender))

# Change characters to factors
df <- df %>% mutate_if(is.character, as.factor)
str(df)

df <- df %>% 
  mutate(gender2 = relevel(gender2, ref = "other"))


# Graphs ------------------------------------------------------------------

# Make some descriptive plots
ggplot(df) + 
  geom_bar(aes(x = exposureLevel, fill = gender)) +
  theme_bw()

ggplot(df) + 
  geom_bar(aes(x = age2, fill = virusTest)) +
  theme_bw()



# Check correlations ------------------------------------------------------

corMatrix <- cor(df %>% select_if(is.numeric), method = 'pearson')
diag(corMatrix) <- 0

# Correlation should not be higher than 70% 
# to not cause multicollinearity problem
max(abs(corMatrix), na.rm = T) >= 0.7


# Models ------------------------------------------------------------------

# Create Dummy variables for test results
df <- df %>% mutate(testPos = ifelse(virusTest == "positive", 1, 0),
                    testEver = ifelse(virusTest == 'notTested', 0, 1))

rhs <- paste(c('age2', "exposureLevel", "faceCovering", 
               'gender2', "healthIssues"), collapse = "+")
fmla <- paste0('testPos ~ ', rhs)
model1 <- glm(fmla, data=df, family = 'binomial')
summary(model1)


rhs <- paste(c('age2', "exposureLevel", "faceCovering", 
               'gender2', "healthIssues", "noSymptoms"), collapse = "+")
fmla <- paste0('testEver ~ ', rhs)
model2 <- glm(fmla, data=df, family = 'binomial')
summary(model2)

df <- df %>% mutate(age2 = relevel(age2, ref = "26_35"))

rhs <- paste(c('age2', "exposureLevel", "faceCovering", 
               'gender2', "healthIssues"), collapse = "+")
fmla <- paste0('testPos ~ ', rhs)
model11 <- glm(fmla, data=df, family = 'binomial')
summary(model11)

# face covering may not be that efficient, as 
# faceCoveringNever coefficient is not significant.
# Age matters, with people 25-36 being most affected
# Virus transfers between people fast, as 
# "exposureLevelhaveDirectContact" coefficient is significant

rhs <- paste(c('age2', "exposureLevel", "faceCovering", 
               'gender2', "healthIssues", "noSymptoms"), collapse = "+")
fmla <- paste0('testEver ~ ', rhs)
model21 <- glm(fmla, data=df, family = 'binomial')
summary(model21)

# Young people are tested more than elderly
# people with direct contact test more
# pople not covering or occasionally wearing masks are less probable to test,
# it is logical, as they don't care much, hence don't test
# people with some or chronic issues are more probable to be tested
# poepl with symptoms are more probable to test


rhs <- paste(c('age2', "exposureLevel", "faceCovering", 
               'gender2', "healthIssues", "noSymptoms",
               'bodyAche', 'headAche', 'diarrhea',
               'difficultyBreathing', 'disorientation',
               'fatigue', 'irritatedEyes', 'lossOfSmell',
               'persistentCough', 'soreThroat',
               'temperature'), collapse = "+")
fmla <- paste0('testEver ~ ', rhs)
model3 <- glm(fmla, data=df, family = 'binomial')
summary(model3)


rhs <- paste(c('age2', "exposureLevel", "faceCovering", 
               'gender2', "healthIssues", "noSymptoms",
               'bodyAche', 'headAche', 'diarrhea',
               'difficultyBreathing', 'disorientation',
               'fatigue', 'irritatedEyes', 'lossOfSmell',
               'persistentCough', 'soreThroat',
               'temperature'), collapse = "+")
fmla <- paste0('testPos ~ ', rhs)
model4 <- glm(fmla, data=df, family = 'binomial')
summary(model4)


# Model for only people who were tested
rhs <- paste(c('age2', "exposureLevel", "faceCovering", 
               'gender2', "healthIssues", "noSymptoms",
               'bodyAche', 'headAche', 'diarrhea',
               'difficultyBreathing', 'disorientation',
               'fatigue', 'irritatedEyes', 'lossOfSmell',
               'persistentCough', 'soreThroat',
               'temperature'), collapse = "+")
fmla <- paste0('testPos ~ ', rhs)
model5 <- glm(fmla, data = df[virusTest != 'notTested'], family = 'binomial')
summary(model5)


rhs <- paste(c('age2', "exposureLevel", "faceCovering", 
               'gender2', "healthIssues", "noSymptoms"), collapse = "+")
fmla <- paste0('testPos ~ ', rhs)
model6 <- glm(fmla, data = df[virusTest != 'notTested'], family = 'binomial')
summary(model6)


rhs <- paste(c("noSymptoms", 'bodyAche', 'headAche', 'diarrhea',
               'difficultyBreathing', 'disorientation',
               'fatigue', 'irritatedEyes', 'lossOfSmell',
               'persistentCough', 'soreThroat',
               'temperature', "noSymptoms"), collapse = "+")
fmla <- paste0('testPos ~ ', rhs)
model7 <- glm(fmla, data = df[virusTest != 'notTested'], family = 'binomial')
summary(model7)


# Graphs based on models --------------------------------------------------

# by loss of smell
df %>% 
  group_by(virusTest, lossOfSmell) %>% 
  summarise(n()) %>% 
  ggplot(aes(x = virusTest, y = `n()`, fill = lossOfSmell)) +
  geom_bar(position = 'fill', stat = 'identity') +
  theme_bw()

# by temperature
df %>% 
  group_by(virusTest, temperature) %>% 
  summarise(n()) %>% 
  ggplot(aes(x = virusTest, y = `n()`, fill = temperature)) +
  geom_bar(position = 'fill', stat = 'identity') +
  theme_bw()

# by age
df %>% 
  group_by(virusTest, age2) %>% 
  summarise(n()) %>% 
  ggplot(aes(x = virusTest, y = `n()`, fill = age2)) +
  geom_bar(position = 'fill', stat = 'identity') +
  theme_bw()



# With Edgar --------------------------------------------------------------


# Test by symptoms
df %>% 
  group_by(virusTest) %>% 
  summarise(bodyAche = mean(bodyAche), 
            headAche = mean(headAche),
            diarrhea = mean(diarrhea),
            breathing = mean(difficultyBreathing),
            disorientation = mean(disorientation),
            fatigue = mean(fatigue),
            irritatedEyes = mean(irritatedEyes),
            lossOfSmell = mean(lossOfSmell),
            persistentCough = mean(persistentCough),
            soreThroat = mean(soreThroat),
            temperature = mean(temperature)) %>% 
  gather(symptom, prob, bodyAche:temperature) %>% 
  # filter(virusTest != "positive") %>% 
  arrange(desc(prob)) %>%
  ggplot() + 
  geom_bar(aes(x = reorder(symptom, prob), y = prob), stat = "identity", fill = 'steelblue') +
  facet_wrap(~virusTest) +
  coord_flip() +
  theme_bw() +
  labs(x = "Symptom", y = "Probability")



# wearing Mask
df %>% 
  group_by(faceCovering) %>%
  summarise(body = mean(bodyAche), 
            head = mean(headAche),
            diarrhea = mean(diarrhea),
            breathing = mean(difficultyBreathing),
            disorientation = mean(disorientation),
            fatigue = mean(fatigue),
            irritatedEyes = mean(irritatedEyes),
            lossOfSmell = mean(lossOfSmell),
            persistentCough = mean(persistentCough),
            soreThroat = mean(soreThroat),
            temperature = mean(temperature),
            noSymptoms = mean(noSymptoms)) %>%  # added for mask checks
  gather(symptom, prob, body:noSymptoms) %>% 
  filter(!is.na(faceCovering)) %>% 
  arrange(desc(prob)) %>% 
  ggplot() + 
  geom_bar(aes(x = symptom, y = prob), stat = "identity", fill = 'steelblue') +
  coord_flip() +
  facet_wrap(~faceCovering) +
  theme_bw() +
  labs(x = "Symptom", y = "Probability")


# People with direct contact 
df %>% 
  group_by(faceCovering, virusTest, exposureLevel) %>% 
  summarise(N = n()) %>% 
  filter(!is.na(faceCovering) & exposureLevel == "haveDirectContact") %>% 
  ggplot() + 
  geom_bar(aes(x = faceCovering, y = N, fill = virusTest), position = 'fill', stat = "identity") +
  labs(title = "With direct contact")

  
df %>% 
  filter(!is.na(faceCovering)) %>% 
  ggplot() +
  geom_bar(aes(x = virusTest, fill = faceCovering), position = 'fill')
  # geom_bar(aes(x = faceCovering, fill = virusTest), position = 'fill')


df %>% group_by(faceCovering, virusTest, exposureLevel) %>% 
  summarise(N = n()) %>% 
  filter(virusTest == 'positive' & !is.na(faceCovering)) %>% 
  ggplot(aes(x = exposureLevel, y =N, fill = faceCovering)) +
  geom_bar(stat = 'identity')



# Test
df %>% 
  group_by(exposureLevel, virusTest, faceCovering) %>%  
  summarise(N = n()) %>% 
  ungroup() %>% 
  filter(exposureLevel == 'haveDirectContact') %>% 
  mutate(prob = N / sum(N) * 100)



# Test 2
df %>% 
  group_by(faceCovering) %>% 
  summarise(head = mean(headAche),
            fatigue = mean(fatigue),
            lossOfSmell = mean(lossOfSmell)) %>%
  gather(symptom, prob, head:lossOfSmell) %>% 
  arrange(faceCovering, desc(prob)) %>% 
  filter(faceCovering %in% c("Always", "Never")) %>% 
  ggplot() +
  geom_bar(aes(x = symptom, y = prob), stat = 'identity') +
  facet_grid(~faceCovering) +
  theme_bw()


df %>%
  filter(virusTest!="notTested") %>%
  ggplot(aes(x=leftHomeTimes, fill=virusTest)) + geom_bar(position = "fill")

# Karevor
df %>%
  filter(!is.na(faceCovering)) %>%
  ggplot(aes(x=leftHomeTimes, fill=faceCovering)) + geom_bar(position = "fill")

df$leftHomeTimes <- factor(df$leftHomeTimes, levels=c("0", "1", ">2"))

df %>%
  filter(faceCovering=="Never") %>%
  ggplot(aes(x=factor(1), fill=leftHomeTimes)) + geom_bar(width=1) + coord_polar(theta = "y") +
  theme_void()

df %>%
  filter(!is.na(faceCovering)) %>%
  ggplot(aes(x=factor(1), fill=leftHomeTimes)) + geom_bar(width=1) + coord_polar(theta = "y") +
  theme_void()

df %>%
  filter(virusTest=="positive") %>%
  ggplot(aes(x=factor(1), fill=leftHomeTimes)) + geom_bar(width=1) + coord_polar(theta = "y") +
  theme_void()



df %>%
  filter(!is.na(faceCovering)) %>%
  ggplot(aes(x=faceCovering, fill=mentalHealthImpact)) + 
  geom_bar(position = "fill")

df %>%
  ggplot(aes(x=factor(1), fill=mentalHealthImpact)) + 
  geom_bar(width = 1) + 
  coord_polar(theta = "y")

df %>%
  filter(leftHomeTimes=="0" & virusTest=="positive") %>%
  as.data.frame() %>%
  select(householdHeadcount)

df %>%
  filter(!is.na(faceCovering)) %>%
  ggplot(aes(x=age2, fill=faceCovering)) + geom_bar()


df %>%
  filter(!is.na(faceCovering)) %>%
  ggplot(aes(x=healthIssues, fill = faceCovering)) + geom_bar(position = "fill") +
  facet_grid(virusTest ~ ., scales = 'free_y')





