#[4] DATA ANALYSIS
#    (for [5] and [6], i.e., 'Sharing' and 'Action', see the Readme page
#    at https://github.com/tsegayeh/InsightsFromFitBitData/edit/main/Readme.md)

# Install packages

# install.packages("dplyr")
# install.packages("vtable")
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("tidyr")
# install.packages(ggcorrplot)

library(dplyr)
library(vtable)
library(tidyverse)
library(stringr)
library(tidyr)
library(ggcorrplot)

## CLEANED, PROCESSED DATA: 

# 1. Distance 
distance <- read.csv("distance.csv")
head(distance,2)

# 2. Steps
Steps <- read.csv("Steps.csv")
head(Steps,2)

# 3. Heart Rate
HrtRate <- read.csv("HrtRate.csv")
head(HrtRate,2)

# 4. Calories
Calories <- read.csv("Calories.csv")
head(Calories,2)

# 5. Intensity
Intensity <- read.csv("Intensity.csv")
head(Intensity,2)

# 6. METs
METs <- read.csv("METs.csv")
head(METs,2)

# 7. Sleep State
SleepState <- read.csv("SleepState.csv")
head(SleepState,2)

# 8. Sleep Amount and Frequency
SleepAmtFreq <- read.csv("SleepAmtFreq.csv")
head(SleepAmtFreq,2) 

# AGGREGATE DATA BY Id, Hour, and Date:

# 1 --- distance ---

DistanceById <- distance %>% 
  group_by(Id) %>%
  summarise(Distance = sum(round(TotalDistance))) # all the 31 days
head(DistanceById,2)

write.csv(DistanceById, "DistanceById.csv", row.names = FALSE)
dfDistanceById <- read.csv("DistanceById.csv")
head(dfDistanceById,2)

# *** Hourly distance not available ***

DistanceByDate <- distance %>%
  group_by(ActivityDate) %>%
  summarise(Distance = sum(round(TotalDistance)))
head(DistanceByDate,2)

write.csv(DistanceByDate, "DistanceByDate.csv", row.names = FALSE)
dfDistanceByDate <- read.csv("DistanceByDate.csv")
head(dfDistanceByDate,2)

# 2 --- steps ---

stepsById <- Steps %>%
  group_by(Id) %>%
  summarise(Steps = round(sum(StepTotal))) # all the 31 days
head(stepsById,2)

write.csv(stepsById, "stepsById.csv", row.names = FALSE)
dfstepsById <- read.csv("stepsById.csv")
head(dfstepsById,2)

stepsByHour <- Steps %>%
  group_by(Hour) %>%
  summarise(Steps = round(sum(StepTotal)))
head(stepsByHour,2)

write.csv(stepsByHour, "stepsByHour.csv", row.names = FALSE)
dfstepsByHour <- read.csv("stepsByHour.csv")
head(dfstepsByHour,2)

stepsByDate <- Steps %>%
  group_by(ActivityDate) %>%
  summarise(Steps = round(sum(StepTotal)))
head(stepsByDate,2)

write.csv(stepsByDate, "stepsByDate.csv", row.names = FALSE)
dfstepsByDate <- read.csv("stepsByDate.csv")
head(dfstepsByDate,2)

# 3 --- intensity ---

intensById <- Intensity %>%
  group_by(Id) %>%
  summarise(Intensity = sum(TotalIntensity)) # all the 31 days
head(intensById,2)

write.csv(intensById, "intensById.csv", row.names = FALSE)
dfintensById <- read.csv("intensById.csv")
head(dfintensById,2)

intensByHour <- Intensity %>%
  group_by(Hour) %>%
  summarise(TotInt = sum(TotalIntensity))
head(intensByHour,2)

write.csv(intensByHour, "intensByHour.csv", row.names = FALSE)
dfintensByHour <- read.csv("intensByHour.csv")
head(dfintensById,2)

intensByDate <- Intensity %>%
  group_by(Date) %>%
  summarise(TotInt = sum(TotalIntensity))
head(intensByDate,2)

write.csv(intensByDate, "intensByDate.csv", row.names = FALSE)
dfintensByDate <- read.csv("intensByDate.csv")
head(dfintensByDate,2)

# 4. --- hrtRate ---

hrtRateById <- HrtRate %>%
  group_by(Id) %>%
  summarise(meanHrtRate = round(mean(Value)))
head(hrtRateById,2)

write.csv(hrtRateById, "hrtRateById.csv", row.names = FALSE)
dfhrtRateById <- read.csv("hrtRateById.csv")
head(dfhrtRateById,2)

hrtRateByHour <- HrtRate %>%
  group_by(Hour) %>%
  summarise(meanHrtRate = round(mean(Value)))
head(hrtRateByHour,2)

write.csv(hrtRateByHour, "hrtRateByHour.csv", row.names = FALSE)
dfhrtRateByHour <- read.csv("hrtRateByHour.csv")
head(dfhrtRateByHour,2)

hrtRateByDate <- HrtRate %>%
  group_by(ActivityDate) %>%
  summarise(meanHrtRate = round(mean(Value)))
head(hrtRateByDate,2)

write.csv(hrtRateByDate, "hrtRateByDate.csv", row.names = FALSE)
dfhrtRateByDate <- read.csv("hrtRateByDate.csv")
head(dfhrtRateByDate,2)

# 5. --- Calories ---

CaloriesById <- Calories %>%
  group_by(Id) %>%
  summarise(Calories = sum(Calories)) # All the 31 days
head(CaloriesById,2)

write.csv(CaloriesById, "CaloriesById.csv", row.names = FALSE)
dfCaloriesById <- read.csv("CaloriesById.csv")
head(dfCaloriesById,2)

CaloriesByDate <- Calories %>%
  group_by(ActivityDate) %>%
  summarise(Calories = sum(Calories))
head(CaloriesByDate,2)

write.csv(CaloriesByDate, "CaloriesByDate.csv", row.names = FALSE)
dfCaloriesByDate <- read.csv("CaloriesByDate.csv")
head(dfCaloriesByDate,2)

CaloriesByHour <- Calories %>%
  group_by(Hour) %>%
  summarise(Calories = sum(Calories))
head(CaloriesByHour,2)

write.csv(CaloriesByHour, "CaloriesByHour.csv", row.names = FALSE)
dfCaloriesByHour <- read.csv("CaloriesByHour.csv")
head(dfCaloriesByHour,2)

# 6. --- METs ---

METsById <- METs %>%
  group_by(Id) %>%
  summarise(METs = round(sum(METs)))
head(METsById,2)

write.csv(METsById, "METsById.csv", row.names = FALSE)
dfMETsById <- read.csv("METsById.csv")
head(dfMETsById,2)

METsByDate <- METs %>%
  group_by(ActivityDate) %>%
  summarise(METs = sum(METs))
head(METsByDate,2)

write.csv(METsByDate, "METsByDate.csv", row.names = FALSE)
dfMETsByDate <- read.csv("METsByDate.csv")
head(dfMETsByDate,2)

METsByHour <- METs %>%
  group_by(Hour) %>%
  summarise(METs = sum(METs))
tail(METsByHour,2)

write.csv(METsByHour, "METsByHour.csv", row.names = FALSE)
dfMETsByHour <- read.csv("METsByHour.csv")
head(dfMETsByHour,2)

# 7 --- sleep_state ---

# Rename variable 'value' to 'sleepState'
SleepState <- rename(SleepState, 'sleepState' = 'value')

# Rename levels (state ofsleep):
SleepState$sleepState <- as.factor(SleepState$sleepState)
levels(SleepState$sleepState) <- c("asleep", "restless", "awake")
head(SleepState,2)

table(SleepState$sleepState)
# asleep restless    awake
# 172480    14023     2018

# 8. --- SleepAmtFreq ---

SleepAmtById <- SleepAmtFreq %>%
  group_by(Id) %>%
  summarise(TotalSleepRecords = sum(TotalSleepRecords),
            TotalMinutesAsleep = sum(TotalMinutesAsleep),
            TotalTimeInBed = sum(TotalTimeInBed))
head(SleepAmtById,2)

write.csv(SleepAmtById, "SleepAmtById.csv", row.names = FALSE)
dfSleepAmtById <- read.csv("SleepAmtById.csv")
head(dfSleepAmtById,2)

# sleep amount and frequency by hour is not available.

SleepAmtByDate <- SleepAmtFreq %>%
  group_by(Date) %>%
  summarise(TotalSleepRecords = sum(TotalSleepRecords),
            TotalMinutesAsleep = sum(TotalMinutesAsleep),
            TotalTimeInBed = sum(TotalTimeInBed))
head(SleepAmtByDate,2)

write.csv(SleepAmtByDate, "SleepAmtByDate.csv", row.names = FALSE)
dfSleepAmtByDate <- read.csv("SleepAmtByDate.csv")
head(dfSleepAmtByDate,2)

#   CORRELATIONS ---------------
#a) Five data frames with equal length (n=33)
# First merging the 5 data frames 

fitbit5Vars <- list(DistanceById, stepsById, CaloriesById,intensById, METsById) %>%
  reduce(full_join, by = "Id")
write.csv(fitbit5Vars, "fiveVarsByID.csv", row.names = FALSE)
fiveVarsByID <- read.csv("fiveVarsByID.csv")
head(fiveVarsByID,2)

# See P matrix values
options( scipen = 999 )
p.mat <-  cor_pmat(fiveVarsByID[-1]) 
head(p.mat)

# Compute correlations among the five vars
corr <- round(cor(fiveVarsByID[-1]), 1)
cor_fiveVarsByID <- ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE)
cor_fiveVarsByID

#b) Seven data frames including HrtRate (length n=24) and 
#   SleepAmtFreq (length n=14)
# Merge/join fitbit5Vars with HrtRate and SleepAmtFreq data frames 
df6vars = fiveVarsByID %>% inner_join(dfhrtRateById,by="Id")
df7vars = df6vars %>% inner_join(dfSleepAmtById,by="Id")
df7vars

#Only use  TotalMinutesAsleep from dfSleepAmtById
df7vars <- df7vars[-c(8,10)]
df7vars

write.csv(df7vars, "sevenVarsById.csv", row.names = FALSE)
sevenVarsById <- read.csv("sevenVarsById.csv")
head(sevenVarsById,2)

# Compute correlations among the seven vars
corr <- round(cor(sevenVarsById[-1]), 1)
cor_sevenVarsById <- ggcorrplot(corr, hc.order = TRUE, type = "lower",
                              lab = TRUE)
cor_sevenVarsById

# END