#[4] DATA ANALYSIS
#    (for [5] and [6], i.e., 'Sharing' and 'Action', see the Readme page
#    at https://github.com/tsegayeh/InsightsFromFitBitData/edit/main/Readme.md)

# Install packages

# install.packages("dplyr")
# install.packages("vtable")
# install.packages("tidyverse")
# install.packages("stringr")
# install.packages("tidyr")

# library(dplyr)
# library(vtable)
# library(tidyverse)
# library(stringr)
# library(tidyr)

## CLEANED, PROCESSED DATA: 

# 1. Distance 
head(distance,2)

# 2. Steps
head(Steps,2)

# 3. Heart Rate
head(HrtRate,2)

# 4. Calories
head(Calories,2)

# 5. Intensity
head(Intensity,2)

# 6. METs
head(METs,2)

# 7. Sleep State
head(SleepState,2)

# 8. Sleep Amount and Frequency
head(SleepAmtFreq,2) 

# AGGREGATE DATA BY Id, Hour, and Date:

# 1 --- distance ---

DistanceById <- distance %>% 
  group_by(Id) %>%
  summarise(DistanceById = sum(round(TotalDistance))) # all the 31 days
head(DistanceById,2)

write.csv(DistanceById, "DistanceById.csv", row.names = FALSE)
dfDistanceById <- read.csv("DistanceById.csv")
head(dfDistanceById,2)

# *** Hourly distance not available ***

DistanceByDate <- distance %>%
  group_by(ActivityDate) %>%
  summarise(DistanceByDate = sum(round(TotalDistance)))
head(DistanceByDate,2)

write.csv(DistanceByDate, "DistanceByDate.csv", row.names = FALSE)
dfDistanceByDate <- read.csv("DistanceByDate.csv")
head(dfDistanceByDate,2)

# 2 --- steps ---

stepsById <- Steps %>%
  group_by(Id) %>%
  summarise(StepsById = round(sum(StepTotal))) # all the 31 days
head(stepsById,2)

write.csv(stepsById, "stepsById.csv", row.names = FALSE)
dfstepsById <- read.csv("stepsById.csv")
head(dfstepsById,2)

stepsByHour <- Steps %>%
  group_by(Hour) %>%
  summarise(StepsByHour = round(sum(StepTotal)))
head(stepsByHour,2)

write.csv(stepsByHour, "stepsByHour.csv", row.names = FALSE)
dfstepsByHour <- read.csv("stepsByHour.csv")
head(dfstepsByHour,2)

stepsByDate <- Steps %>%
  group_by(ActivityDate) %>%
  summarise(StepsByDate = round(sum(StepTotal)))
head(stepsByDate,2)

write.csv(stepsByDate, "stepsByDate.csv", row.names = FALSE)
dfstepsByDate <- read.csv("stepsByDate.csv")
head(dfstepsByDate,2)

# 3 --- intensity ---

intensById <- Intensity %>%
  group_by(Id) %>%
  summarise(TotInt = sum(TotalIntensity ),
            AvgInt = sum(round(AverageIntensity))) # all the 31 days
head(intensById,2)

write.csv(intensById, "intensById.csv", row.names = FALSE)
dfintensById <- read.csv("intensById.csv")
head(dfintensById,2)

intensByHour <- Intensity %>%
  group_by(Hour) %>%
  summarise(TotInt = sum(TotalIntensity),
            AvgInt = sum(round(AverageIntensity)))
head(intensByHour,2)

write.csv(intensByHour, "intensByHour.csv", row.names = FALSE)
dfintensByHour <- read.csv("intensByHour.csv")
head(dfintensById,2)

intensByDate <- Intensity %>%
  group_by(Date) %>%
  summarise(TotInt = sum(TotalIntensity),
            AvgInt = sum(round(AverageIntensity)))
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
head(CalorieById,2)

write.csv(CaloriesById, "CaloriesById.csv", row.names = FALSE)
dfCaloriesById <- read.csv("CaloriesById.csv")
head(dfCaloriesById,2)

CaloriesByDate <- Calories %>%
  group_by(ActivityDate) %>%
  summarise(Calories = sum(Calories))
head(CaloriesByDate,2)

write.csv(CalorieByDate, "CaloriesByDate.csv", row.names = FALSE)
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

write.csv(METsByDate, "METsByHour.csv", row.names = FALSE)
dfMETsByHour <- read.csv("METsByHour.csv")
head(dfMETsByHour,2)

# 7 --- sleep_state ---

# Rename variable 'value' to 'sleepState'
# SleepState <- rename(SleepState, 'sleepState' = 'value')

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
hist(SleepAmtByDate$TotalMinutesAsleep)
## End 