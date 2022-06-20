TimeBegining <- Sys.time()
TimeBegining
# -----------------------------------------------------------------
#  Title: Insights From FitBit's FITNESS DATA To Inform Bellabeat's 
#        Marketing Strategy: A Capstone Project
# -----------------------------------------------------------------

#[1] ASKING (Business Task): Gain insights from FitBit data
#           to inform Bellabeat's marketing strategy 

#[2] PREPARATION
#           A 18-table dataset was downloded from Kaggle website:
#             https://www.kaggle.com/datasets/arashnic/fitbit. 

#[3] PROCESSING

#A. Install packages

# install.packages("dplyr")
# install.packages("vtable")
# install.packages("corrplot")
# install.packages("ggplot2")
# install.packages("anytime")
# install.packages("egg")

library(dplyr)
library(vtable)
library(corrplot)
library(ggcorrplot)
library(ggplot2)
library(tidyverse)
library(stringr)
library(tidyr)
library(anytime)
library(egg)
library(viridis)
library(hrbrthemes)
library(sqldf)

#B. Read data into R to create respective dataframes - 'a' through 'p'. 

a <- read.csv("dailyActivity_merged.csv") 
b <- read.csv("dailyCalories_merged.csv") 
c <- read.csv("dailyIntensities_merged.csv") 
d <- read.csv("dailySteps_merged.csv") 
e <- read.csv("heartrate_seconds_merged.csv")
f <- read.csv("hourlyCalories_merged.csv")
g <- read.csv("hourlyIntensities_merged.csv")
h <- read.csv("hourlySteps_merged.csv")
i <- read.csv("minuteCaloriesNarrow_merged.csv")
j <- read.csv("minuteCaloriesWide_merged.csv")
k <- read.csv("minuteIntensitiesNarrow_merged.csv")
l <- read.csv("minuteIntensitiesWide_merged.csv")
m <- read.csv("minuteMETsNarrow_merged.csv")
n <- read.csv("minuteSleep_merged.csv")
o <- read.csv("minuteStepsNarrow_merged.csv")
p <- read.csv("minuteStepsWide_merged.csv")
q <- read.csv("sleepday_merged.csv")
r <- read.csv("weightLogInfo_merged.csv")

#C. Explore data

# Checking for NAs
colSums(is.na(a)) # No NAs in any of data frames 'a' through 'r'.

# Vtables (df 'a' through 'q'):

# df 'a' ------------------
# vtable(a) # Data frame (or df) 'a': Daily Activity ( Steps, Distance(7), Minutes(4), Calories)
head(a,2)

class(a$ActivityDate) # character
# * consider 'a' upon Changing 'ActivityDate' data type 
#   from 'character' to 'date' *** .................................
a$ActivityDate <- as.Date(a$ActivityDate, format = "%m/%d/%Y")

# df 'b' ------------------
# vtable(b) # df 'b': Daily Calories; 
head(b,2)

# Is it a duplicate of 'Calories' column in 'a'? Yes:
sum(a$Calories - b$Calories) # Customer level comparison

# *** therefore, drop df 'b'; use df 'a' for analysis *** .....

# df ''c ------------------
# vtable(c) # df 'c': Daily Intensities - Distances(4), their respective minutes(4)
head(c,2)

#   Are these 8 columns duplicates of same columns in df 'a'? Yes:
sum(a$SedentaryMinutes - c$SedentaryMinutes) # Customer level comparison
sum(a$LightlyActiveMinutes - c$LightlyActiveMinutes)
sum(a$FairlyActiveMinutes - c$FairlyActiveMinutes)
sum(a$VeryActiveMinutes - c$VeryActiveMinutes)
sum(a$SedentaryActiveDistance - c$SedentaryActiveDistance) 
sum(a$LightActiveDistance - c$LightActiveDistance)
sum(a$ModeratelyActiveDistance - c$ModeratelyActiveDistance)
sum(a$VeryActiveDistance - c$VeryActiveDistance)

# *** therefore, drop df 'c'; use df 'a' from analysis *** .....

# df 'd' ------------------
# vtable(d) # df 'd': Daily Steps; 
head(d,2)

# Is the 'StepTotal' column in df 'd' a duplicate of the 
# 'TotalSteps' column in df 'a'? Yes:
sum(a$TotalSteps - d$StepTotal)

# *** therefore, drop df 'd', use df 'a' for analysis *** ......

# df 'e' ------------------
# vtable(e) # df 'e': Heart rate at Seconds level (2,483,658 observations)
head(e,2)

# *** use df 'e' in data analysis, but
#     for aggregation purposes, split the 'Time' column into
#     'ActivityDate', 'HH:MM:SS', 'AM_PM':  

timeDf_e1_begin <- Sys.time()
timeDf_e1_begin

# Step 1. split 'Time' into 'ActivityDate', 'HH:MM:SS', 'AM_PM':
eNew <- separate(e, col=Time, into=c('ActivityDate', 'HH:MM:SS', 'AM_PM'), sep=' ')
head(eNew,2)

timeDf_e1_end <- Sys.time()
timeDf_e1_end

timeDf_e1_diff <- timeDf_e1_begin - timeDf_e1_end
timeDf_e1_diff

# Convert data type of ActivityDate column from character to date:
eNew$ActivityDate <- as.Date(eNew$ActivityDate, format = "%m/%d/%Y")

timeDf_e2_begin <- Sys.time()
timeDf_e2_begin

# For aggregation at hour level, and distinguish between am and pm, concatenate HH with AM_PM:
# First, split 'HH:MM:SS' into 'HH', 'MM', 'SS' to isolate 'HH':
e_hrlyfinal <- separate(eNew, col= 'HH:MM:SS', into=c('HH', 'MM', 'SS'), sep=':')
head(e_hrlyfinal,2)

timeDf_e2_end <- Sys.time()
timeDf_e2_end

timeDf_e2_diff <- timeDf_e2_begin - timeDf_e2_end

# Now, concatenate HH and AM_PM to distinguish am vs pm:
e_hrlyfinal$Hour <- paste(e_hrlyfinal$AM_PM, e_hrlyfinal$HH, sep = "-")

# Add alphabetic prefix (a to w) to maintain sequence of the 24 hours:
e_hrlyfinal$Hour <- recode_factor(e_hrlyfinal$Hour,
                                  "AM-12"="a-AM12",
                                  "AM-1"="b-AM1",
                                  "AM-2"="c-AM2",
                                  "AM-3"="d-AM3",
                                  "AM-4"="e-AM4",
                                  "AM-5"="f-AM5",
                                  "AM-6"="g-AM6",
                                  "AM-7"="h-AM7",
                                  "AM-8"="i-AM8",
                                  "AM-9"="j-AM9",
                                  "AM-10"="k-AM10",
                                  "AM-11"="l-AM11",
                                  "PM-12"="m-PM12",
                                  "PM-1"="n-PM1",
                                  "PM-2"="o-PM2",
                                  "PM-3"="p-PM3",
                                  "PM-4"="q-PM4",
                                  "PM-5"="r-PM5",
                                  "PM-6"="s-PM6",
                                  "PM-7"="t-PM7",
                                  "PM-8"="u-PM8",
                                  "PM-9"="v-PM9",
                                  "PM-10"="w-PM10",
                                  "PM-11"="x-PM11"
)
head(e_hrlyfinal,2)

# Remove unneeded columns and reorder the rest of the columns:
e_hrlyfinal <- e_hrlyfinal[, c(1, 2, 8, 7)]

# summarize (average) heart rate value by o'clock:
e_hrlyfinal <- e_hrlyfinal %>%
  group_by(Id, ActivityDate, Hour) %>%
  summarise(Value = round(mean(Value)))
head(e_hrlyfinal,2)


write.csv(e_hrlyfinal, "HrtRateHrly.csv", row.names = FALSE)
HrtRateHrly <- read.csv("HrtRateHrly.csv")
tail(HrtRateHrly, 3)

# df 'f' ------------------
# vtable(f) # df 'f': Hourly Calories 
head(f,2)

# Is the data in 'f' an hourly breakdown of the ones in 'a' or 'b'?
# Yes, Calories in df 'f' are less by 13243 calories or 0.6%
sum(f$Calories) #[1] 2152150
sum(b$Calories) #[1] 2165393
sum(a$Calories) #[1] 2165393
sum(f$Calories) - sum(b$Calories) # [1] -13243

# Where is the discrepancy coming from? 
# Let's summarize Calories by customer Id in each dataframe
# and compare the values to find out where the difference is.

# Summarize 'f' 
f1 <- f %>%
  group_by(Id) %>%
  summarise(f_Calories_by_Id = sum(Calories))
head(f1,3)

# Summarize 'b'
b1 <- b %>%
  group_by(Id) %>%
  summarise(b_Calories_by_Id = sum(Calories))
head(b1,3)

# Summarize 'a' 
a1 <- a %>%
  group_by(Id) %>%
  summarise(a_Calories_by_Id = sum(Calories))
head(a1,3)

# Merge data frames 'f1' and 'b1' based on the 'Id' column 
#   (leave out dataframe 'a1' since it's exactly the same as 'b1' ) 
f1_b1 <- merge(f1,b1, by = "Id")

# create a column, 'Diff', in the f1_b1 df to hold the
# differnce between the values of Calories in 'f' and 'b':
f1_b1$Diff <- f1_b1$f_Calories_by_Id - f1_b1$b_Calories_by_Id

# out of the total difference (13243), 86% comes from the top 5 users:
Top5Diffs <- head(f1_b1[order(f1_b1$Diff, decreasing= F),], n = 5)
sum(Top5Diffs$Diff) # [1] -11419 (or 86.2%)     

# *** disregard Calories variable in df 'a' and  Use df 'f' for data analysis despite
#     the difference. 
#     However, for analysis purpose, the 'ActivityHour' column
#     will be split into 'ActivityDate', 'HH:MM:SS', and 'AM_PM'..........

fNew <- separate(f, col=ActivityHour, into=c('ActivityDate', 'HH:MM:SS', 'AM_PM'), sep=' ')
head(fNew,2)

# Now, let's put the 'ActivityDate' column in a proper format:
fNew$ActivityDate <- as.Date(fNew$ActivityDate, format = "%m/%d/%Y")

# Then, for aggregation purpose, at hour level, and make distinction between am and pm, concatenate HH with AM_PM:
# first, split 'HH:MM:SS' into 'HH', 'MM', 'SS' to isolate 'HH':
fNew <- separate(fNew, col= 'HH:MM:SS', 
                 into=c('HH', 'MM', 'SS'), sep=':')
head(fNew,2)

# Now, concatenate HH and AM_PM to make distinction between am and pm hours:
fNew$Hour <- paste(fNew$AM_PM, fNew$HH, sep = "-")

# Finally, add alphabetic prefix (a to w) to maintain sequence of the 24 hours:
fNew$Hour <- recode_factor(fNew$Hour,
                           "AM-12"="a-AM12",
                           "AM-1"="b-AM1",
                           "AM-2"="c-AM2",
                           "AM-3"="d-AM3",
                           "AM-4"="e-AM4",
                           "AM-5"="f-AM5",
                           "AM-6"="g-AM6",
                           "AM-7"="h-AM7",
                           "AM-8"="i-AM8",
                           "AM-9"="j-AM9",
                           "AM-10"="k-AM10",
                           "AM-11"="l-AM11",
                           "PM-12"="m-PM12",
                           "PM-1"="n-PM1",
                           "PM-2"="o-PM2",
                           "PM-3"="p-PM3",
                           "PM-4"="q-PM4",
                           "PM-5"="r-PM5",
                           "PM-6"="s-PM6",
                           "PM-7"="t-PM7",
                           "PM-8"="u-PM8",
                           "PM-9"="v-PM9",
                           "PM-10"="w-PM10",
                           "PM-11"="x-PM11"
)
head(fNew,2)

# Remove unneeded columns and reorder the remaining columns:
fNew <- fNew[, c(1, 2, 8, 7)]

write.csv(fNew, "CaloriesHrly.csv", row.names = FALSE)
CaloriesHrly <- read.csv("CaloriesHrly.csv")
tail(CaloriesHrly, 2)
# *** df 'f' will be considered for analysis ***

# df 'g' ------------------
# vtable(g) # df 'g': Hourly Intensities (No. of minutes on fitness activity)
head(g, 2)

# Let's see if the sum of TotalIntensity in dataframe 'g' amounts to the
# total activity minutes in dataframe 'a', excluding 'SedentaryMinutes'

Intensity_a <- sum(a$VeryActiveMinutes +a$FairlyActiveMinutes +
                     a$LightlyActiveMinutes)
# [1] 213890

Intensity_g <- sum(g$TotalIntensity) 
# [1] 265969 (or 24.3% higher)

# *** df 'g' is not comparable with 'a'; however, use it for analysis. ..................................

#     For analysis purpose, the 'ActivityHour' column
#     will be split into 'ActivityDate', 'HH:MM:SS', and 'AM_PM'..........

gNew <- separate(g, col=ActivityHour, into=c('ActivityDate',
                                             'HH:MM:SS', 'AM_PM'), sep=' ')
head(gNew,2)

# Then, let's put the 'ActivityDate' column in a proper format:
gNew$ActivityDate <- as.Date(gNew$ActivityDate, format = "%m/%d/%Y")

# Then, for aggregation purpose, at hour level, and make distinction between am and pm, concatenate HH with AM_PM:
# first, split 'HH:MM:SS' into 'HH', 'MM', 'SS' to isolate 'HH':

gNew <- separate(gNew, col= 'HH:MM:SS', 
                 into=c('HH', 'MM', 'SS'), sep=':')
head(gNew,2)

# Concatenate HH and AM_PM to to distinguish am vs pm:
gNew$Hour <- paste(gNew$AM_PM, gNew$HH, sep = "-")

# Add alphabetic prefix (a to w) to maintain sequence of the 24 hours:
gNew$Hour <- recode_factor(gNew$Hour,
                           "AM-12"="a-AM12",
                           "AM-1"="b-AM1",
                           "AM-2"="c-AM2",
                           "AM-3"="d-AM3",
                           "AM-4"="e-AM4",
                           "AM-5"="f-AM5",
                           "AM-6"="g-AM6",
                           "AM-7"="h-AM7",
                           "AM-8"="i-AM8",
                           "AM-9"="j-AM9",
                           "AM-10"="k-AM10",
                           "AM-11"="l-AM11",
                           "PM-12"="m-PM12",
                           "PM-1"="n-PM1",
                           "PM-2"="o-PM2",
                           "PM-3"="p-PM3",
                           "PM-4"="q-PM4",
                           "PM-5"="r-PM5",
                           "PM-6"="s-PM6",
                           "PM-7"="t-PM7",
                           "PM-8"="u-PM8",
                           "PM-9"="v-PM9",
                           "PM-10"="w-PM10",
                           "PM-11"="x-PM11"
)
head(gNew,2)

# Remove and reorder columns:
gNew <- gNew[, c(1, 2, 9, 7, 8)]

# Rename 'ActivityDate' column to 'Date':
gNew <- rename(gNew, 'Date' = 'ActivityDate')

write.csv(gNew, "IntensityHrly.csv", row.names = FALSE)
IntensityHrly <- read.csv("IntensityHrly.csv")
tail(IntensityHrly, 2)

# df 'h' ------------------
# vtable(h) # df 'g': Hourly Steps 
head(h, 2)

# Let's see if the total number of Steps in 'h' amounts to the one in 'a' or 'd'.
sum(a$TotalSteps)
sum(d$StepTotal)
sum(h$StepTotal) # (104280 steps or 1.5% less)

# *** Despite the difference, this df will be used in the data analysis. 
#     For analysis purpose, however, 'ActivityHour' column will be split into:
#     'ActivityDate', 'HH:MM:SS', and 'AM_PM' 

hNew <- separate(h, col=ActivityHour, into=c('ActivityDate',
                                             'HH:MM:SS', 'AM_PM'), sep=' ')
head(hNew,2)

# Now, let's put the 'ActivityDate' column in a proper format:
hNew$ActivityDate <- as.Date(hNew$ActivityDate, format = "%m/%d/%Y")

# Then, for aggregation purpose, at hour level, and make distinction between
# am and pm, concatenate HH with AM_PM:
# first, split 'HH:MM:SS' into 'HH', 'MM', 'SS' to isolate 'HH':
hNew <- separate(hNew, col= 'HH:MM:SS', into=c('HH', 'MM', 'SS'), sep=':')
head(hNew,2)

# Concatenate HH and AM_PM to to distinguish am vs pm:
hNew$Hour <- paste(hNew$AM_PM, hNew$HH, sep = "-")

# Now, add alphabetic prefix (a to w) to maintain sequence of the 24 hours:

hNew$Hour <- recode_factor(hNew$Hour,
                           "AM-12"="a-AM12",
                           "AM-1"="b-AM1",
                           "AM-2"="c-AM2",
                           "AM-3"="d-AM3",
                           "AM-4"="e-AM4",
                           "AM-5"="f-AM5",
                           "AM-6"="g-AM6",
                           "AM-7"="h-AM7",
                           "AM-8"="i-AM8",
                           "AM-9"="j-AM9",
                           "AM-10"="k-AM10",
                           "AM-11"="l-AM11",
                           "PM-12"="m-PM12",
                           "PM-1"="n-PM1",
                           "PM-2"="o-PM2",
                           "PM-3"="p-PM3",
                           "PM-4"="q-PM4",
                           "PM-5"="r-PM5",
                           "PM-6"="s-PM6",
                           "PM-7"="t-PM7",
                           "PM-8"="u-PM8",
                           "PM-9"="v-PM9",
                           "PM-10"="w-PM10",
                           "PM-11"="x-PM11"
)
head(hNew,2)

# Remove and reorder columns:
hNew <- hNew[, c(1, 2, 8, 7)]

write.csv(hNew, "StepsHrly.csv", row.names = FALSE)
StepsHrly <- read.csv("StepsHrly.csv")
tail(StepsHrly, 2)

# *** df 'h' will be included in analysis

# df 'i' ------------------
# vtable(i) # df 'i' is Minute level Calories
head(i,2)

# compare it with the daily ('a') and hourly ('f') Calories data (:
sum(a$Calories) #[1] 2165393
sum(f$Calories) #[1] 2152150    # (0.62% less than 'a')
sum(i$Calories) #[1] 2151588    # (0.64% less than 'a')

# *** drop df 'i' since we have df 'CaloriesHrly' for hour-level analysis ***

# df 'j' ------------------
# vtable(j) # df 'j' contains wide data for minute-level Calories
head(j, 1) 

# Is data in df 'j' different from dfs 'a', 'b', 'f', and 'i'?
# Let's convert the wide data in df 'j' to long (narrow) data to sum it to
# and compare it with the other dfs:

jlong <- j %>%
  gather("Minute", "Calories", "Calories00":"Calories59")
head(jlong,2)

sum(jlong$Calories)#[1] 2106440 
# less by 58953 calories or 2.7%, from dfs 'a' and 'b'
sum(f$Calories)    #[1] 2152150  
sum(i$Calories)    #[1] 2151588
sum(b$Calories)    #[1] 2165393
sum(a$Calories)    #[1] 2165393

# *** drop df 'j' since we dont intend for a minute level analysis and 
# we have the hour-level data for hour-level analysis.

# df 'k' ------------------
# vtable(k) # k is a Minute level Intensities in a long form
# Intensity Values: 0 = Sedentary, 1 = Light, 2 = Moderate, 3 = Very Active
tail(k,2)

# out of the total 1325580 minutes of intensity, 
#   1112102  were sedentary (83.9%)
#   180891   light          (13.7%)
#   12749    moderate       (0.96%) and
#   19838    very active    (1.5%)
# 
length(k$ActivityMinute)
# [1] 1325580
table(k$Intensity)
#       0       1       2       3 
# 1112102  180891   12749   19838

# *** we drop this df ('k') since we don't intend to do minute-level analysis.

# df 'l' ------------------
# vtable(l) # df 'l' is a Minute level Intensities in a WIDE form
head(l,2)

# To compare the wide form and long form data, 
# let's convert wide data to long data:

L_long <- l %>%
  gather("Minute", "Intensity", "Intensity00":"Intensity59")

head(L_long,3)
length(L_long$Intensity)
# [1] 1298700 (total minutes of intensity)

table(L_long$Intensity)

#           0       1        2           3 
# (sedentary) (light) (moderate) (very active)

#     1089591  176816    12821       19472 
#       83.9%   13.6%    0.98%        1.5%

#df 'l' and 'k' have the same proportion of categories
#     except that df 'l' has a 26880 minutes difference: 
length(k$Intensity) - length(L_long$Intensity)
# [1] 26880 

#  *** both dfs 'k' and 'l' will be dropped; 
#      hourly level intensity data will be used ***

# df 'm' ------------------
# vtable(m) # 'm' consists of METs (metabolic equivalents).
head(m,2)

# One MET is defined as the energy you use when you're resting or
# sitting still. An activity that has a value of 4 METs means
# you're exerting 4 times the energy than you would if you were sitting still.
# https://www.healthline.com/health/what-are-mets  Oct 21, 2019

# First, Change 'ActivityMinute' into 'date' and 'time' format:
mNew <- separate(m, col=ActivityMinute, into=c('ActivityDate',
                                               'HH:MM:SS', 'AM_PM'), sep=' ')
head(mNew,2)
# Put the date column in a proper format:
mNew$ActivityDate <- as.Date(mNew$ActivityDate, format = "%m/%d/%Y")

mNew <- separate(mNew, col= 'HH:MM:SS', 
                 into=c('HH', 'MM', 'SS'), sep=':')
head(mNew,2)

# Concatenate HH and AM_PM to distinguish am vs pm:
mNew$Hour <- paste(mNew$AM_PM, mNew$HH, sep = "-")

# Add alphabetic prefix (a to w) to maintain sequence of the 24 hours:
mNew$Hour <- recode_factor(mNew$Hour,
                           "AM-12"="a-AM12",
                           "AM-1"="b-AM1",
                           "AM-2"="c-AM2",
                           "AM-3"="d-AM3",
                           "AM-4"="e-AM4",
                           "AM-5"="f-AM5",
                           "AM-6"="g-AM6",
                           "AM-7"="h-AM7",
                           "AM-8"="i-AM8",
                           "AM-9"="j-AM9",
                           "AM-10"="k-AM10",
                           "AM-11"="l-AM11",
                           "PM-12"="m-PM12",
                           "PM-1"="n-PM1",
                           "PM-2"="o-PM2",
                           "PM-3"="p-PM3",
                           "PM-4"="q-PM4",
                           "PM-5"="r-PM5",
                           "PM-6"="s-PM6",
                           "PM-7"="t-PM7",
                           "PM-8"="u-PM8",
                           "PM-9"="v-PM9",
                           "PM-10"="w-PM10",
                           "PM-11"="x-PM11"
)
head(mNew,2)

# Remove and reorder columns:
mNew <- mNew[, c(1, 2, 8, 7)]

mNew <- mNew %>%
  group_by(Id, ActivityDate, Hour) %>%
  summarise(METs = round(mean(METs)))
head(mNew,2)

write.csv(mNew, "METsHrly.csv", row.names = FALSE)
METsHrly <- read.csv("METsHrly.csv")
head(METsHrly, 2)
# *** df 'm' will be used for analysis ***

# df 'n' ------------------
# vtable(n) # minute-level sleep state
head(n,2)

# Change ActivityMinute into 'date' format
nNew <- separate(n, col=date, into=c('ActivityDate',
                                     'HH:MM:SS', 'AM_PM'), sep=' ')
head(nNew,2)

# Now, let's put the 'ActivityDate' column in a proper format:
nNew$ActivityDate <- as.Date(nNew$ActivityDate, format = "%m/%d/%Y")

# Then, for aggregation purpose, at hour level, and make distinction between
# am and pm, concatenate HH with AM_PM:
# first, split 'HH:MM:SS' into 'HH', 'MM', 'SS' to isolate 'HH':
nNew <- separate(nNew, col= 'HH:MM:SS', into=c('HH', 'MM', 'SS'), sep=':')
head(nNew,2)

# Concatenate HH and AM_PM to distinguish am vs pm:
nNew$Hour <- paste(nNew$AM_PM, nNew$HH, sep = "-")

# Now, add alphabetic prefix (a to w) to maintain sequence of the 24 hours:
nNew$Hour <- recode_factor(nNew$Hour,
                           "AM-12"="a-AM12",
                           "AM-1"="b-AM1",
                           "AM-2"="c-AM2",
                           "AM-3"="d-AM3",
                           "AM-4"="e-AM4",
                           "AM-5"="f-AM5",
                           "AM-6"="g-AM6",
                           "AM-7"="h-AM7",
                           "AM-8"="i-AM8",
                           "AM-9"="j-AM9",
                           "AM-10"="k-AM10",
                           "AM-11"="l-AM11",
                           "PM-12"="m-PM12",
                           "PM-1"="n-PM1",
                           "PM-2"="o-PM2",
                           "PM-3"="p-PM3",
                           "PM-4"="q-PM4",
                           "PM-5"="r-PM5",
                           "PM-6"="s-PM6",
                           "PM-7"="t-PM7",
                           "PM-8"="u-PM8",
                           "PM-9"="v-PM9",
                           "PM-10"="w-PM10",
                           "PM-11"="x-PM11"
)
head(nNew,2)

# Remove and reorder columns:
nNew <- nNew[, c(1, 2, 9, 7)]

write.csv(nNew, "SleepStateHrly.csv", row.names = FALSE)
SleepStateHrly <- read.csv("SleepStateHrly.csv")
tail(SleepStateHrly,3)

# df 'o' ------------------
# vtable(o) # 'o' is minute-level steps data in Long format
head(o,2)

sum(StepsHrly$StepTotal) #[1] 7075356
sum(o$Steps) #[1] 7073549 (1807 or 0.025% steps less)
# df 'o' will be dropped since it is the same as the hourly steps

# df 'p' ------------------
# vtable(p) # 'p' is a minute-level steps data in WIDE format
head(p,2)

# Adding up the sums of sums of steps for each minute:
stepsLong <- p %>%
  gather("Minute", "Steps", "Steps00":"Steps59")
head(stepsLong,2)

# compare with hourly steps data:
sum(stepsLong$Steps) # [1] 6938153 (135396 steps or 1.95% less)
sum(StepsHrly$StepTotal) # [1] 7073549 

# *** df 'p' will be dropped; hourly steps data will be used for analysis.

# df 'q' ------------------
# vtable(q) # 'q' is a Daily Sleep frequency and minutes slept
head(q,2)

unique(q$Id) # there are only 24 device user in this table

# Split the 'SleepDay' column to isolate the 'date' part of the datetime
qNew <- separate(q, col=SleepDay, into=c('ActivityDate','HH:MM', 'am_pm'), sep=' ')
head(qNew,3)

table(qNew$`HH:MM`) # it's all 12:00:00
table(qNew$`am_pm`) # it's all AM
qNew <- qNew[-c(3,4)] # remove the unwanted 'HH:MM' and 'am_pm' columns

# Step 2. Now, let's put the 'ActivityDate' column in a proper format:
qNew$ActivityDate <- as.Date(qNew$ActivityDate, format = "%m/%d/%Y")

# Rename ActivityDate to Date:
qNew <- rename(qNew, 'Date' = 'ActivityDate')

write.csv(qNew, "SleepAmtFreqDaily.csv", row.names = FALSE)
SleepAmtFreqDaily <- read.csv("SleepAmtFreqDaily.csv")
head(SleepAmtFreqDaily,2)

# *** df 'q' will be considered for aanalysis

# df 'r' ------------------
# vtable(r) # 'q' is a Daily Sleep frequency and minutes slept
unique(r$Id) # Only 8 users out of 33 have records.

# *** data in this df is too small for analysis; it will be dropped. 

# *******************************************************
# -------------------------------------------------------

# 4.  ANALYSIS

## List and rename the processed data frames ready for analysis:

# 1. Distance (the Distance column from df 'a' is considered for analysis).
# distanceDaily
distanceDaily <- a[c(1,2,4)]
head(distanceDaily,2)
distance_a <- distanceDaily # renamed df

# 2. Steps
# StepsHrly
head(StepsHrly,2)
steps_a <- StepsHrly # renamed df

# 2. Heart Rate: HrtRateHrly
head(HrtRateHrly,2)
hrtRate_a <- HrtRateHrly # renamed df

# 3. Calories: CaloriesHrly
head(CaloriesHrly,2)
calor_a <- CaloriesHrly # renamed df

# 4. Intensity: IntensityHrly
head(IntensityHrly,2)
intens_a <- IntensityHrly # renamed df

# 6. Metabolic Equivalents: METsHrly
head(METsHrly,12)
METs_a <- METsHrly # renamed df

# 7. Sleep State
# SleepStateHrly
head(SleepStateHrly,2)

# 8. Sleep Amount and Frequency (not available at hourly level)
# SleepAmtFreqDaily
head(SleepAmtFreqDaily,2) 

# -------------------------------------------------------------

## Summaries by: Id, Hour, Date:

# 1 --- distance_a ---
distance_a <- distanceDaily

DistanceById <- distance_a %>% 
  group_by(Id) %>%
  summarise(DistanceById = sum(round(TotalDistance))) # all the 31 days
head(DistanceById,2)

write.csv(DistanceById, "DistanceById.csv", row.names = FALSE)
dfDistanceById <- read.csv("DistanceById.csv")
head(dfDistanceById,2)

# Hourly distance not available

DistanceByDate <- distance_a %>%
  group_by(ActivityDate) %>%
  summarise(DistanceByDate = sum(round(TotalDistance)))
head(DistanceByDate,2)

write.csv(DistanceByDate, "DistanceByDate.csv", row.names = FALSE)
dfDistanceByDate <- read.csv("DistanceByDate.csv")
head(dfDistanceByDate,2)

# -----------------------------------------------------

# 2 --- steps_a ---
steps_a <- StepsHrly

stepsById <- steps_a %>%
  group_by(Id) %>%
  summarise(StepsById = round(sum(StepTotal))) # all the 31 days
head(stepsById,2)

write.csv(stepsById, "stepsById.csv", row.names = FALSE)
dfstepsById <- read.csv("stepsById.csv")
head(dfstepsById,2)

stepsByHour <- steps_a %>%
  group_by(Hour) %>%
  summarise(StepsByHour = round(sum(StepTotal)))
head(stepsByHour,2)

write.csv(stepsByHour, "stepsByHour.csv", row.names = FALSE)
dfstepsByHour <- read.csv("stepsByHour.csv")
head(dfstepsByHour,2)

stepsByDate <- steps_a %>%
  group_by(ActivityDate) %>%
  summarise(StepsByDate = round(sum(StepTotal)))
head(stepsByDate,2)

write.csv(stepsByDate, "stepsByDate.csv", row.names = FALSE)
dfstepsByDate <- read.csv("stepsByDate.csv")
head(dfstepsByDate,2)
# ----------------------------------------------

# 3 --- intens_a ---

intens_a <- IntensityHrly

intensById <- intens_a %>%
  group_by(Id) %>%
  summarise(TotInt = sum(TotalIntensity ),
            AvgInt = sum(round(AverageIntensity))) # all the 31 days
head(intensById,2)

write.csv(intensById, "intensById.csv", row.names = FALSE)
dfintensById <- read.csv("intensById.csv")
head(dfintensById,2)

intensByHour <- intens_a %>%
  group_by(Hour) %>%
  summarise(TotInt = sum(TotalIntensity),
            AvgInt = sum(round(AverageIntensity)))
head(intensByHour,2)

write.csv(intensByHour, "intensByHour.csv", row.names = FALSE)
dfintensByHour <- read.csv("intensByHour.csv")
head(dfintensById,2)

intensByDate <- intens_a %>%
  group_by(Date) %>%
  summarise(TotInt = sum(TotalIntensity),
            AvgInt = sum(round(AverageIntensity)))
head(intensByDate,2)

write.csv(intensByDate, "intensByDate.csv", row.names = FALSE)
dfintensByDate <- read.csv("intensByDate.csv")
head(dfintensByDate,2)
# ------------------------------------------------

# 4. --- hrtRate_a ---
hrtRate_a <- HrtRateHrly

hrtRateById <- hrtRate_a %>%
  group_by(Id) %>%
  summarise(meanHrtRate = round(mean(Value)))
head(hrtRateById,2)

write.csv(hrtRateById, "hrtRateById.csv", row.names = FALSE)
dfhrtRateById <- read.csv("hrtRateById.csv")
head(dfhrtRateById,2)

hrtRateByHour <- hrtRate_a %>%
  group_by(Hour) %>%
  summarise(meanHrtRate = round(mean(Value)))
head(hrtRateByHour,2)

write.csv(hrtRateByHour, "hrtRateByHour.csv", row.names = FALSE)
dfhrtRateByHour <- read.csv("hrtRateByHour.csv")
head(dfhrtRateByHour,2)

hrtRateByDate <- hrtRate_a %>%
  group_by(ActivityDate) %>%
  summarise(meanHrtRate = round(mean(Value)))
head(hrtRateByDate,2)

write.csv(hrtRateByDate, "hrtRateByDate.csv", row.names = FALSE)
dfhrtRateByDate <- read.csv("hrtRateByDate.csv")
head(dfhrtRateByDate,2)
# -------------------------------------------

# 5. --- Calor_a ---

calor_a <- CaloriesHrly

CalorieById <- calor_a %>%
  group_by(Id) %>%
  summarise(Calories = sum(Calories)) # All the 31 days
head(CalorieById,2)

write.csv(CalorieById, "CalorieById.csv", row.names = FALSE)
dfCalorieById <- read.csv("CalorieById.csv")
head(dfCalorieById,2)

CalorieByDate <- calor_a %>%
  group_by(ActivityDate) %>%
  summarise(Calories = sum(Calories))
head(CalorieByDate,2)

write.csv(CalorieByDate, "CalorieByDate.csv", row.names = FALSE)
dfCalorieByDate <- read.csv("CalorieByDate.csv")
head(dfCalorieByDate,2)

CalorieByHour <- calor_a %>%
  group_by(Hour) %>%
  summarise(Calories = sum(Calories))
head(CalorieByHour,2)

write.csv(CalorieByHour, "CalorieByHour.csv", row.names = FALSE)
dfCalorieByHour <- read.csv("CalorieByHour.csv")
head(dfCalorieByHour,2)
# -----------------------------------

# 6. --- METs_a ---

METs_a <- METsHrly

METsById <- METs_a %>%
  group_by(Id, ActivityDate, Hour) %>%
  summarise(METs = round(mean(METs)))
head(METsById,2)

write.csv(METsById, "METsById.csv", row.names = FALSE)
dfMETsById <- read.csv("METsById.csv")
head(dfMETsById,2)

METsByDate <- METs_a %>%
  group_by(ActivityDate) %>%
  summarise(METs = sum(METs))
head(METsByDate,2)

write.csv(METsByDate, "METsByDate.csv", row.names = FALSE)
dfMETsByDate <- read.csv("METsByDate.csv")
head(dfMETsByDate,2)

METsByHour <- METs_a %>%
  group_by(Hour) %>%
  summarise(METs = sum(METs))
tail(METsByHour,2)

write.csv(METsByDate, "METsByHour.csv", row.names = FALSE)
dfMETsByHour <- read.csv("METsByHour.csv")
head(dfMETsByHour,2)
# --------------------------------------

# 7 --- sleep_state_a ---

sleep_state_a <- SleepStateHrly

# Rename variable 'value' to 'sleepState'
sleep_state_a <- rename(sleep_state_a, 'sleepState' = 'value')
SleepStateHrly <- rename(SleepStateHrly, 'sleepState' = 'value')


# Rename levels (state ofsleep):
sleep_state_a$sleepState <- as.factor(sleep_state_a$sleepState)
levels(sleep_state_a$sleepState) <- c("asleep", "restless", "awake")
head(sleep_state_a,2)

table(sleep_state_a$sleepState)
# asleep restless    awake
# 172480    14023     2018
# ------------------------

# 8. --- sleep_a ---
sleep_a <- SleepAmtFreqDaily

SleepAmtById <- sleep_a %>%
  group_by(Id) %>%
  summarise(TotalSleepRecords = sum(TotalSleepRecords),
            TotalMinutesAsleep = sum(TotalMinutesAsleep),
            TotalTimeInBed = sum(TotalTimeInBed))
head(SleepAmtById,2)

write.csv(SleepAmtById, "SleepAmtById.csv", row.names = FALSE)
dfSleepAmtById <- read.csv("SleepAmtById.csv")
head(dfSleepAmtById,2)

# sleep amount and frequency by hour is not available.

SleepAmtByDate <- sleep_a %>%
  group_by(Date) %>%
  summarise(TotalSleepRecords = sum(TotalSleepRecords),
            TotalMinutesAsleep = sum(TotalMinutesAsleep),
            TotalTimeInBed = sum(TotalTimeInBed))
head(SleepAmtByDate,2)

write.csv(SleepAmtByDate, "SleepAmtByDate.csv", row.names = FALSE)
dfSleepAmtByDate <- read.csv("SleepAmtByDate.csv")
head(dfSleepAmtByDate,2)
# -----------------------------------------------------------------
# 
TimeEnd <- Sys.time()
TimeEnd


TimeBegining - TimeEnd

# ------------------
