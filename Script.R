# Install packages.
install.packages("tidyverse")
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)
library(jsonlite)


# Explore JSON format for sleep records from Garmin Connect.
sleep_JSON_format <- read_json("C:/Users/lross/Documents/R/Sleep JSON Files/2024-05-27_2024-09-04_118063874_sleepData.json")


# Convert JSON data to data frames. Each JSON contains up to 100 days of data.
sleep1_df <- fromJSON("C:/Users/lross/Documents/R/Sleep JSON Files/2024-05-27_2024-09-04_118063874_sleepData.json")
sleep2_df <- fromJSON("C:/Users/lross/Documents/R/Sleep JSON Files/2024-09-04_2024-12-13_118063874_sleepData.json")
sleep3_df <- fromJSON("C:/Users/lross/Documents/R/Sleep JSON Files/2024-12-13_2025-03-23_118063874_sleepData.json")


# Remove spo2 data that is only included in the sleep2_df (all values are null).
sleep2_df <- select(sleep2_df, -18)


# Combine the data frames and cast the calendar date as ymd .
sleep_data_compiled <- tibble(bind_rows(sleep1_df, sleep2_df, sleep3_df))
sleep_data_compiled$calendarDate <- ymd(sleep_data_compiled$calendarDate)


# Clean data:
# Reduce the data to 4 months (120 days) before/after birth of daughter
# (2024.07.15 to 2025.03.12), and update date to reflect ET instead of GMT.
# Add sleep duration column, and rename sleep score columns for clarity.
sleep_4_months <- filter(sleep_data_compiled, between(sleep_data_compiled$calendarDate, ymd('2024-07-16'), ymd('2025-03-14'))) %>%
  mutate(time_relative_to_birth = ifelse(calendarDate < '2024-11-12', "before", "after")) %>%
  mutate(sleep_duration_hours = as.numeric(ymd_hms(sleepEndTimestampGMT) - ymd_hms(sleepStartTimestampGMT)) - awakeSleepSeconds/3600) %>%
  mutate(remSleepSeconds = if_else(is.na(remSleepSeconds),0,remSleepSeconds)) %>%
  rename(date = calendarDate)
sleep_4_months$date <- sleep_4_months$date - 1

#Create summary statistics.
before_and_after_summary <- summarize(sleep_4_months,
                                        mean_sleep_hours = mean(sleep_duration_hours),
                                        mean_sleep_score = mean(sleepScores$overallScore),
                                        mean_sleep_stress = mean(avgSleepStress),
                                        mean_awake_mins = mean(awakeSleepSeconds)/60,
                                        mean_awake_count = mean(awakeCount),
                                        mean_restless_moments = mean(restlessMomentCount),
                                          .by = time_relative_to_birth)


# Plots time asleep.
sleep_stages <- summarize(sleep_4_months,
                         Deep = mean(deepSleepSeconds)/3600,
                         Light = mean(lightSleepSeconds)/3600,
                         REM = mean(remSleepSeconds)/3600,
                         .by = time_relative_to_birth)

stages_df_long <- pivot_longer(sleep_stages, -time_relative_to_birth, values_to = "duration", names_to = "Stage") %>%
  mutate(duration_rd = format(round(duration,2)))
Period <- forcats::fct_rev(stages_df_long$time_relative_to_birth)

ggplot(data=stages_df_long, aes(x=Period, y=duration))+
  geom_col(aes(fill=Stage))+
  labs(x="Period", y="Duration (hours)", title="Average Sleep Duration: Before & After")+
  geom_text(aes(label=duration_rd), size=5, hjust=0.5, vjust=3, position="stack")+
  theme(text = element_text(size = 16))


# Plot sleep score summary. 
sleep_score_summary <- summarize(sleep_4_months,
                                 Quality = mean(sleepScores$qualityScore),
                                 Duration = mean(sleepScores$durationScore),
                                 Recovery = mean(sleepScores$recoveryScore),
                                 Deep_Sleep = mean(sleepScores$deepScore),
                                 REM = mean(sleepScores$remScore),
                                 Light_Sleep = mean(sleepScores$lightScore),
                                 Awakenings = mean(sleepScores$awakeningsCountScore),
                                 Awake_Time = mean(sleepScores$awakeTimeScore),
                                 Restfulness = mean(sleepScores$restfulnessScore),
                                 Interruptions = mean(sleepScores$interruptionsScore),
                                 .by = time_relative_to_birth)

score_df_long <- pivot_longer(sleep_score_summary, -time_relative_to_birth, values_to = "score", names_to = "type")
Period <- forcats::fct_rev(score_df_long$time_relative_to_birth)

ggplot(data=score_df_long, aes(x=type, y=score))+
  geom_col(aes(fill=Period), position = "dodge")+
  labs(x="Score Type", y="Sleep Score", title="Before and After Look at Sleep Scores")+
  theme(text = element_text(size = 16), axis.text.x = element_text(angle = 45, hjust = 1))

#Export file for external visualization.
write.csv(sleep_4_months, "C:/Users/lross/Documents/R/Sleep JSON Files/sleep_data_visualization.csv")
