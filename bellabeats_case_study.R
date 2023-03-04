#Install packages and load libraries needed for data analysis

install.packages("tidyverse")

library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

#Import Datasets

fit_bit_gtrend <- read_csv('FitBit google trend - cleaned.csv')

activity <- read_csv('y dailyActivity_merged - dailyActivity_merged.csv')

sleep <- read_csv('y sleepDay_merged - cleaned.csv')

weight <- read_csv('y weightLogInfo_merged - cleaned.csv')

hourly_steps <- read.csv('y hourlySteps_merged - cleaned.csv')

steps <- read.csv('hourly_steps.csv')

#Google Trend Graph

ggplot(data=fit_bit_gtrend, mapping = aes(x=Week, y=interest_over_time))+
  geom_line()+
  labs(title="FitBit Google Trends Data", y="Interest Over Time")


#Data exploration - looking for patterns or trends between dataframes

activity_weight <- inner_join(activity, weight, by = "Id") %>% 
  select(Id, TotalSteps, SedentaryMinutes, BMI, WeightPounds) %>% 
  group_by(Id) %>% 
  summarize(mean_steps=mean(TotalSteps,na.rm = T),
            mean_sedentary_minutes=mean(SedentaryMinutes,na.rm = T),
            mean_bmi=mean(BMI,na.rm = T),
            mean_weightlbs=mean(WeightPounds,na.rm = T))  

activity_weight #only 1 record in the overweight BMI category

ggplot(data=activity_weight, mapping = aes(x=mean_steps, y=mean_weightlbs))+
  geom_point() +
  labs(title="User Weight and Average Daily Steps",
       y="User Weight in lbs", x="Avg. Daily Steps") #not enough user data to determine correlation


daily_activity <- activity %>% 
  group_by(ActivityDate) %>%
  summarize(mean_steps=mean(TotalSteps))

            
daily_activity %>% 
  arrange(mean_steps)#not enough data to show if certain days of the week have less activity
         
ggplot(data=daily_activity, aes(x=ActivityDate, y=mean_steps)) +
  geom_bar(stat = "identity")+
  labs(title="Average Steps by Date", y="Avg. Steps", x="Date")+
  theme(axis.text.x = element_text(angle = 90))

summary_steps <- steps %>% #determine hour of day with most steps
  group_by(Time) %>%
  summarize(mean_steps=mean(StepTotal)) %>% 
  arrange(Time)

summary_steps

ggplot(data=summary_steps, aes(x=Time, y=mean_steps))+
  geom_line()+
  labs(title="Average Steps by Hour of Day", y="Avg. Steps", x="Time (24hr format)")

ggplot(data=summary_steps, aes(x=Time, y=mean_steps)) +
  geom_bar(stat = "identity")+
  labs(title="Average Steps by Hour of Day", y="Avg. Steps", x="Time (24hr format)")
  #evenings 6pm and another peak at noon

activity_logs<- activity %>% #most users track daily
  count(Id)

sleep_logs<- sleep %>% #some users track
  count(Id)


weight_logs<- weight %>% #few users track
  count(Id)


logs <- full_join(activity_logs, sleep_logs, by = "Id") %>%
  full_join(weight_logs, by = "Id") %>% 
  rename(activity = n.x, sleep = n.y, weight=n)

logs #compare all logs -activity, sleep, weight

logs_long <- logs %>%
  pivot_longer(cols = c(activity, sleep, weight), names_to = "type", values_to = "count")

logs_bar<- ggplot(logs_long, aes(x = type, y = count, fill = type)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Logs Recorded by Type", x = "Type", y = "Total Logs")

logs_bar #bar chart of logs recorded
