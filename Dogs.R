#Install and load packages

install.packages("tidyverse")
library(readr) 
library(ggplot2)
library(dplyr)

#SQL queried csv file imported
dogs <- read_csv("dog_intelligence_size_v3_R.csv")
View(dogs)

#started with a bar graph to visualize average weight and intelligence classification

class_weight <- dogs %>% 
  group_by(classification) %>%
  summarize(avg_weight=mean(median_weight)) %>% 
  arrange(desc(avg_weight))

class_weight

dog_barchart <- ggplot(data = class_weight) +
  geom_bar(mapping = aes(y = reorder(classification, avg_weight), x = avg_weight, fill=classification), stat = "identity")+
  labs(title="Weight of Classification", y="Classification", x="Average Weight lbs")+
  guides(fill=FALSE)

dog_barchart

#scatter plots created to help illustrate how obedience and repetitions are related 
#included classification to the plot to show it was based on the results of both

dog_intelligence <- ggplot(data=dogs)+
  geom_point(mapping = aes(x = obey_rate, y = median_reps, color = classification, size = 0.6))+
  facet_wrap(~obey_rate~median_reps)+
  labs(title="Obey vs Reps", subtitle="By Classification",
       x="Obey On First Command %", y="Median Repetitions to Learn New Command")+
  guides(size=FALSE)

dog_intelligence

#scatter plot to help answer the question and illustrate correlation of dog size vs intelligence

dog_size <- ggplot(data=dogs)+
  geom_point(mapping=aes(x=median_height, y=median_weight, color=classification))+
  geom_smooth(mapping=aes(x=median_height, y=median_weight), color="black", size=.5)+
  labs(title="Dog Size and Intelligence Classification", x="Median Weight (lbs)", y="Median Height (in)")

dog_size









