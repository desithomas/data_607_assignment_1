# load in the tidyverse
library(tidyverse)
library(dplyr)
library(janitor)

## RAW URL is assigned to the object dataset_url
dataset_url <- "https://raw.githubusercontent.com/desithomas/data_607_assignment_1/refs/heads/main/StudentPerformanceFactors.csv"


# dataset_url has been passed into read_csv() function
student_data <- read_csv(dataset_url)

student_data_modded <- student_data %>%  
  clean_names() %>%
  select(hours_studied, exam_score, tutoring_sessions) %>% 
  filter(hours_studied > 0, tutoring_sessions > 0)

glimpse(student_data_modded)

final_data <- student_data %>% 
  clean_names() %>% 
  mutate(
    support_levels = case_when(
      tutoring_sessions >= 6 ~ "3. High (6+)",  
      tutoring_sessions >= 2 ~ "2. Mid (2-5)",   
      TRUE                   ~ "1. Low (0-1)"    
    )
  ) %>% 
  select(exam_score, support_levels, tutoring_sessions) %>% 
  filter(!is.na(exam_score))

# Now ggplot has ONE column with 3 boxes to draw
ggplot(data = final_data, mapping = aes(x = support_levels, y = exam_score, fill = support_levels)) + 
  geom_boxplot() + 
  labs(title = "Number of Tutoring Sessions (Support Levels) vs. Student Exam Scores",
       x = "Support Level")





glimpse(final_data)

summary(final_data$tutoring_sessions)
table(final_data$tutoring_sessions)

ggplot(data = final_data, mapping = aes(x = support_levels, y = exam_score, fill = support_levels)) + 
  geom_boxplot() + 
  labs(title = "Number of Tutoring Sessions (Support Levels) vs. Student Exam Scores",
       x = "Support Level")