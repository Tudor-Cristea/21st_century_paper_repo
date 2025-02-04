## Calculate numbers in section 3.1 Table 1 (page 4) and t-tests for the quartiles

library(dplyr)
library(psych)

# Load the dataset 
combined_data<- read.csv(paste0(data_path, "combined_data.csv"))


## Calculate the alphas

  # Expressive digital communication skills
alpha(select(combined_data, internet_literacy_1, internet_literacy_2, internet_literacy_3)) #the alpha

  # Social media communication
alpha(select(combined_data, contacts_1, contacts_2, contacts_3)) #the alpha

  # Loneliness
alpha(select(combined_data, loneliness_1, loneliness_2, loneliness_3, loneliness_4, #the alpha
             loneliness_5, loneliness_6, loneliness_7, loneliness_8))

  # Online contacts
alpha(select(combined_data, online_students, online_friends, online_family)) #the alpha

  # Offline contacts
alpha(select(combined_data, offline_students, offline_friends, offline_family)) #the alpha



## Calculate the averages (M) and SD of the scales (Table 1 in the paper)
table_1<- combined_data %>%
  group_by(quartile) %>%
  summarise(
    n = n(),
    Skills_M = mean(Skills), # We don't need the usual "na.rm= TRUE" since we already filtered the missing values
    Skills_SD = sd(Skills),
    Communication_M = mean(Communication),
    Communication_SD = sd(Communication),
    OnlineContacts_M = mean(Online),
    OnlineContacts_SD = sd(Online),
    OfflineContacts_M = mean(Offline),
    OfflineContacts_SD = sd(Offline),
    Loneliness_M = mean(Loneliness),
    Loneliness_SD = sd(Loneliness)
  ) %>%
  mutate(across(where(is.numeric), ~ round(., 2))) # Round all numeric columns to 2 decimal places

# View Table 1
print(table_1, width = Inf)


## Calculate the difference in averages between quartiles (t-tests)

#T-tests, the first is Welch's and the second is the student t-test (in the paper we used the later)
t.test(Online ~ quartile, combined_data)
t.test(Offline ~ quartile, combined_data)

t.test(Online ~ quartile, combined_data, var.equal = TRUE)
t.test(Offline ~ quartile, combined_data, var.equal = TRUE)


## Save the final datasets per quartile for the next script
q4_data<- combined_data %>%
  filter(quartile== "Q4")
write.csv(q4_data, paste0(data_path, "final_q4.csv"))

q1_data<- combined_data %>%
  filter(quartile== "Q1")
write.csv(q1_data, paste0(data_path, "final_q1.csv"))





