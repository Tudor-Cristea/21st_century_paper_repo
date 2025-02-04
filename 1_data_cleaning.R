#Clean the data and calculate the alphas

library(dplyr)
library(haven)
library(data.table)

# Load the data, remove a problematic column, and add the quartile number
data_path<- "your_data_path"

Q4_data<- read_dta(paste0(data_path, "raw_data_Q4.dta")) %>%
  select(-H1_3_TEXT) %>% #remove this variable, it helps with binding
  mutate(quartile= "Q4")

Q1_data<- read_dta(paste0(data_path, "raw_data_Q1.dta"))%>%
  select(-H1_3_TEXT) %>%
  mutate(quartile= "Q1")

# Bind the two datasets
combined_data <- bind_rows(Q4_data, Q1_data)


# Calculate the missing variables
combined_data <- combined_data %>%
  mutate(
    missing = rowSums(
      is.na(select(., loneliness_1, loneliness_2, loneliness_3, loneliness_4, 
                   loneliness_5, loneliness_6, loneliness_7, loneliness_8, 
                   internet_literacy_1, internet_literacy_2, internet_literacy_3, 
                   contacts_1, contacts_2, contacts_3, online_students, 
                   online_friends, online_family, offline_students, 
                   offline_friends, offline_family))
    )
  ) %>%
filter(missing == 0) # Filter rows with missing values greater than 0

# Check if the filter worked (it did!)
table(combined_data$missing)
table(combined_data$quartile, combined_data$missing)


## Calculate the scales (average of items)

# Expressive digital communication skills
combined_data$Skills<- rowMeans(combined_data[, c("internet_literacy_1", #the scale
                                            "internet_literacy_2", 
                                            "internet_literacy_3")]) 
 
# Social media communication
combined_data$Communication<- rowMeans(combined_data[, c("contacts_1", "contacts_2", #the scale
                                                   "contacts_3")])

# Loneliness
combined_data$Loneliness<- rowMeans(combined_data[, c("loneliness_1", "loneliness_2", #the scale
                                                "loneliness_3", "loneliness_4",
                                                "loneliness_5", "loneliness_6",
                                                "loneliness_7", "loneliness_8")])

# Online contacts
combined_data$Online<- rowMeans(combined_data[,c("online_students", "online_friends", #the scale
                                           "online_family")])

# Offline contacts
combined_data$Offline<- rowMeans(combined_data[,c("offline_students", "offline_friends", #the scale
                                           "offline_family")])


# Save the final dataset
write.csv(combined_data, paste0(data_path, "combined_data.csv"))
