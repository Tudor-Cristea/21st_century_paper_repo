## Calculate the numbers in section 3.2 (correlations between the scales)

library(dplyr)
library(Hmisc)

# Prepare the datasets for correlation analysis per quartile
q4_data<- read.csv(paste0(data_path, "final_q4.csv")) %>%
  select(Skills, Communication, Online, Offline, Loneliness)

q1_data<- read.csv(paste0(data_path, "final_q1.csv")) %>%
  filter(quartile== "Q1") %>%
  select(Skills, Communication, Online, Offline, Loneliness)


## Calculate correlation matrix and p-values
q4_cors <- rcorr(as.matrix(q4_data))
q1_cors <- rcorr(as.matrix(q1_data))

# Extract correlations and p-values for Q4 
  #Q4 (Table 2, page 5)
q4_cors$r
q4_cors$P
 
  #Q1(Table 3, page 6)
q1_cors$r
q1_cors$P

