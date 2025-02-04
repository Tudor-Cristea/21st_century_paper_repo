## Fit all the path models found in section 3.3
library(lavaan)
library(dplyr) 

# Load the datasets per quartile
q4_data<- read.csv(paste0(data_path, "final_q4.csv")) %>%
  select(Skills, Communication, Online, Offline, Loneliness, online_students,
         online_friends, online_family, offline_students, offline_friends, 
         offline_family)

q1_data<- read.csv(paste0(data_path, "final_q1.csv")) %>%
  select(Skills, Communication, Online, Offline, Loneliness, online_students,
         online_friends, online_family, offline_students, offline_friends, 
         offline_family)

# Define the proposed model
model <- ' 
  # Regressions
  Communication ~ Skills
  Online ~ Communication + Skills
  Offline ~ Communication + Skills
  Loneliness ~ Skills + Online + Offline + Communication'

# Fit the model for Q4 and Q1
fit_q4 <- sem(model, q4_data, std.lv = TRUE)
fit_q1 <- sem(model, q1_data, std.lv = TRUE)

# Summarize results
summary(fit_q4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(fit_q1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

# Model fit indices
fitMeasures(fit_q4, c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"))
fitMeasures(fit_q1, c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr"))


## Final models for Q4 and Q1
final_model_q4 <- ' 
  # Regressions
  Communication ~ Skills
  Online ~ Communication + Skills
  Loneliness ~ Skills'

final_model_q1 <- ' 
  # Regressions
  Communication ~ Skills
  Online ~ Communication + Skills
  Offline ~ Skills
  Loneliness ~ Skills + Offline
  # Covariances
  Online ~~ Offline'

# Fit the final models
final_fit_q4 <- sem(final_model_q4, q4_data, std.lv = TRUE)
final_fit_q1 <- sem(final_model_q1, q1_data, std.lv = TRUE)

# Summarize results
summary(final_fit_q4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(final_fit_q1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


## Independent Item models

independent_model_q4<- ' 
# Regressions
  Communication ~ Skills
online_students ~ Communication + Skills
online_friends ~ Communication + Skills
online_family ~ Communication
offline_family ~ Communication
Loneliness ~ Skills + offline_friends'

independent_model_q1<- ' 
# Regressions
  Communication ~ Skills
online_students ~ Communication + Skills
online_friends ~ Communication + Skills
online_family ~ Communication
offline_students ~ Communication
offline_friends ~ Communication + Skills
offline_family ~ Communication + Skills
Loneliness ~ Skills + online_students + offline_students + offline_friends'

# Fit the final independent items models
final_item_q4 <- sem(independent_model_q4, q4_data, std.lv = TRUE)
final_item_q1 <- sem(independent_model_q1, q1_data, std.lv = TRUE)

# Summarize results
summary(final_item_q4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(final_item_q1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)


## Simple models
simple_model_q4 <- ' 
  # Regressions
  Communication ~ Skills
  Loneliness ~ Skills'

simple_model_q1 <- ' 
  # Regressions
  Communication ~ Skills
  Loneliness ~ Skills + Communication'

# Fit the simple models
simple_fit_q4 <- sem(simple_model_q4, q4_data, std.lv = TRUE)
simple_fit_q1 <- sem(simple_model_q1, q1_data, std.lv = TRUE)

# Summarize results
summary(simple_fit_q4, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(simple_fit_q1, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)



