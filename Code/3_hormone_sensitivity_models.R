
# Load required packages
library(dplyr)
library(FSA)
library(rcompanion)
library(MuMIn)
library(tidyverse)
library(GGally)
#
merged_df <- read.csv("Data/hormone_sensitivity_asd_09152025.csv")

#DATA FRAME should read from the data frame PRODUCED FROM "2_HORMONESENSITIVITY_CALCULATION_DATAFRAME"
#nrow(merged_df)

# Testing distribution of both hormone_sensitivity and PQB scores
ggplot(merged_df, aes(x = hormone_sensitivity)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  labs(title = "Distribution of Hormone Sensitivity Scores",
       x = "Hormone Sensitivity Score",
       y = "Frequency") +
  theme_minimal()

ggplot(merged_df, aes(x = pqb_symptom)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  labs(title = "Distribution of PQB Scores",
       x = "PQB Score",
       y = "Frequency") +
  theme_minimal()


######### NOT SURE YOU NEED THIS BIT / CLEANING COULD GO IN CLEANING SCRIPT

# # Taking out records that do not have a PQB score.
 df_gamma <- merged_df[!is.na(merged_df$pqb_symptom) & merged_df$pqb_symptom > 0, ]

# df_gamma <- merged_df[!is.na(merged_df$pqb_symptom),] ## 0 anyway

# dropped_rows <- merged_df[is.na(merged_df$pqb_symptom) | merged_df$pqb_symptom <= 0, ]
# #also 0

# print(dropped_rows)

# ggplot(df_gamma, aes(x = hormone_sensitivity)) +
# geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
# labs(title = "Distribution of Hormone Sensitivity Scores",
#       x = "Hormone Sensitivity Score",
#       y = "Frequency") +
# theme_minimal()

#### create a model df
mod_df<-df_gamma %>%
  select(record_id,
        hormone_sensitivity,
        pqb_symptom,
        age,
        moodDXyesNo,
        birth_control,
        lastperiod_daysSince) %>%
    filter(!is.na(age)) %>%
  mutate(record_id = as.factor(record_id)) %>% 
  mutate(birth_control = as.factor(birth_control)) %>%
  mutate(moodDXyesNo = as.factor(moodDXyesNo)) 

   x<-ggpairs(mod_df) #remove IDs to make work

   ### issue - 2 record IDs have 2 rows each
   records<-list(569,1174)
   mod_df  %>% filter(record_id %in% records)

###### create object that has all models
formulas <- list(
  null = pqb_symptom ~ 1,
  noInt = pqb_symptom ~ hormone_sensitivity + age + moodDXyesNo + birth_control + lastperiod_daysSince,
  age = pqb_symptom ~ hormone_sensitivity + age*lastperiod_daysSince + moodDXyesNo + birth_control,
  sensXbc = pqb_symptom ~ hormone_sensitivity*birth_control + age + moodDXyesNo + lastperiod_daysSince,
  sexXmood = pqb_symptom ~ hormone_sensitivity*moodDXyesNo + age + birth_control + lastperiod_daysSince,
 #bcXtype = pqb_symptom ~ hormone_sensitivity + moodDXyesNo + age + birth_control*type + lastperiod_daysSince,
 sensXlastperiod = pqb_symptom ~ hormone_sensitivity*lastperiod_daysSince + moodDXyesNo + age + birth_control)

# Testing GLM with Gaussian and Gamma distributions
# --- Define your models for both families ---
families <- list(
  gaussian = gaussian(link = "identity"),
  gamma    = Gamma(link = "log")
)

# Store fitted models + summaries
results <- list()
for (fam_name in names(families)) {
  fam <- families[[fam_name]]
  for (mod_name in names(formulas)) {
    form <- formulas[[mod_name]]
    # Fit the model
    fit <- glm(formula = form, data = mod_df, family = fam)
    # Save results
    results[[paste(mod_name, fam_name, sep = "_")]] <- list(
      model = fit,
      summary = summary(fit),
      aicc = AICc(fit)
    )
    # Optional: print summary
    cat("\n---", mod_name, fam_name, "---\n")
    print(summary(fit))
  }
}
# --- Compile AICc values into a data frame ---
aicc_df <- data.frame(
  Model = names(results),
  AICc  = sapply(results, function(x) x$aicc)
)
print(aicc_df)
  
  