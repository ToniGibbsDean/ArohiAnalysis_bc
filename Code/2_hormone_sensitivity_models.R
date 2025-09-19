
# Load required packages
library(dplyr)
library(FSA)
library(rcompanion)
library(MuMIn)
library(tidyverse)
library(GGally)
#
df <- read.csv("Outputs/summarydf.csv")
merged_df <- df  %>%
  filter(ever_pregnant == 1)
#DATA FRAME should read from the data frame PRODUCED FROM "2_HORMONESENSITIVITY_CALCULATION_DATAFRAME"
nrow(merged_df)

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
        bc_type,
        bc_exposure,
        lastperiod_daysSince) %>%
    filter(!is.na(age)) %>%
  mutate(record_id = as.factor(record_id)) %>% 
  mutate(bc_type = as.factor(bc_type)) %>%
  mutate(moodDXyesNo = as.factor(moodDXyesNo)) 

mod_df %>%
  summarise(
    hormone_sensitivity_NA = sum(is.na(hormone_sensitivity)),
    bc_type_NA            = sum(is.na(bc_type)),
    bc_exposure_NA = sum(is.na(bc_exposure))
  )

x <- ggpairs(mod_df %>% select(-record_id))

   ### issue - 2 record IDs have 2 rows each
   records<-list(569,1174)
   mod_df  %>% filter(record_id %in% records)

###### create object that has all models
formulas <- list(
  null = pqb_symptom ~ 1,
  noInt = pqb_symptom ~ hormone_sensitivity + age + moodDXyesNo + bc_type + lastperiod_daysSince,
  age = pqb_symptom ~ hormone_sensitivity + age*lastperiod_daysSince + moodDXyesNo + bc_type,
  sensXbc = pqb_symptom ~ hormone_sensitivity*bc_type + age + moodDXyesNo + lastperiod_daysSince,
  sexXmood = pqb_symptom ~ hormone_sensitivity*moodDXyesNo + age + bc_type + lastperiod_daysSince,
 #bcXtype = pqb_symptom ~ hormone_sensitivity + moodDXyesNo + age + birth_control*type + lastperiod_daysSince,
 sensXlastperiod = pqb_symptom ~ hormone_sensitivity*lastperiod_daysSince + moodDXyesNo + age + bc_type,
 bc_and_exp = pqb_symptom ~ hormone_sensitivity*bc_exposure + bc_type)

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
  
  