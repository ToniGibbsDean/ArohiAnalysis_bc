
# Load required packages
library(dplyr)
library(ggplot2)
library(FSA)
library(rcompanion)
library(MuMIn)
library(tidyverse)
library(GGally)
#
merged_df <- read.csv("Data/hormone_sensitivity_asd_09152025.csv")
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

# Taking out records that do not have a PQB score.
df_gamma <- merged_df[!is.na(merged_df$pqb_symptom) & merged_df$pqb_symptom > 0, ]

df_gamma <- merged_df[!is.na(merged_df$pqb_symptom),] ## 0 anyway

dropped_rows <- merged_df[is.na(merged_df$pqb_symptom) | merged_df$pqb_symptom <= 0, ]
#also 0

print(dropped_rows)

ggplot(df_gamma, aes(x = hormone_sensitivity)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  labs(title = "Distribution of Hormone Sensitivity Scores",
       x = "Hormone Sensitivity Score",
       y = "Frequency") +
  theme_minimal()
# Testing GLM with Gaussian and Gamma distributions
# --- Define your models for both families ---
families <- list(
  gaussian = gaussian(link = "identity"),
  gamma    = Gamma(link = "log")
)


#### create a model df
mod_df<-df_gamma %>%
  select(hormone_sensitivity,
        pqb_symptom,
        age,
        moodDXyesNo,
        birth_control,
        lastperiod_daysSince) %>%
    filter(!is.na(age)) %>%
  #mutate(record_id = as.factor(record_id)) %>% 
  mutate(birth_control = as.factor(birth_control)) %>%
  mutate(moodDXyesNo = as.factor(moodDXyesNo)) 
  
  summary()

  x<-ggpairs(mod_df)

  AgeorLastperiod = pqb_symptom ~ age + lastperiod_daysSince, dat

# Define model formulas
formulas <- list(
  null = pqb_symptom ~ 1,
  noInt = pqb_symptom ~ hormone_sensitivity + age + moodDXyesNo + birth_control + lastperiod_daysSince,
  sensXbc = pqb_symptom ~ hormone_sensitivity*birth_control + age + moodDXyesNo + lastperiod_daysSince,
  sexXmood = pqb_symptom ~ hormone_sensitivity*moodDXyesNo + age + birth_control + lastperiod_daysSince,
 #bcXtype = pqb_symptom ~ hormone_sensitivity + moodDXyesNo + age + birth_control*type + lastperiod_daysSince,
 sensXlastperiod = pqb_symptom ~ hormone_sensitivity*lastperiod_daysSince + moodDXyesNo + age + birth_control

### go through the RHQ drop for bc type - / see if theres a way to
)
summary(null)
summary(noInt)
summary(sensXbc)
summary(sexXmood)
summary(sensXlastperiod)
# --- Fit models and collect results ---
results <- list()

for (fam_name in names(families)) {
  fam <- families[[fam_name]]
  
  for (mod_name in names(formulas)) {
    form <- formulas[[mod_name]]
    
    fit <- glm(formula = form, data = mod_df, family = fam)
    
    # Save model info
    results[[paste(mod_name, fam_name, sep = "_")]] <- list(
      model = fit,
      aicc = AICc(fit)
    )
    
    # --- QQ Plot of residuals ---
    qqnorm(residuals(fit, type = "pearson"), main = paste(mod_name, fam_name))
    qqline(residuals(fit, type = "pearson"))
  }
}

# --- Compile AICc values into a data frame ---
aicc_df <- data.frame(
  Model = names(results),
  AICc  = sapply(results, function(x) x$aicc)
)

print(aicc_df)

# Optional: rank by best model (lowest AICc)
aicc_df <- aicc_df[order(aicc_df$AICc), ]
print(aicc_df)




# Define model families
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
    fit <- glm(formula = form, data = df_gamma, family = fam)
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




x<-lm(pqb_symptom ~ hormone_sensitivity*birth_control + age + moodDXyesNo + lastperiod_daysSince, mod_df)
# # GLM with Gamma distribution, because PQB are skewed.
# model1 <- glm(pqb_symptom ~ 1,
#               data = df_gamma,
#               family = Gamma(link = "log"))
# summary(model1)
# AICc(model1)
# model2 <- glm(pqb_symptom ~ hormone_sensitivity + age  + moodDXyesNo + birth_control + lastperiod_daysSince ,
#                    data = df_gamma,
#                    family = Gamma(link = "log"))
# summary(model2)
# AICc(model2)
# model3 <- glm(pqb_symptom ~ hormone_sensitivity:age  + moodDXyesNo + birth_control + lastperiod_daysSince ,
#               data = df_gamma,
#               family = Gamma(link = "log"))
# summary(model3)
# AICc(model3)
# model4 <- glm(pqb_symptom ~ hormone_sensitivity:age:moodDXyesNo + birth_control + lastperiod_daysSince ,
#               data = df_gamma,
#               family = Gamma(link = "log"))
# summary(model4)
# AICc(model4)
# model5 <- glm(pqb_symptom ~ hormone_sensitivity:age:moodDXyesNo:birth_control + lastperiod_daysSince ,
#               data = df_gamma,
#               family = Gamma(link = "log"))
# summary(model5)
# AICc(model5)
# model6 <- glm(pqb_symptom ~ hormone_sensitivity*age*moodDXyesNo:birth_control:lastperiod_daysSince ,
#               data = df_gamma,
#               family = Gamma(link = "log"))
# summary(model6)
# AICc(model6)
# nrow(df_gamma)

## old old, do not worry
# Linear correlation and plot testing hormone sensitivity predicting PQB score
# cor.test(df_gamma$hormone_sensitivity,
#          df_gamma$pqb_symptom,
#          use = "complete.obs",
#          method = "spearman")
# library(ggplot2)
# 
# ggplot(df_gamma, aes(x = hormone_sensitivity, y = pqb_symptom)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE, color = "blue") +
#   labs(title = "Scatterplot of Hormone Sensitivity vs PQB Symptom")
# 
# # Making hormone_sensitivity a y/n.
# df_gamma$hs_yn <- ifelse(df_gamma$hormone_sensitivity > 0, 1, 0)
# table(df_gamma$hs_yn)
# 
# ggplot(df_gamma, aes(x = hs_yn)) +
#   geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
#   labs(title = "Distribution of HS (y/n) Scores",
#        x = "Hormone Sensitivity Score",
#        y = "Frequency") +
#   theme_minimal()
# 
# library(ggplot2)
# 
# ggplot(df_gamma, aes(x = factor(hs_yn), y = pqb_symptom)) +
#   geom_boxplot(fill = "skyblue", color = "darkblue") +
#   labs(x = "Hormone Sensitivity (hs_yn)",
#        y = "PQB Symptom",
#        title = "Boxplot of PQB Symptom by Hormone Sensitivity Group") +
#   theme_minimal()
# write.csv(df_gamma, "/Users/arohidandawate/PowersLab/Analysis/PREMAP/HRT_PQB/hormone_sensitivity_08252025.csv", row.names = FALSE)
