
# Load required packages
library(dplyr)
library(ggplot2)
library(FSA)
library(rcompanion)
library(MuMIn)

# # Step 1: Load the data
# # File paths
# file1 <- "/Users/arohidandawate/PowersLab/Analysis/PREMAP/HRT_PQB/hormone_sensitivity_08252025.csv"
# file2 <- "/Users/arohidandawate/PowersLab/Analysis/PREMAP/HRT_PQB/PREMAPIntegration-ArohiTest_DATA_2025-08-21_1638.csv"
# file3 <- "/Users/arohidandawate/Downloads/PREMAPIntegration-EmailPregnants_DATA_2025-09-12_1445.csv"
# 
# # Read the CSV files
# df1 <- read_csv(file1)
# df2 <- read_csv(file2)
# df3 <- read_csv(file3)
# 
# # Select relevant columns
# df1_selected <- df1 %>%
#   select(record_id, hormone_sensitivity, repro80, repro81, pqb_symptom, pqb_concern)
# 
# df2_selected <- df2 %>%
#   select(record_id, email)
# 
# summarydf_RHQ_selected <- summarydf_RHQ %>%
#   select(email, age, PCOS, moodDXyesNo, lastperiod_daysSince, birth_control)
# 
# df3_selected <- df3 %>%
#   select(email, repro31, repro33)
# # Convert MoodDXYesNo to an integer
# summarydf_RHQ_selected$moodDXyesNo=as.factor(summarydf_RHQ_selected$moodDXyesNo)
# # Convert birth_control to an integer
# summarydf_RHQ_selected$birth_control=as.factor(summarydf_RHQ_selected$birth_control)
# 
# 
# # Convert record_id to integer
# df1_selected <- df1_selected %>%
#   mutate(record_id = as.integer(record_id))
# 
# df2_selected <- df2_selected %>%
#   mutate(record_id = as.integer(record_id))
# 
# 
# 
# # Now perform the left joins
# merge <- df1_selected %>%
#   left_join(df2_selected, by = "record_id")
# 
# print(merge)
# merge_preg <- left_join(merge, df3_selected, by = "email" )
# merge_RHQ <- left_join(merge_preg, summarydf_RHQ_selected, by = "email")
# merged_df <- merge_RHQ[merge_RHQ$repro31 != 0, ]
# merged_df <- merged_df[!is.na(merged_df$repro31), ]
# print(merged_df)
# write.csv(merged_df, "/Users/arohidandawate/PowersLab/Analysis/PREMAP/HRT_PQB/hormone_sensitivity_asd_09152025.csv", row.names = FALSE)
merged_df <- read_csv("/Users/arohidandawate/PowersLab/Analysis/PREMAP/HRT_PQB/hormone_sensitivity_asd_09152025.csv")
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
dropped_rows <- merged_df[is.na(merged_df$pqb_symptom) | merged_df$pqb_symptom <= 0, ]
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

# Define model formulas
formulas <- list(
  model1 = pqb_symptom ~ 1,
  model2 = pqb_symptom ~ hormone_sensitivity + age + moodDXyesNo + birth_control + lastperiod_daysSince,
  model3 = pqb_symptom ~ hormone_sensitivity:age + moodDXyesNo + birth_control + lastperiod_daysSince,
  model4 = pqb_symptom ~ hormone_sensitivity:age:moodDXyesNo + birth_control + lastperiod_daysSince,
  model5 = pqb_symptom ~ hormone_sensitivity:age:moodDXyesNo:birth_control + lastperiod_daysSince,
  model6 = pqb_symptom ~ hormone_sensitivity:age:moodDXyesNo:birth_control:lastperiod_daysSince
)
summary(model1)
summary(model2)
summary(model3)
summary(model4)
summary(model5)
summary(model6)
# --- Fit models and collect results ---
results <- list()

for (fam_name in names(families)) {
  fam <- families[[fam_name]]
  
  for (mod_name in names(formulas)) {
    form <- formulas[[mod_name]]
    
    fit <- glm(formula = form, data = df_gamma, family = fam)
    
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
