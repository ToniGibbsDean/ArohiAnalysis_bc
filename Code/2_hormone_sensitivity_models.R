
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
  filter(ever_pregnant == 1) %>%
  filter(!is.na(bc_exposure)) %>%
  filter(cycle_regular_lifetime != 0)

#DATA FRAME should read from the data frame PRODUCED FROM "2_HORMONESENSITIVITY_CALCULATION_DATAFRAME"
nrow(merged_df)
print(paste("Number of rows:", nrow(merged_df)))
merged_df %>%
  summarise(
    across(c(cycle_regular_now, symptoms_bc, prenatal_depression, postpartum_depression, postpartum_psychosis),
           ~ paste(unique(.), collapse = ", "))
  )
cor_matrix <- cor(
  merged_df[, c("hormone_sensitivity", "cycle_regular_now", "symptoms_bc", "prenatal_depression", "postpartum_depression", "postpartum_psychosis" )],
  use = "pairwise.complete.obs")

print(cor_matrix)

# Correlation matrix heatmap
cor_df <- as.data.frame(as.table(cor_matrix))
colnames(cor_df) <- c("Var1", "Var2", "value")

# --- Heatmap ---
p <- ggplot(cor_df, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab",
                       name="Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_fixed()
ggsave("heatmap_horm_sens.png", plot = p, width = 6, height = 4)

## Checking to see if there are any NAs, there are not.
# mod_df %>%
#   summarise(
#     hormone_sensitivity_NA = sum(is.na(merged_df$hormone_sensitivity)),
#     bc_type_NA            = sum(is.na(merged_df$bc_type)),
#     bc_exposure_NA = sum(is.na(merged_df$bc_exposure))
#   )
# summary(mod_df$hormone_sensitivity_NA)
table(df$hormone_sensitivity)
# Testing distribution of both hormone_sensitivity and PQB scores
p <- ggplot(merged_df, aes(x = hormone_sensitivity)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  labs(title = "Distribution of Hormone Sensitivity Scores",
       x = "Hormone Sensitivity Score",
       y = "Frequency") +
  theme_minimal()
ggsave("dist_horm_sens.png", plot = p, width = 6, height = 4)

p <- ggplot(merged_df, aes(x = pqb_symptom)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  labs(title = "Distribution of PQB Scores",
       x = "PQB Score",
       y = "Frequency") +
  theme_minimal()
ggsave("dist_pqb_symp.png", plot = p, width = 6, height = 4)
p <- ggplot(merged_df, aes(x = bc_exposure)) +
  geom_histogram(binwidth = 0.05, fill = "skyblue", color = "white")+  # let ggplot choose bin width
  labs(title = "Distribution of BC Exposure",
       x = "BC Exposure",
       y = "Frequency") +
  theme_minimal()

ggsave("dist_bc_exp.png", plot = p, width = 6, height = 4)
summary(merged_df$bc_exposure)
hist(merged_df$bc_exposure)


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
        bc_lifetime,
        bc_exposure,
        lastperiod_daysSince) %>%
    filter(!is.na(age)) %>%
  mutate(record_id = as.factor(record_id)) %>% 
  mutate(bc_lifetime = as.factor(bc_lifetime)) %>%
  mutate(moodDXyesNo = as.factor(moodDXyesNo)) 

mod_df %>%
  summarise(
    hormone_sensitivity_NA = sum(is.na(hormone_sensitivity)),
    bc_lifetime_NA            = sum(is.na(bc_lifetime)),
    bc_exposure_NA = sum(is.na(bc_exposure))
  )

x <- ggpairs(mod_df %>% select(-record_id))

   ### issue - 2 record IDs have 2 rows each
   records <-list(569,1174)
   mod_df  %>% filter(record_id %in% records)

###### create object that has all models
formulas <- list(
  null = pqb_symptom ~ 1,
  noInt = pqb_symptom ~ hormone_sensitivity + age + moodDXyesNo + bc_lifetime + lastperiod_daysSince,
  age = pqb_symptom ~ hormone_sensitivity + age*lastperiod_daysSince + moodDXyesNo + bc_lifetime,
  sensXbc = pqb_symptom ~ hormone_sensitivity*bc_lifetime + age + moodDXyesNo + lastperiod_daysSince,
  sexXmood = pqb_symptom ~ hormone_sensitivity*moodDXyesNo + age + bc_lifetime + lastperiod_daysSince,
 #bcXtype = pqb_symptom ~ hormone_sensitivity + moodDXyesNo + age + birth_control*type + lastperiod_daysSince,
 sensXlastperiod = pqb_symptom ~ hormone_sensitivity*lastperiod_daysSince + moodDXyesNo + age + bc_lifetime,
 bc_and_hs = pqb_symptom ~ hormone_sensitivity+bc_exposure,
 bc_int_hs = pqb_symptom ~ hormone_sensitivity*bc_exposure,
 hs_mooddx = pqb_symptom ~ hormone_sensitivity+moodDXyesNo,
 #concern_hs_mood = pqb_concern ~ hormone_sensitivity+bc_exposure+moodDXyesNo,
 bc_and_exp_moodDx = pqb_symptom ~ hormone_sensitivity*moodDXyesNo+bc_exposure)
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
  
library("ggpubr")
ggscatter(summarydf, x = "hormone_sensitivity", y = "pqb_symptom", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Horm Sens", ylab = "PQB Symptom")

res <- cor.test(summarydf$pqb_symptom, summarydf$hormone_sensitivity, method = "spearman")
res