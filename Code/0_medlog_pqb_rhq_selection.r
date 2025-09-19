library(dplyr)


df <- read.csv("Data/PREMAPIntegration-Medlogpqbbc_DATA_2025-09-19_1420.csv") %>%
  as_tibble()

# Block 1: your hand-written list
cols_repro <- c(
  "record_id", "prescreendate", "email", "dob", "rhq_date", "residence_state", "residence_zipcode", "lastperiod",
  "repro1","repro2","repro3","repro6","repro4","repro5","repro7","repro8","repro9","repro10",
"repro11","repro12","repro13","repro14","repro15","repro16","repro17","repro18","repro19","repro20",
"repro21___1","repro21___2","repro21___3","repro21___4","repro21___5","repro21___6","repro21___7","repro21___8","repro21___9","repro21___10",
"repro21___11","repro21___12","repro21___13","repro21___14","repro21___15","repro21___16","repro21___17","repro21___18","repro21___19","repro21___20", # nolint
"repro21___21","repro21___22","repro21___23","repro21___24","repro21notes","repro22","repro23","repro24","repro25","repro26",
"repro27","repro28","repro29","repro30","repro31","repro32","repro33","repro34","repro35","repro36",
"repro37","repro38","repro39","repro40","repro42","repro43","repro44","repro45","repro46","repro47",
"repro48","repro49","repro50","repro51","repro52","repro53","repro54","repro55___5","repro55___1","repro55___2",
"repro55___3","repro55___4","repro55___9","repro56","repro57","repro58","repro59","repro60","repro61","repro62",
"repro63","repro64","repro65","repro66","repro67","repro68","repro69","repro70","repro71","repro72",
"repro73___1","repro73___2","repro73___8","repro73___9","repro74","repro75","repro76___1","repro76___2","repro76___3","repro76___9",
"repro77","repro78","repro79","repro80","repro81","repro82","repro83___1","repro83___2","repro83___3","repro83___4",
"repro83___5","repro83___6","repro83___7","repro84","repro85","repro86","repro87","repro88","repro89___1","repro93",
"repro90","repro91","repro92","repro94","repro95","reproductive_history_questionnaire_complete"
)
df <- df %>%
  mutate(pqb_symptom = 
  rowSums(
      select(., pqb1:pqb21),  # select the columns to sum
      na.rm = TRUE  # ignores NAs so missing values don’t break the sum
    )
  ) %>%
  mutate(pqb_concern = 
  rowSums(
    select(., pqb1_yes:pqb21_yes),
    na.rm = TRUE
  ))

df %>%
  select(pqb_symptom, pqb_concern) %>%
  print(n = 10)


# Block 2: meds
cols_med <- unlist(lapply(1:25, function(i) {
  c(
    paste0("medname", i),
    paste0("medtype", i),
    paste0("medstart", i),
    paste0("medcurrent", i, "___1"),  # only take ___1 version
    paste0("medend", i)
  )
}))

# Block 3: mood dx
cols_mood <- paste0("mooddx", 1:6)
cols_pqb <- c("pqb_symptom", "pqb_concern")

all_cols <- c(cols_repro, cols_med, cols_mood, cols_pqb)
df_subset <- df[, all_of(all_cols)]

repro_name_map <- c(
  repro1 = "first_period",
  repro2 = "cycle_regular_lifetime",
  repro3 = "currently_menstruating",
  repro4 = "period_last_month",
  repro5 = "period_last_year",
  repro7 = "cycle_regular_now",
  repro10 = "irregular_duration",
  repro11 = "irregular_bleed_length_change",
  repro15 = "amenorrhea_any",
  repro16 = "amenorrhea_duration",
  repro18 = "menopause_symptoms_ever",
  repro19 = "menopause_symptoms_start_age",
  repro20 = "menopause_symptoms_end_age",
  repro30 = "ever_pregnant",
  repro31 = "pregnancy_count",
  repro32 = "pregnancy_6months_ever",
  repro33 = "pregnancy_6months_count",
  repro36 = "live_birth_count",
  repro47 = "prenatal_depression",
  repro48 = "postpartum_depression",
  repro49 = "postpartum_psychosis",
  repro57 = "breastfed_any",
  repro58 = "breastfed_child_count",
  repro61 = "breastfeeding_total_months",
  repro62 = "ovary_removal",
  repro63 = "ovary_removal_age",
  repro64 = "tubal_ligation",
  repro65 = "tubal_ligation_age",
  repro78 = "breast_cancer_dx",
  repro79 = "ovarian_uterine_cancer_dx",
  repro80 = "birthcontrol_current",
  repro81 = "birthcontrol_current_type",
  repro82 = "birthcontrol_ever",
  repro83___1 = "bc_pill_ever",
  repro83___2 = "bc_patch_ever",
  repro83___3 = "bc_ring_ever",
  repro83___4 = "bc_implant_ever",
  repro83___5 = "bc_injection_ever",
  repro83___6 = "bc_iud_ever",
  repro83___7 = "bc_other_ever",
  repro84 = "hormone_med_current",
  repro85 = "hormone_med_current_type",
  repro86 = "hormone_med_ever",
  repro87 = "hormone_med_ever_details",
  repro88 = "symptoms_bc",
  pqb_symptom = "pqb_symptom",
  pqb_concern = "pqb_concern"
)

# Use rename_with to match and replace based on the mapping
new_df_renamed <- df_subset %>%
  rename_with(.fn = ~ repro_name_map[.x], .cols = names(repro_name_map)[names(repro_name_map) %in% names(.)])

exclude_cols <- c("prescreendate", "email")

# Filter out rows where all other columns are NA, 0, or blank
cleaned_df <- new_df_renamed %>%
  filter(
    rowSums(
      across(
        .cols = -any_of(exclude_cols),
        .fns = ~ !is.na(.) & . != 0 & . != ""
      )
    ) > 0
  )

write.csv(cleaned_df, "Outputs/RHQ_PSCREEN_MEDLOGdfELIGIBLEONLY.csv")
saveRDS(cleaned_df, "Outputs/RHQ_PSCREEN_MEDLOGdfELIGIBLEONLY.rds")