library(tidyverse)

df <- read_csv("Outputs/joinedCorrected_RHQ_dfELIGIBLEONLY.csv")


# Convert and clean variables

# Batch 1
df_clean <- df %>%
  mutate(
    first_period = as.numeric(str_extract(first_period, "\\d+(\\.\\d+)?")),
    cycle_regular_lifetime = recode_factor(as.character(cycle_regular_lifetime), `0` = "irregular", `1` = "regular", `2` = "both"),
    currently_menstruating = recode_factor(as.character(currently_menstruating), `0` = "no", `1` = "yes", `2` = NA_character_),
    period_last_month = recode_factor(as.character(period_last_month), `0` = "no", `1` = "yes", .default = NA_character_),
    period_last_year = recode_factor(as.character(period_last_year), `0` = "no", `1` = "yes", .default = NA_character_)
  ) %>%

  # Batch 2
  mutate(
    cycle_regular_now = recode_factor(as.character(cycle_regular_now), `0` = "irregular", `1` = "regular", `2` = "both"),
    irregular_duration = as.numeric(irregular_duration),
    irregular_bleed_length_change = recode_factor(as.character(irregular_bleed_length_change), `0` = "no", `1` = "yes", `2` = NA_character_),
    amenorrhea_any = recode_factor(as.character(amenorrhea_any), `0` = "no", `1` = "yes", `2` = NA_character_),
    amenorrhea_duration = as.numeric(amenorrhea_duration)
  ) %>%

  # Batch 3
  mutate(
    menopause_symptoms_ever = recode_factor(as.character(menopause_symptoms_ever), `0` = "no", `1` = "yes", `2` = NA_character_),
    #menopause_symptoms_start_age = as.numeric(menopause_symptoms_start_age),
    #menopause_symptoms_end_age = as.numeric(menopause_symptoms_end_age),
    ever_pregnant = recode_factor(as.character(ever_pregnant), `0` = "no", `1` = "yes", `2` = NA_character_),
    pregnancy_count = as.numeric(pregnancy_count),
    pregnancy_6months_ever = recode_factor(as.character(pregnancy_6months_ever), `0` = "no", `1` = "yes", `2` = NA_character_),
    live_birth_count = as.numeric(live_birth_count)
  ) %>%

  # Batch 4
  mutate(
    prenatal_depression = recode_factor(as.character(prenatal_depression), `0` = "no", `1` = "yes", `2` = NA_character_),
    postpartum_depression = recode_factor(as.character(postpartum_depression), `0` = "no", `1` = "yes", `2` = NA_character_),
    postpartum_psychosis = recode_factor(as.character(postpartum_psychosis), `0` = "no", `1` = "yes", `2` = NA_character_),
    symptoms_bc = recode_factor(as.character(symptoms_bc), '0' = "no", '1' = "yes", '2' = NA_character_),
  ) %>%
    
  # Batch 5
  mutate(
    breastfed_any = recode_factor(as.character(breastfed_any), `0` = "no", `1` = "yes", `2` = NA_character_),
    breastfed_child_count = as.numeric(breastfed_child_count),
    breastfeeding_total_months = as.numeric(breastfeeding_total_months)
  ) %>%

  # Batch 5
  mutate(
    ovary_removal = recode_factor(as.character(ovary_removal), `0` = "no", `1` = "yes", `2` = NA_character_),
    ovary_removal_age = as.numeric(ovary_removal_age),
    tubal_ligation = recode_factor(as.character(tubal_ligation), `0` = "no", `1` = "yes", `2` = NA_character_),
    tubal_ligation_age = as.numeric(tubal_ligation_age),
    breast_cancer_dx = recode_factor(as.character(breast_cancer_dx), `0` = "no", `1` = "yes", `2` = NA_character_)
  ) %>%
  
  # Batch 6 - Birth Control Methods and Pregnancy Detail
mutate(
  pregnancy_6months_count = as.numeric(pregnancy_6months_count),
  bc_pill_ever = as.numeric(bc_pill_ever),
  bc_patch_ever = as.numeric(bc_patch_ever),
  bc_ring_ever = as.numeric(bc_ring_ever),
  bc_implant_ever = as.numeric(bc_implant_ever)
) %>%

mutate(
    # Final batch
    ovarian_uterine_cancer_dx = recode_factor(as.character(ovarian_uterine_cancer_dx), `0` = "no", `1` = "yes", `2` = NA_character_),
    birthcontrol_current = recode_factor(as.character(birthcontrol_current), `0` = "no", `1` = "yes", `2` = NA_character_),
    birthcontrol_ever = recode_factor(as.character(birthcontrol_ever), `0` = "no", `1` = "yes", `2` = NA_character_),
    birthcontrol_current_type = recode_factor(as.character(birthcontrol_current_type), `0` = "no", `1` = "yes", `2` = NA_character_),
    hormone_med_current = recode_factor(as.character(hormone_med_current), `0` = "no", `1` = "yes", `2` = NA_character_)
  ) %>%

mutate(
    bc_injection_ever = as.numeric(bc_injection_ever),
    bc_iud_ever = as.numeric(bc_iud_ever),
    bc_other_ever = as.numeric(bc_other_ever),
    hormone_med_current_type = as.character(hormone_med_current_type),
    hormone_med_ever = recode_factor(as.character(hormone_med_ever), `0` = "no", `1` = "yes", `2` = NA_character_),
    hormone_med_ever_details = as.character(hormone_med_ever_details),
    DataCollection = as.character(DataCollection)
  )