library(tidyverse)
df_clean <- df_clean %>%
  mutate(
    hormone_sensitivity = 0,  # initialize
    hormone_sensitivity = hormone_sensitivity +
      ifelse(!is.na(cycle_regular_now) & cycle_regular_now == "yes", 1, 0) +
      ifelse(!is.na(symptoms_bc) & symptoms_bc == "yes", 2, 0) +
      ifelse(!is.na(prenatal_depression) & prenatal_depression == "yes", 2, 0) +
      ifelse(!is.na(postpartum_depression) & postpartum_depression == "yes", 2, 0) +
      ifelse(!is.na(postpartum_psychosis) & postpartum_psychosis == "yes", 3, 0)
  ) %>%
  mutate(
    bc_type = case_when(
      bc_pill_ever == 1 | bc_patch_ever == 1 | bc_ring_ever == 1 ~ "P/E",
      bc_implant_ever == 1 | bc_injection_ever == 1 | bc_iud_ever == 1 ~ "P",
      bc_other_ever == 1 ~ "other",
      TRUE ~ "none"
    )
  )


write.csv(cleaned_df, "Outputs/cleanedRHQ_hormsens_bctype_asd96_091825.csv")
saveRDS(cleaned_df, "Outputs/cleanedRHQ_hormsens_bctype_asd96_091825.rds")

