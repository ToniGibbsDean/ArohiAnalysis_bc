# Script to connect the two datasets

library(tidyverse)


# 1)  initial data loading ---------------------------------------------------------------
#new data 
# load and remove records without names or times as failed data collections
n_post<-read.csv("Data/PREMAPIntegration-RHQPQB_DATA_2025-09-18_1313.csv") %>%  # nolint
    as_tibble #%>% 
   # filter(timestamp!="" & lastname!="") 
# old data
# remove those without a name, all lack other data and are marked as tests or failed data entries
o_post<-read.csv("Data/OLDPREMAPPreScreenin-RHQPQB_DATA_2025-09-18_1341.csv") %>%  # nolint
    as_tibble %>%
    filter(pscreen_name!="")

# 1.5) calculate pqb_symptom and pqb_concern for old data
o_post <- o_post %>%
  mutate(pqb_symptom = 
  rowSums(
      select(., pscreen_pqb1:pscreen_pqb21),  # select the columns to sum
      na.rm = TRUE  # ignores NAs so missing values don’t break the sum
    )
  ) %>%
  mutate(pqb_concern = 
  rowSums(
    select(., pscreen_pqb1_yes:pscreen_pqb21_yes),
    na.rm = TRUE
  ))

o_post %>%
  select(pqb_symptom, pqb_concern) %>%
  print(n = 10)
# 2) remove incorrect old data from new data frae ---------------------------------------------
# filter out everthing from before the new dataset - these were merged in an error-prone way
# this also removes 4 rows that do not have prescreendate values, but all have timestamp values 
#showing they are from the old dataset
n<-n_post %>%
    mutate(prescreendate=as_date(prescreendate))%>%  # nolint
    filter(!(prescreendate < "2024-11-12") |is.na(prescreendate)) %>%
    arrange(prescreendate)


# 3) remove multiple record collection events per person ------------------------------------------------------
# remove multiple records fro mthe sam person (using emails as they must be unique)
# always favoring the last record on assumption that multiple collection efforts reflect
# the need to repeat

# remove second occurances on people in new data collection effort
n<-n[!duplicated(n$email ),]

#remove second occurance of people in old data collection effort
o<-o_post[  !(duplicated(o_post$pscreen_email) & o_post$pscreen_email!="") ,]

#remove people from old data collection effort who subsequnelt came and had data collected in the new effort
o<-o[ ! o$pscreen_email %in% 
    n$email,]


# 4) remove anyone from @yale.edu domain; these are tests ------------------------------------------------------
n<-n[!str_detect(n$email, "@yale.edu"),]
o<-o[!str_detect(o$pscreen_email, "@yale.edu"),]

# 5) align columns between old and new dataframes --------------------------------------------------------------------
# Vector of columns to retain in new dataset
# Vector of columns to retain in new dataset
cols <- c(
  "prescreendate", 
  "email", 
  "repro1",
  "repro2",
  "repro3",
  "repro4",
  "repro5",
  "repro7",
  # "repro9",
  "repro10",
  "repro11",
  # "repro12",
  "repro15",
  "repro16",
  # "repro17",
  "repro18",
  "repro19",
  "repro20",
  # "repro23",
  # "repro22",
  # "repro25",
  # "repro26",
  # "repro27",
  # "repro28",
  # "repro29",
  "repro30",
  "repro31",
  "repro32",
  "repro33",
  # "repro34",
  # "repro35",
  "repro36",
  # "repro37",
  # "repro38",
  # "repro39",
  # "repro40",
  # "repro42",
  # "repro43",
  # "repro44",
  # "repro45",
  # "repro46",
  "repro47",
  "repro48",
  "repro49",
  # "repro50",
  # "repro51",
  # "repro52",
  # "repro53",
  # "repro54",
  # "repro55___1",
  # "repro55___2",
  # "repro55___3",
  # "repro55___4",
  # "repro55___5",
  # "repro55___9",
  # "repro56",
  "repro57",
  "repro58",
  # "repro59",
  # "repro60",
  "repro61",
  "repro62",
  "repro63",
  "repro64",
  "repro65",
  # "repro66",
  # "repro67",
  # "repro68",
  # "repro69",
  # "repro70",
  # "repro71",
  # "repro72",
  # "repro73___1",
  # "repro73___2",
  # "repro73___8",
  # "repro73___9",
  # "repro74",
  # "repro75",
  # "repro76___1",
  # "repro76___2",
  # "repro76___3",
  # "repro76___9",
  # "repro77",
  "repro78",
  "repro79",
  "repro80",
  "repro82",
  "repro81",
  "repro83___1",
  "repro83___2",
  "repro83___3",
  "repro83___4",
  "repro83___5",
  "repro83___6",
  "repro83___7",
  "repro84",
  "repro85",
  "repro86",
  "repro87",
  "repro88",
  "pqb_symptom",
  "pqb_concern"
  # "repro88",
  # "repro89___1",
  # "repro90",
  # "repro91",
  # "repro92",
  # "repro93",
  # "repro94",
  # "repro95",
  # "repro6",
  # "repro8",
  # "repro13",
  # "repro14",
  # "repro21___1", "repro21___2", "repro21___3", "repro21___4", "repro21___5",
  # "repro21___6", "repro21___7", "repro21___8", "repro21___9", "repro21___10",
  # "repro21___11", "repro21___12", "repro21___13", "repro21___14", "repro21___15",
  # "repro21___16", "repro21___17", "repro21___18", "repro21___19", "repro21___20",
  # "repro21___21", "repro21___22", "repro21___23", "repro21___24",
  # "repro21notes",
  # "repro24"
)

n_selected <- n %>%
  #mutate(name=paste0(firstname, " ", middleinitial, " ", lastname)) %>% 
  select(all_of(cols))# %>%
  #mutate(
   # dob=as_date(dob))

# Reformat the old dataset to match the new format
attach(o)
# o_reformatted <- o %>%
#   transmute(
#   "prescreendate" = pscreen_date, # nolint
#   "email" = pscreen_email,
#     repro1 = repro1,
#     repro2 = repro_c2,
#     repro3 = repro2,
#     repro4 = straw2c_6,
#     repro5 = straw2,
#     repro7 = straw2a,
#     #repro9 = repro_supplemental_3,
#     repro10 = straw2c_4,
#     repro11 = straw2c_3,
#     #repro12 = straw2c_5,
#     repro15 = repro_c_4,
#     repro16 = repro_c_4_1,
#     #repro17 = repro_supplement_4,
#     repro18 = repro_c_6,
#     repro19 = repro_c_6_1,
#     repro20 = repro_c_6_2,
#     #repro23 = meno_impact,
#     #repro22 = meno_difficult,
#     #repro25 = meno_treatment2,
#     #repro26 = meno_doctors,
#     #repro27 = meno_younger,
#     #repro28 = meno_peers,
#     #repro29 = meno_advice,
#     repro30 = repro_c_7,
#     repro31 = repro_c_7_1,
#     repro32 = repro_c_7_2,
#     repro33 = repro_c_7_3,
#     #repro34 = repro_c_7_4,
#     #repro35 = repro_c_7_5,
#     repro36 = repro_c_7_6,
#     #repro37 = induction,
#     #repro38 = repro_c_7_7,
#     #repro39 = repro_c_7_8,
#     #repro40 = repro_c_7_9,
#     #repro42 = csection2,
#     #repro43 = repro_pregnancy,
#     #repro44 = repro_pregcompli,
#     #repro45 = repro_labor,
#     #repro46 = repro_laborcomp,
#     repro47 = repro_section3_3,
#     repro48 = repro_section3_4,
#     repro49 = postpartumpsychosis,
#     #repro50 = repro_section3_4a,
#     #repro51 = repro_section3_4b,
#     #repro52 = repro_c_8,
#     #repro53 = repro_c_8_1,
#     #repro54 = repro_c_8_2,
#     #"repro55___1"=str_detect(repro_c_8_3, "1"),
#     #"repro55___2"=str_detect(prescreen_hyster_type, "2"),
#     #"repro55___3"=str_detect(prescreen_hyster_type, "3"),
#     #"repro55___4"=str_detect(prescreen_hyster_type, "4"),
#     #"repro55___5"=str_detect(prescreen_hyster_type, "5"),
#     #"repro55___9"=str_detect(prescreen_hyster_type, "6"), # always false as 6 not an option in old
#     #"repro55___1"=str_detect(prescreen_hyster_type, "7"), 
#     #repro56 = repro_c_8_4,
#     repro57 = repro_c_9,
#     repro58 = repro_c_9_1,
#     #repro59 = repro_c_9_2,
#     #repro60 = repro_c_9_3,
#     repro61 = repro_c_9_4,
#     repro62 = repro_c_10,
#     repro63 = repro_c_10_1,
#     repro64 = repro_c_11,
#     repro65 = repro_c_11_1,
#     #repro66 = repro_c_12,
#     #repro67 = repro_c_12_1,
#     #repro68 = repro_c_13,
#     #repro69 = repro_c_13_1,
#     #repro70 = repro_c_14,
#     #repro71 = repro_c_14_1,
#     #repro72 = repro_c_14_2,
#     #repro73 = NA,#repro_c_14_3,
#     #repro74 = repro_c_14_4,
#     #repro75 = repro_c_15,
#     #repro76 = NA, #repro_c_15_1,
#     #repro77 = repro_c_15_2,
#     repro78 = repro_breastcancer,
#     repro79 = repro_ovarcance,
#     repro80 = repro13,
#     repro82 = repro15,
#     repro81 = repro13a,
# "repro83___1" = repro13a_2___1,
# "repro83___2" = repro13a_2___2,
# "repro83___3" = repro13a_2___3,
# "repro83___4" = repro13a_2___4,
# "repro83___5" = repro13a_2___5,
# "repro83___6" = repro13a_2___6,
# "repro83___7" = repro13a_2___7,
#     repro84 = repro_hormonmeds,
#     repro85 = repro_hormonmedstype,
#     repro86 = repro_hormonmeds3,
#     repro87 = repro_hormonmeds4,
#     repro88 = repro14,
#     pqb_symptom = pqb_symptom,
#     pqb_concern = pqb_concern,
#     #repro89 = NA,#mother,
#     #repro90 = repro_mothersdep,
#     #repro91 = repro_motherppd,
#     #repro92 = motherpostpartumpsycho,
#     #repro93 = meno_mothermeno,
#     #repro94 = momperiillness,
#     #repro95 = meno_mothermeno2,
#     #repro6 = NA,
#     #repro8 = NA,
#     #repro13 = NA,
#     #repro14 = NA,
#     #repro21 = NA,
#     #repro21notes = NA,
#     #repro24 = NA
#   ) %>% 
#     as_tibble 

# 6) rbind old and new  --------------------------------------------------------------------------
new_df<-rbind(
    cbind(o_reformatted, "DataCollection"="old"),
    cbind(n_selected, "DataCollection"="new")
) %>% as_tibble 

# Define internal lookup for repro variable renaming
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
new_df_renamed <- new_df %>%
  rename_with(.fn = ~ repro_name_map[.x], .cols = names(repro_name_map)[names(repro_name_map) %in% names(.)])

exclude_cols <- c("prescreendate", "email", "DataCollection")

# Filter out rows where all other columns are NA, 0, or blank
cleaned_df <- new_df_renamed %>%
  filter(
    rowSums(
      across(
        .cols = -all_of(exclude_cols),
        .fns = ~ !is.na(.) & . != 0 & . != ""
      )
    ) > 0
  )



write.csv(cleaned_df, "Outputs/joinedCorrected_RHQ_dfELIGIBLEONLY.csv")
saveRDS(cleaned_df, "Outputs/joinedCorrected_RHQ_dfELIGIBLEONLY.rds")
#write.csv(new_df_renamed, "Outputs/joinedCorrected_RHQ_df.csv")
#saveRDS(new_df_renamed, "Outputs/joinedCorrected_RHQ_df.rds")

length(repro_name_map)