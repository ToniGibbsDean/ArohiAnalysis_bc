library(tidyverse)
library(dplyr)
library(ggpubr)
library(dplyr)
library(purrr)
library(rlang)
library(lubridate)
path="Figures"

df <-readRDS("Outputs/RHQ_PSCREEN_MEDLOGdfELIGIBLEONLY.rds")


df_unique <- df %>% group_by(record_id) %>% summarise(across(everything(), ~ first(na.omit(.))), .groups = "drop")

nrow(df_unique)


datcl <- df_unique %>%
  mutate(
    hormone_sensitivity = 0,  # initialize
    hormone_sensitivity = hormone_sensitivity +
      ifelse(!is.na(cycle_regular_now) & cycle_regular_now == 1, 1, 0) +
      ifelse(!is.na(symptoms_bc) & symptoms_bc == 1, 2, 0) +
      ifelse(!is.na(prenatal_depression) & prenatal_depression == 1, 2, 0) +
      ifelse(!is.na(postpartum_depression) & postpartum_depression == 1, 2, 0) +
      ifelse(!is.na(postpartum_psychosis) & postpartum_psychosis == 1, 3, 0)
  ) %>%
  mutate(
    dob_parsed = parse_date_time(dob, orders = c("mdy", "dmy", "ymd")),
    prescreendate_parsed = parse_date_time(prescreendate, orders = c("mdy", "dmy", "ymd")),
    age = floor(time_length(interval(dob_parsed, prescreendate_parsed), "years")),
    age_years = floor(time_length(interval(dob_parsed, prescreendate_parsed), "years")),
    age_weeks = time_length(interval(dob_parsed, prescreendate_parsed), "weeks"), # convert years to weeks
    
    country_US = as_factor(case_when(
      is.na(residence_state) ~ "NotUSA",
      TRUE ~ "USA"
    )),
    
    zipcode = as_factor(residence_zipcode),
    
    lastperiod_parsed = parse_date_time(lastperiod, orders = c("mdy", "dmy", "ymd")),
    lastperiod_daysSince = floor(time_length(interval(lastperiod_parsed, prescreendate_parsed), "days")),
    menopausalStatus = lastperiod_daysSince > 365,
    
    bc_lifetime = case_when(
      bc_pill_ever == 1 | bc_patch_ever == 1 | bc_ring_ever == 1 |
      bc_implant_ever == 1 | bc_injection_ever == 1 | bc_iud_ever == 1 |
      bc_other_ever == 1 ~ "yes",
      TRUE ~ "no"
    ))
    
library(dplyr)
library(lubridate)

x <- datcl

for (i in 1:25) {
  medstart_col <- paste0("medstart", i)
  medend_col <- paste0("medend", i)
  medtype_col <- paste0("medtype", i)
  medcurrent_col <- paste0("medcurrent", i, "___1")
  duration_col <- paste0("bc", i, "_weeks")
  
  x <- x %>% mutate(
    !!duration_col := case_when(
      !!sym(medtype_col) == 0 ~ as.numeric(
        time_length(
          interval(
            parse_date_time(!!sym(medstart_col), orders = c("mdy", "dmy", "ymd")),
            if_else(
              is.na(!!sym(medend_col)) & !!sym(medcurrent_col) == 1,
              parse_date_time(rhq_date, orders = c("mdy", "dmy", "ymd")),
              parse_date_time(!!sym(medend_col), orders = c("mdy", "dmy", "ymd")),
              missing = as.POSIXct(NA)  # <- fix: use datetime NA
            )
          ),
          "weeks"
        )
      ),
      TRUE ~ NA_real_
    )
  )
}

# Sum all bc{num}_weeks columns into bc_exposure
x <- x %>%
  mutate(
    bc_exposure = rowSums(select(., matches("^bc\\d+_weeks$")), na.rm = TRUE),
    bc_exposure = bc_exposure / age_weeks
  )

print(range(x$bc_exposure, na.rm = TRUE))

# Mood and psychosis dx data - must be processed in old and new redcap projects seperately 

newcheck <- x %>%
  mutate(
    BPDI = factor(case_when(mooddx1 == 0 | mooddx1 == 3 ~ 0, mooddx1 == 1 ~ 1, TRUE ~ 0)),
    BPDII = factor(case_when(mooddx2 == 0 | mooddx1 == 3 ~ 0, mooddx2 == 1 ~ 1, TRUE ~ 0)),
    MDD_Dep_otherDep = factor(case_when(mooddx3 == 0 | mooddx1 == 3 ~ 0, mooddx3 == 1 ~ 1, TRUE ~ 0)),
    CyclothymicDep = factor(case_when(mooddx4 == 0 | mooddx1 == 3 ~ 0, mooddx4 == 1 ~ 1, TRUE ~ 0)),
    BPD_related_onlyNewRedCap = factor(case_when(mooddx5 == 0 | mooddx1 == 3 ~ 0, mooddx5 == 1 ~ 1, TRUE ~ 0)),
    Other_dep_onlyNewRedCap = factor(case_when(mooddx6 == 0 | mooddx1 == 3 ~ 0, mooddx6 == 1 ~ 1, TRUE ~ 0)),
  )
  ##filter(
    #(BPDII_age <= 100 | is.na(BPDII_age)),
    #(MDD_Dep_otherDep_age <= 100 | is.na(MDD_Dep_otherDep_age)),
    #(Other_dep_onlyNewRedCap_age <= 100 | is.na(Other_dep_onlyNewRedCap_age))
  #) %>%
  

allClnDat1 <- bind_rows(newcheck) %>%
  select(-starts_with("mooddx")) %>%
  mutate(
    moodDXyesNo = rowSums(across(c(BPDI, BPDII, MDD_Dep_otherDep, CyclothymicDep, Other_dep_onlyNewRedCap, BPD_related_onlyNewRedCap)) == 1, na.rm = TRUE) > 0
  )
#DIM OF ALLCLNDAT = 1387
allClnDat <- allClnDat1 %>%
  mutate(record_id = as.character(record_id))
df <- df %>%
  mutate(record_id = as.character(record_id))

# allClnDat <- allClnDat1 %>%
#   left_join(select(df, record_id, cycle_regular_now, prenatal_depression, postpartum_depression, postpartum_psychosis), by = "record_id") # nolint

#filtering for cleaning puposes DIM = 1307

datcl_filtered <- allClnDat %>%
  filter(
    !is.na(lastperiod_daysSince),
    lastperiod_daysSince >= 0 & lastperiod_daysSince <= 20000,
    age > 0 & !is.na(age),
    lastperiod_daysSince >= 0 & lastperiod_daysSince <= 20000,
  )

############################################################################
#Create exclusions tracking csv
###########################################################################
track_filters <- function(data, ...) {
  conditions <- enquos(...)
  names(conditions) <- sapply(conditions, rlang::as_label)

  out <- tibble(Step = "Original", RowsRemaining = nrow(data), RowsRemoved = 0)
  current_data <- data

  for (i in seq_along(conditions)) {
    step_label <- names(conditions)[i]
    before <- nrow(current_data)
    current_data <- filter(current_data, !!conditions[[i]])
    after <- nrow(current_data)
    out <- bind_rows(out, tibble(
      Step = step_label,
      RowsRemaining = after,
      RowsRemoved = before - after
    ))
  }

  return(out)
}

filter_report <- track_filters(
  allClnDat,
    !is.na(lastperiod_daysSince),
    lastperiod_daysSince >= 0 & lastperiod_daysSince <= 20000,
    age > 0 & !is.na(age),
    lastperiod_daysSince >= 0 & lastperiod_daysSince <= 20000,
)


# create summary data frame
summarydf <- datcl_filtered %>%
  select(
    record_id, 
    email,
    age,
    age_weeks, 
    zipcode,
    lastperiod_daysSince,
    ever_pregnant,
    menopausalStatus,
    pqb_symptom,
    pqb_concern,
    symptoms_bc,
    cycle_regular_now,
    prenatal_depression,
    postpartum_depression,
    postpartum_psychosis,
    hormone_sensitivity,
    bc_lifetime,
    #hyster2_ageAtHystorOop,
    #remove_flag,
    moodDXyesNo,
    starts_with("med"),
    starts_with("bc")
  )


table(summarydf$hormone_sensitivity)
saveRDS(summarydf, "Outputs/summarydf.RDS")
write.csv(filter_report, "Outputs/trackingExclusions_datacleaning.csv")
write.csv(summarydf, "Outputs/summarydf.csv")

i <- 1450
start <- parse_date_time(x$medstart1, orders = "mdy")
end <- parse_date_time(x$medend1, orders = "mdy")
head(start); head(end)