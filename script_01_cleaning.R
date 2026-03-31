#load packs
library(tidyverse)
library(here)
library(readr)
library(janitor)
library(dplyr)

#load file
sparcs <-read.csv2(here("data", "raw_data", "sparcs_2024.csv"))

#check 
dplyr::glimpse(sparcs)

#change columns names
sparcs <- sparcs |>
  rename(
    health_service_area = Health.Service.Area,
    hospital_county = Hospital.County,
    hospital_id = Facility.Name,
    age_group = Age.Group,
    sex = Gender,
    race = Race,
    ethnicity = Ethnicity,
    los = Length.of.Stay,
    admission_type = Type.of.Admission,
    discharge_status = Patient.Disposition,
    discharge_year = Discharge.Year,
    ccsr_dx_code = CCSR.Diagnosis.Code,
    ccsr_dx = CCSR.Diagnosis.Description,
    ccsr_proc_code = CCSR.Procedure.Code,
    ccsr_proc = CCSR.Procedure.Description,
    apr_drg_code = APR.DRG.Code,
    apr_drg = APR.DRG.Description,
    apr_mdc_code = APR.MDC.Code,
    apr_mdc = APR.MDC.Description,
    apr_severity_code = APR.Severity.of.Illness.Code,
    apr_severity = APR.Severity.of.Illness.Description,
    apr_mortality_risk = APR.Risk.of.Mortality,
    med_surg_type = APR.Medical.Surgical.Description,
    payer_primary = Payment.Typology.1,
    payer_secondary = Payment.Typology.2,
    payer_tertiary = Payment.Typology.3,
    birth_weight = Birth.Weight,
    ed_indicator = Emergency.Department.Indicator,
    charges = Total.Charges,
    costs = Total.Costs)

#check
dplyr::glimpse(sparcs)

#create sparcs_hf = sparcs heart failure
sparcs_hf <- sparcs |>
  dplyr::filter(apr_drg_code == 194,ed_indicator == "Y") |>
  dplyr::select(
    hospital_id,
    age_group,
    sex,
    race,
    los,
    apr_drg_code,
    apr_drg,
    apr_severity,
    payer_primary,
    payer_secondary,
    ed_indicator)

#check
dplyr::glimpse(sparcs_hf)

#change variables from <chr> to <fct> 
sparcs_hf <- sparcs_hf |>
  mutate(los = readr::parse_number(los),
    hospital_id = factor(hospital_id),
    age_group = factor(age_group),
    sex = factor(sex),
    race = factor(race),
    apr_severity = factor(apr_severity),
    payer_primary = factor(payer_primary),
    payer_secondary = factor(payer_secondary))

#check variables
dplyr::glimpse(sparcs_hf)

#check hospitals = 168
length(unique(sparcs_hf$hospital_id))

#check age_group = 5 levels
length(unique(sparcs_hf$age_group))
table(sparcs_hf$age_group)
#remove patients younger than 30 years old = pediatric conditions heart failure
sparcs_hf <- sparcs_hf |>
  filter(age_group != "0-17",
         age_group != "18-29")
#create 3 ages groups = <50, 50-69 and >=70 years old
sparcs_hf <- sparcs_hf |>
  mutate(age_group = case_when(
      age_group == "30-49" ~ "<50",
      age_group == "50-69" ~ "50-69",
      age_group == "70 or Older" ~ ">=70"),
    age_group = factor(age_group, levels = c("<50", "50-69", ">=70")))  
#check 
table(sparcs_hf$age_group)

#check sex = 3 12527 F 13335M and 2 U   
length(unique(sparcs_hf$sex))
table(sparcs_hf$sex)
#remove 2 individual with sex = U
sparcs_hf <- sparcs_hf |>
  dplyr::filter(sex %in% c("M", "F")) |>
  dplyr::mutate(sex = droplevels(sex))
table(sparcs_hf$sex)
#check race
length(unique(sparcs_hf$race))
#delete race na
sparcs_hf <- sparcs_hf |>
  dplyr::filter(!is.na(race)) |> 
  dplyr::mutate(across(where(is.factor), droplevels))
#Check race
table(sparcs_hf$race)
#check payer = 10 categories and 3 with missing values
table(sparcs_hf$payer_primary)
#remove 3 missing values create 4 categories and create payer_group
sparcs_hf <- sparcs_hf |>
  filter(payer_primary != "") |>
  mutate(payer_group = case_when(
    payer_primary == "Medicare" ~ "Medicare",
    payer_primary == "Medicaid" ~ "Medicaid",
    payer_primary %in% c("Private Health Insurance",
                         "Blue Cross/Blue Shield") ~ "Private",TRUE ~ "Other"))

#check 
table(sparcs_hf$payer_group)

# create levels for variables 
sparcs_hf <- sparcs_hf |>
  mutate(hospital_id = factor(hospital_id),
         age_group = factor(age_group,levels = c("<50","50-69",">=70")),
         sex = factor(sex,levels = c("M","F")),
         race = factor(race,levels = c("White","Black/African American","Other Race")),
         apr_severity = factor(apr_severity,levels = c("Minor","Moderate","Major","Extreme")),
         payer_group = factor(payer_group,levels = c("Private","Medicare","Medicaid","Other")))

#check levels
lapply(sparcs_hf[,c("age_group", "sex", "race", "apr_severity", "payer_group")], levels)

#save as RDS = keep variables class and levels 
saveRDS(sparcs_hf,"data/processed_data/sparcs_hf.rds")