#load packs
library(tidyverse)
library(here)
library(janitor)
#---------------------------------------------------
#CLEANING DATASET 
#---------------------------------------------------
#What patient and hospital-level factors are associated with length of stay 
#among patients hospitalized for heart failure?
#---------------------------------------------------

#load file
sparcs <-read.csv2(here("data", "raw_data", "sparcs_2024.csv"))

#check 
dplyr::glimpse(sparcs)
#---------------------------------------------------
#VARIABLES OF INTEREST: hospital_id, age_group, sex, race, los, discharge_status, 
#ccsr_proc, apr_drg, apr_severity, payer_primary, ed_indicator
#---------------------------------------------------

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

#delete "  " and "" in <chr> variables
sparcs <- sparcs |> 
  dplyr::mutate(
    across(where(is.character), ~na_if(trimws(.), "")))
  
#check missing values in all variables (SEE variables of interest = hospital_id=0, 
#age_group=0, sex=0, race=0, discharge_status=0, ccsr_proc=352461, apr_drg=64015, apr_severity=64015,
#payer_primary=64121, ed_indicator=64015
colSums(is.na(sparcs)) 

#find "HEART" in apr_drg  = HEART FAILURE = 28063
sparcs |> 
   filter (grepl("HEART", apr_drg)) |> 
  count(apr_drg, sort = TRUE)

#find how many heart failure came from emergency = 25963. no emergency =  2100 total = 28063
table(sparcs$apr_drg == "HEART FAILURE", 
      sparcs$ed_indicator, useNA = "always")

#create final dataset = only heart failure = sparcs_hf
sparcs_hf <-sparcs |> 
  dplyr::filter(
    apr_drg =="HEART FAILURE")
    
#check rows new dataset = 28063 ok
nrow(sparcs_hf)

#check duplicates = 1
sum(duplicated(sparcs_hf))
#inspection duplicate 
sparcs_hf[duplicated(sparcs_hf) | duplicated(sparcs_hf, fromLast = TRUE), ]
#remove duplicate
sparcs_hf <- sparcs_hf |> 
  dplyr::distinct()
#check if duplicate = 0
sum(duplicated(sparcs_hf))

#change class variables from <chr> to (fct> (here only <chr> variables) 
#variables of interest = hospital_id, age_group, sex, race, discharge_status, 
#ccsr_proc, apr_drg, apr_severity, payer_primary and ed_indicator
sparcs_hf <- sparcs_hf |>
  dplyr::mutate(
    health_service_area = factor(health_service_area),
    hospital_county = factor(hospital_county),
    hospital_id = factor(hospital_id),
    age_group = factor(age_group),
    sex = factor(sex),
    race = factor(race),
    ethnicity = factor(ethnicity),
    admission_type = factor(admission_type),
    discharge_status = factor(discharge_status),
    ccsr_dx = factor(ccsr_dx),
    ccsr_proc = factor(ccsr_proc),
    apr_drg = factor(apr_drg),
    apr_mdc = factor(apr_mdc),
    apr_severity = factor(apr_severity),
    apr_mortality_risk = factor(apr_mortality_risk),
    med_surg_type = factor(med_surg_type),
    payer_primary = factor(payer_primary),
    payer_secondary = factor(payer_secondary),
    payer_tertiary = factor(payer_tertiary),
    ed_indicator = factor(ed_indicator))

#find levels age_group
levels(sparcs_hf$age_group)
#count levels age_group
table(sparcs_hf$age_group)
#delete levels age group <=29 years old
sparcs_hf <- sparcs_hf |>
  dplyr::filter(!age_group %in% c("0-17", "18-29"))
#check if 0-17 = 0 and 18-29 =0 30-49= 1597 50-69=8337 and 70r older =18005
table(sparcs_hf$age_group)
#drop empty levels 
sparcs_hf <- droplevels(sparcs_hf)

#find levels sex
levels(sparcs_hf$sex) 
#count levels sex
table(sparcs_hf$sex)
#delete sex U
sparcs_hf<- sparcs_hf |> 
  dplyr::filter(sex!="U")
#check if F = 13511 M = 14426 and U =0
table(sparcs_hf$sex)
#drop empty level
sparcs_hf <- droplevels(sparcs_hf)

#find levels race 
levels(sparcs_hf$race)
#count levels race
table(sparcs_hf$race)
#group from 4 categories to 3 categories
sparcs_hf <- sparcs_hf |>
  dplyr::mutate(
    race_group = dplyr::case_when(
      race == "Black/African American" ~ "Black/African American",
      race == "White" ~ "White",
      race %in% c("Multi-racial", "Other Race") ~ "Other Race",
      TRUE ~ NA_character_))
#convert race group as factor 
sparcs_hf$race_group <- factor(sparcs_hf$race_group)
#count race_group = black=6740 other race= 6231 and white=14966
table(sparcs_hf$race_group)

#find levels discharge_status
levels(sparcs_hf$discharge_status)
#count levels discharge_status
table(sparcs_hf$discharge_status)
#group from 19 categories to 9 categories
sparcs_hf <- sparcs_hf |>
  dplyr::mutate(
    discharge_group = dplyr::case_when(
      discharge_status == "Home or Self Care" ~ "Home",
      discharge_status == "Home w/ Home Health Services" ~ "Home Health",
      discharge_status %in% c("Skilled Nursing Home","Medicaid Cert Nursing Facility",
        "Medicare Cert Long Term Care Hospital") ~ "Skilled Nursing / LTC",
      discharge_status %in% c("Short-term Hospital","Critical Access Hospital") ~ "Hospital Transfer",
      discharge_status == "Inpatient Rehabilitation Facility" ~ "Rehabilitation",
      discharge_status %in% c("Hospice - Home","Hospice - Medical Facility") ~ "Hospice",
      discharge_status == "Expired" ~ "Death",
      discharge_status == "Left Against Medical Advice" ~ "Left AMA",TRUE ~ "Other"))
#count levels discharge_group
table(sparcs_hf$discharge_group)
#convert discharge_group as factor 
sparcs_hf$discharge_group <- factor(sparcs_hf$discharge_group)
#count discharge_group = 9
table(sparcs_hf$discharge_group)

#find levels ccsr_proc
levels(sparcs_hf$ccsr_proc)
#count levels ccsr_proc = 132 levels
table(sparcs_hf$ccsr_proc)
#group from 132 levels to 4 categories
sparcs_hf <- sparcs_hf |>
  mutate(proc_group = case_when(is.na(ccsr_proc) ~ "none",
    ccsr_proc %in% c("CARDIOVERSION",
        "PACEMAKER AND DEFIBRILLATOR INTERROGATION",
        "CARDIAC STRESS TESTS","CARDIAC MONITORING",
        "CARDIAC ASSISTANCE WITH BALLOON PUMP","PERICARDIAL PROCEDURES",
        "CARDIOVASCULAR DEVICE PROCEDURES, NEC") ~ "cardiac_procedure",
    ccsr_proc %in% c("NON-INVASIVE VENTILATION","MECHANICAL VENTILATION",
        "AIRWAY INTUBATION","TRACHEOSTOMY","HEMODIALYSIS","PERITONEAL DIALYSIS",
        "INFUSION OF VASOPRESSOR","TRANSFUSION OF BLOOD AND BLOOD PRODUCTS",
        "TRANSFUSION OF PLASMA") ~ "organ_or_respiratory_support",
      TRUE ~ "minor_or_diagnostic"))
#count levels proc_group
table(sparcs_hf$proc_group)
#convert proc_group as factor
sparcs_hf$proc_group <- factor(sparcs_hf$proc_group)
#count proc_group = 4
table(sparcs_hf$proc_group)

#find levels payer_primary 9 categories
levels(sparcs_hf$payer_primary)
#count levels payer_primary
table(sparcs_hf$payer_primary)
#group from 9 categories to 6 categories
sparcs_hf <- sparcs_hf |>
  dplyr::mutate(
    payer_group = dplyr::case_when(
      payer_primary == "Medicare" ~ "Medicare",
      payer_primary == "Medicaid" ~ "Medicaid",
      payer_primary %in% c("Federal/State/Local/VA", "Department of Corrections") ~ "Government",
      payer_primary %in% c("Private Health Insurance", "Blue Cross/Blue Shield") ~ "Private",
      payer_primary == "Self-Pay" ~ "Self-Pay",
      payer_primary %in% c("Miscellaneous/Other", "Managed Care, Unspecified") ~ "Other",
      TRUE ~ "Other"))
#count new levels payer_group
table(sparcs_hf$payer_group)
#convert payer_group as factor
sparcs_hf$payer_group <- factor(sparcs_hf$payer_group)
#count payer_group = 6
table(sparcs_hf$payer_group)

#create variable ed_admission (emergency) Yes or No 
sparcs_hf <- sparcs_hf |> 
  mutate(ed_admission = ifelse( 
    ed_indicator == "Y", "Emergency", "Non-emergency")) 
#count new levels ed_admission 
table(sparcs_hf$ed_admission) 
#convert ed_admission as factor 
sparcs_hf$ed_admission <- factor(sparcs_hf$ed_admission) 
#count ed_admission N=2076 Y=25863 table(sparcs_hf$ed_admission)
table(sparcs_hf$ed_admission) 

#change class variables from <int> to <num> (here only numeric) = los, costs and charges
sparcs_hf <- sparcs_hf |>
  dplyr::mutate(
    los = as.numeric(los),
    charges = as.numeric(charges),
    costs = as.numeric(costs))
#check if variables are numeric
sapply(sparcs_hf[, c("los","charges","costs")], class)

#check NA in los= 12, charges= 0, and costs =0
sum(is.na(sparcs_hf$los))
sum(is.na(sparcs_hf$charges))
sum(is.na(sparcs_hf$costs))

#remove 12 missing values variable los (outcome variable)
sparcs_hf <- sparcs_hf |>
  filter(!is.na(los))
#check if na in los = 0
sum(is.na(sparcs_hf$los))
#check if na in costs =0 
sum(is.na(sparcs_hf$costs))
#check if na in charges =0
sum(is.na(sparcs_hf$charges))


#summary los
summary(sparcs_hf$los)
#summary costs
summary(sparcs_hf$costs)
#summary charges
summary(sparcs_hf$charges)

#remove empty levels (<fct> variables) double check
sparcs_hf <- droplevels(sparcs_hf)

#----------------------------------------------------------
# QUICK CHECK BEFORE ANALYSIS
#----------------------------------------------------------
#check number of hospitals = 178 (random intercept) ok
n_distinct(sparcs_hf$hospital_id)

#check number of admissions per hospital
sparcs_hf |>
  count(hospital_id, sort = TRUE)

#summary hospital n of admissions min 1 max 512 median 128 mean 156
sparcs_hf |>
  count(hospital_id) |>
  summary()

##death changes los analysis
#check mean los X discharge_group
sparcs_hf |>
  dplyr::group_by(discharge_group) |>
  dplyr::summarise(mean_los = mean(los, na.rm = TRUE))

# check dataset sparcs_hf  and type of admission after final cleaning 
#emergency = 25854 and non-emergency = 2073 
table(sparcs_hf$ed_admission)

#check again NO missing values in numeric variables of interest = los, charges, costs
sum(is.na(sparcs_hf$los))
sum(is.na(sparcs_hf$charges))
sum(is.na(sparcs_hf$costs))

#check again NO missing values in factor variables of interest= hospital_id, age_group, race_group, sex, apr_drg,
#apr_severity, discharge_group, proc_group, payer_group and ed_admission
colSums(is.na(sparcs_hf))

#check final dataset
nrow(sparcs_hf) #27927 rows


#save as RDS 
saveRDS(sparcs_hf,"data/processed_data/sparcs_hf.rds")