#load packs
library(tidyverse)
library(here)
library(readr)
library(janitor)

#load file
sparcs_hf <- readRDS("data/processed_data/sparcs_hf.rds")
#check
dplyr::glimpse(sparcs_hf)

#check los = right_skewed = classic pattern los Ok = GAMMA MIXED MODEL 
hist_los <- ggplot(sparcs_hf, aes(x = los)) +
  geom_histogram(bins = 50)

hist_los
#save
ggsave(filename = here("figure", "hist_los.png"),
       plot = hist_los, dpi=600, width = 6, height=4.5)

#check los and severity = see if something changes. Yes! los mean and median rise with severity = good predictor
sparcs_hf |>
  dplyr::group_by(apr_severity) |>
  dplyr::summarise(median_los = median(los),
                   mean_los = mean(los))

#check los and age = see if something changes. No! see los median low rise = weak predictor
sparcs_hf |>
  dplyr::group_by(age_group) |>
  dplyr::summarise(median_los = median(los),
                   mean_los = mean(los))


#check los and sex = see if something changes. no! median and mean dont change = weak predictor
sparcs_hf |>
  group_by(sex) |>
  summarise(median_los = median(los),
    mean_los = mean(los),n = n())

#check race = see somenthing changes.median changes only >70 e mean rises with age
sparcs_hf |>
  group_by(race) |>
  summarise(median_los = median(los),
            mean_los = mean(los),n = n())

#check los and payer = see if something changes. median = for all payers = payer is not a predictor. Mean medicare los higher (older)
#medicare (older patients = higher los and medicaid (higher severity).
sparcs_hf |>
  dplyr::group_by(payer_group) |>
  dplyr::summarise(median_los = median(los),
                   mean_los = mean(los),n = n())

#check 
table(sparcs_hf$payer_group)

#check if severity and payer are related (numbers)
table(sparcs_hf$payer_group, sparcs_hf$apr_severity)

#check if severity and payer are related (%)
prop.table(table(sparcs_hf$payer_group,
                 sparcs_hf$apr_severity), 1)                 
                 
#the difference in los can be explained by age, severity and hospital. 
#the model need to adjust for these variables
#save as RDS  
saveRDS(sparcs_hf,"data/processed_data/sparcs_hf.rds")