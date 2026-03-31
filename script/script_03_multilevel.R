#los - Length of stay was modeled as the outcome (or dependent variable) 
##in a GLMM
install.packages("influence.ME")
install.packages("emmeans")
install.packages("fitdistrplus")
install.packages("glmmTMB")
install.packages("broom.mixed")
install.packages("sjPlot")
install.packages("ggeffects")
library(tidyverse)
library(lme4)
library(performance)
library (sjPlot)
library (DHARMa)
library (car)
library(influence.ME)
library(emmeans)
library(MASS)
library(ggeffects)
library(fitdistrplus)
library(glmmTMB)
library(broom.mixed)
library(sjPlot)
library(ggeffects)
library(here)
# Load analytic dataset
sparcs_hf <- readRDS("data/processed_data/sparcs_hf.rds")
#check
glimpse(sparcs_hf)

#check assimetry los 
hist(sparcs_hf$los)
#save as figure
png(filename = here("figure", "histogram_los.png"),
    res = 600, width = 6, height = 4.5, units = "in")
hist(sparcs_hf$los)
dev.off()

#check assimetry qqplot 
assimetry_qqplot <- car::qqPlot(sparcs_hf$los)
#save as figure
png(filename = here("figure", "hist_assimetry_los.png"),
    res=600, width = 6, height=4.5, units = "in")
car::qqPlot(sparcs_hf$los)
dev.off()

#check Cullen and Frey graph = gamma is the best choice
fitdistrplus::descdist(sparcs_hf$los)
#save as figure
png(filename = here("figure", "cullen_frey.png"),
    res = 600, width = 6, height = 4.5, units = "in")
fitdistrplus::descdist(sparcs_hf$los)
dev.off()

#GLMM negative binomial
mod_glmm_nb <- glmmTMB::glmmTMB(
  los ~ age_group + sex + race + apr_severity + payer_group +
    (1 | hospital_id),
  data   = sparcs_hf,
  family = nbinom2)

#check model fit assessment = deviation significant outliers 
DHARMa::simulateResiduals(mod_glmm_nb)|> plot()

#check quantile outliers = 95%=16 97%=19 99%=28 99.9%=63
quantile(sparcs_hf$los, c(0.95, 0.97, 0.99, 0.999))

#check count number of outliers (patients) = 232
sparcs_hf |> 
  mutate(outlier = los > quantile(los, 0.99)) |> 
  count(outlier)

#check count severity of 232 outliers patients. extreme 122 major 94 moderate 16 = decision: delete 
sparcs_hf |> 
  filter(los > quantile(los, 0.99)) |> 
  count(apr_severity, sort = TRUE)

# delete outliers (LOS above quantile 99)

#GLMM negative binomial after deleting 232 outliers
mod_glmm_nb <- glmmTMB::glmmTMB(
  los ~ age_group + sex + race + apr_severity + payer_group +
    (1 | hospital_id),
  data   = sparcs_hf_sens,
  family = nbinom2)

# check DHARMa after deleting 232 outliers. better
DHARMa::simulateResiduals(mod_glmm_nb) |>plot()

#check random residuals ok 
performance::check_normality(mod_glmm_nb, effects = "random") |>plot()

#check collinearity = age 1.22 sex 1.02 race 1.03 apr_severity 1.00 payer_group 1.20 ok 
performance::check_collinearity(mod_glmm_nb)

#statistical model interpretation
summary(mod_glmm_nb) #results in log 

#exponential summary - statistical model interpretation 
sjPlot::tab_model(mod_glmm_nb, transform="exp")

#compare categories p<0.05 = age, sex race apr severity and payer group
emmeans::joint_tests(mod_glmm_nb)

#compare age_group 4.31 days 4.73 days 5.06 days
emmeans::emmeans(mod_glmm_nb, ~ age_group, type = "response")

#compare sex 4.64 days 4.74 days
emmeans::emmeans(mod_glmm_nb, ~ sex, type = "response")

#compare race white 4.73 days black 4.80 days other 4.54 days
emmeans::emmeans(mod_glmm_nb, ~ race, type = "response")

#compare severity minor 2.72 days moderate 3.74 days major 5.48 days extreme 8.67 days 
emmeans::emmeans(mod_glmm_nb, ~ apr_severity, type = "response")

#compare payer_group private 4.51 days medicare 4.75 days medicaid 4.84 days other 4.66 days
emmeans::emmeans(mod_glmm_nb, ~ payer_group, type = "response")

#statistical comparison
#compare age_group
emmeans::emmeans(mod_glmm_nb, ~ age_group, type = "response") |> 
  pairs(adjust = "Tukey", reverse = T)
#statistical comparison
#compare sex
emmeans::emmeans(mod_glmm_nb, ~ sex, type = "response") |> 
  pairs(adjust = "Tukey", reverse = T)
#statistical comparison
#compare race
emmeans::emmeans(mod_glmm_nb, ~ race, type = "response") |> 
  pairs(adjust = "Tukey", reverse = T)
#statistical comparison
#compare apr_severity
emmeans::emmeans(mod_glmm_nb, ~ apr_severity, type = "response") |> 
  pairs(adjust = "Tukey", reverse = T)
#statistical comparison
#compare payer_group
emmeans::emmeans(mod_glmm_nb, ~ payer_group, type = "response") |> 
  pairs(adjust = "Tukey", reverse = T)
#statistical comparison
#compare race
emmeans::emmeans(mod_glmm_nb, ~ race, type = "response") |> 
  pairs(adjust = "Tukey", reverse = T)
#statistical comparison
#compare severity
emmeans::emmeans(mod_glmm_nb, ~ apr_severity, type = "response") |> 
  pairs(adjust = "Tukey", reverse = T)
#statistical comparison
#compare payer_group
emmeans::emmeans(mod_glmm_nb, ~ payer_group, type = "response") |> 
  pairs(adjust = "Tukey", reverse = T)

#save as RDS  
saveRDS(sparcs_hf_sens, here("data", "processed_data", "sparcs_hf_sens.rds"))
