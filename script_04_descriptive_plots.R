library(tidyverse)
library(scales)

#admissions by age_group 
graph_admissions_age_group <- sparcs |> 
  count(age_group, name = "n") |> 
  mutate(pct = round(n / sum(n) * 100, 1),
         age_group = fct_reorder(age_group, n)) |> 
  ggplot(aes(x = age_group, y = n)) +
  geom_col(fill = "#378ADD") +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = comma) +
  labs(title    = "Hospital Admissions by Age Group",
       subtitle = "SPARCS 2024",
       x        = NULL,
       y        = "Number of admissions") +
  theme_minimal(base_size = 12)

#save
ggsave(filename = here("figure", "admissions_age_group.png"),
       plot = graph_admissions_age_group, dpi=600, width = 6, height=4.5)

#admissions by sex 
graph_admissions_sex <- sparcs |>
  dplyr::filter(sex %in% c("M", "F")) |>
  dplyr::count(sex, name = "n") |>
  dplyr::mutate(pct = round(n / sum(n) * 100, 1)) |>
  ggplot(aes(x = sex, y = n, fill = sex)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = paste0(pct, "%")),
            vjust = -0.5, size = 4) +
  scale_fill_manual(values = c("M" = "#378ADD",
                               "F" = "#D4537E")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.12)),
                     labels = comma) +
  labs(title    = "Hospital Admissions by Sex",
       subtitle = "SPARCS 2024",
       x        = NULL,
       y        = "Number of admissions") +
  theme_minimal(base_size = 12)

graph_admissions_sex
#save
ggsave(filename = here("figure", "admissions_sex.png"),
       plot = graph_admissions_sex, dpi=600, width = 6, height=4.5)

#admissions by race
graph_admissions_race <- sparcs |> 
  dplyr::filter(race %in% c("White", "Black/African American", "Other Race")) |>
  dplyr::count(race, name = "n") |> 
  dplyr::mutate(pct   = round(n / sum(n) * 100, 1),
         race  = fct_reorder(race, n)) |> 
  ggplot(aes(x = race, y = n)) +
  geom_col(fill = "#1D9E75") +
  geom_text(aes(label = paste0(pct, "%")),
            hjust = -0.2, size = 3.5) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = comma) +
  labs(title    = "Hospital Admissions by Race",
       subtitle = "SPARCS 2024",
       x        = NULL,
       y        = "Number of admissions") +
  theme_minimal(base_size = 12)

graph_admissions_race
#save
ggsave(filename = here("figure", "admissions_race.png"),
       plot = graph_admissions_race, dpi=600, width = 6, height=4.5)

#admissions by top 15 disease group - 64,015 hospitalizations without classification 
graph_admissions_disease <- sparcs |>
  dplyr::count(apr_mdc, name = "n") |>
  dplyr::mutate(pct    = round(n / sum(n) * 100, 1),
                apr_mdc = fct_reorder(apr_mdc, n)) |>
  slice_max(n, n = 15) |>
  ggplot(aes(x = apr_mdc, y = n)) +
  geom_col(fill = "#534AB7") +
  geom_text(aes(label = comma(n)),
            hjust = -0.2, size = 3.2) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.25)),
                     labels = comma) +
  labs(title    = "Top 15 Disease Groups by Number of Admissions",
       subtitle = "SPARCS 2024 — APR MDC Classification",
       x        = NULL,
       y        = "Number of admissions") +
  theme_minimal(base_size = 12)

#find the gap in apr_mdc column
sparcs |>
  dplyr::filter(apr_mdc == "" | is.na(apr_mdc)) |>
  dplyr::count(ccsr_dx, sort = TRUE) |>
  as.data.frame() |>
  head(20)

#create a label "not classified" 
sparcs <- sparcs |>
  dplyr::mutate(
    apr_mdc = dplyr::case_when(is.na(apr_mdc) | 
                              apr_mdc == "" ~ "NOT CLASSIFIED",TRUE ~ apr_mdc))
#check 
sparcs |>
  dplyr::count(apr_mdc, sort = TRUE) |>
  as.data.frame() |>
  head(10)
#change from "not classified" to "NOT CLASSIFIED"
sparcs <- sparcs |>
dplyr::mutate(
  apr_mdc = forcats::fct_recode(apr_mdc,"NOT CLASSIFIED" = "Not classified"))
#check
sparcs |>
  dplyr::count(apr_mdc, sort = TRUE) |>
  as.data.frame() |>
  head(10)

#admissions by top 15 disease group - 
graph_admissions_disease <- sparcs |>
  dplyr::count(apr_mdc, name = "n") |>
  dplyr::mutate(pct     = round(n / sum(n) * 100, 1),
                apr_mdc = fct_reorder(apr_mdc, n)) |>
  slice_max(n, n = 15) |>
  ggplot(aes(x = apr_mdc, y = n)) +
  geom_col(fill = "#534AB7") +
  geom_text(aes(label = comma(n)),
            hjust = -0.2, size = 3.2) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20)),
                     labels = comma) +
  labs(title    = "Top 15 Disease Groups by Number of Admissions",
       subtitle = "SPARCS 2024 — APR MDC Classification",
       x        = NULL,
       y        = "Number of admissions") +
  theme_minimal(base_size = 12)

graph_admissions_disease

#save
ggsave(filename = here("figure", "admissions_disease.png"),
       plot = graph_admissions_disease, dpi=600, width = 14, height=8.0)

#cost by disease group  
graph_mean_cost_disease <- sparcs |>
  dplyr::mutate(costs = as.numeric(costs)) |>
  dplyr::group_by(apr_mdc) |>
  dplyr::summarise(mean_cost = mean(costs, na.rm = TRUE)) |>
  slice_max(mean_cost, n = 15) |>
  dplyr::mutate(apr_mdc = fct_reorder(apr_mdc, mean_cost)) |>
  ggplot(aes(x = apr_mdc, y = mean_cost)) +
  geom_col(fill = "#D85A30") +
  geom_text(aes(label = dollar(round(mean_cost))),
            hjust = -0.15, size = 3.2) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20)),labels = dollar) +
  labs(title    = "Top 15 Disease Groups by Mean Cost per Admission",
       subtitle = "SPARCS 2024",
       x        = NULL,
       y        = "Mean cost (USD)") +
  theme_minimal(base_size = 12)

graph_mean_cost_disease

#save
ggsave(filename = here("figure", "mean_cost_disease.png"),
       plot = graph_mean_cost_disease, dpi=600, width = 14, height=8.0)


#emergency vs elective admissions
graph_emergency_admissions <- sparcs |>
  dplyr::filter(ed_indicator %in% c("Y", "N")) |>
  dplyr::count(ed_indicator, name = "n") |>
  dplyr::mutate(
    pct   = round(n / sum(n) * 100, 1),
    label = paste0(ifelse(ed_indicator == "Y", "Emergency", "Elective"),
                   "\n", pct, "%")) |>
  ggplot(aes(x = 2, y = n, fill = ed_indicator)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  scale_fill_manual(values = c("Y" = "#D85A32", "N" = "#378ADD"),
                    labels = c("Y" = "Emergency", "N" = "Elective")) +
  geom_text(aes(label = label),
            position = position_stack(vjust = 0.5),
            color = "white", size = 4, fontface = "bold") +
  labs(title    = "Emergency vs Elective Admissions",
       subtitle = "SPARCS 2024", fill = NULL) +
  theme_void(base_size = 12) +
  theme(legend.position = "bottom", 
        plot.background  = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA))

graph_emergency_admissions

#save
ggsave(filename = here("figure", "emergency_admissions.png"),
       plot = graph_emergency_admissions, dpi=600, width = 6, height=4.5)

#length of stay distribution
graph_los_distribution <- sparcs |>
  dplyr::mutate(los = as.numeric(los)) |>
  dplyr::filter(los <= quantile(los, 0.99, na.rm = TRUE)) |>
  ggplot(aes(x = los)) +
  geom_histogram(binwidth = 1, fill = "#378ADD",
                 color = "white", alpha = 0.85) +
  stat_bin(binwidth = 1,
           geom = "text",
           aes(label = after_stat(count)),
           vjust = -0.5,
           size  = 2.5) +
  scale_x_continuous(breaks = seq(0, 30, 5)) +
  scale_y_continuous(labels = comma,
                     expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "Distribution of Length of Stay",
       subtitle = "SPARCS 2024 — admissions up to 99th percentile",
       x        = "Length of stay (days)",
       y        = "Number of admissions") +
  theme_minimal(base_size = 12)

graph_los_distribution
#save
ggsave(filename = here("figure", "los_distribution.png"),
       plot = graph_los_distribution, dpi=600, width = 14, height=8.0)

#mean los by group disease
##mutate los, cost and charges as numeric
sparcs <- sparcs |>
  dplyr::mutate(
    los     = as.numeric(los),
    costs   = as.numeric(costs),
    charges = as.numeric(charges))
#mean los by group disease
graph_mean_los_disease <- sparcs |>
  dplyr::group_by(apr_mdc) |>
  dplyr::summarise(mean_los = mean(los, na.rm = TRUE)) |>
  slice_max(mean_los, n = 15) |>
  dplyr::mutate(apr_mdc = fct_reorder(apr_mdc, mean_los)) |>
  ggplot(aes(x = apr_mdc, y = mean_los)) +
  geom_col(fill = "#378ADD") +
  geom_text(aes(label = round(mean_los, 1)),
            hjust = -0.2,
            size  = 3.0) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(title    = "Top 15 Disease Groups by Mean Length of Stay",
       subtitle = "SPARCS 2024",
       x        = NULL,
       y        = "Mean length of stay (days)") +
  theme_minimal(base_size = 8)

graph_mean_los_disease

#save
ggsave(filename = here("figure", "mean_los_disease.png"),
       plot = graph_mean_los_disease, dpi=600, width = 12, height=6)

#admissions by county
graph_admissions_county <- sparcs |> 
  count(hospital_county, name = "n") |> 
  slice_max(n, n = 20) |> 
  mutate(hospital_county = fct_reorder(hospital_county, n)) |> 
  ggplot(aes(x = hospital_county, y = n)) +
  geom_col(fill = "#BA7517") +
  geom_text(aes(label = comma(n)),
            hjust = -0.15, size = 3.2) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15)),
                     labels = comma) +
  labs(title    = "Top 20 Counties by Number of Admissions",
       subtitle = "SPARCS 2024",
       x        = NULL,
       y        = "Number of admissions") +
  theme_minimal(base_size = 12)

graph_admissions_county

#save
ggsave(filename = here("figure", "admissions_county.png"),
       plot = graph_admissions_county, dpi=600, width = 6, height=4.5)

#mean cost by payer
sparcs <- sparcs |>
  dplyr::mutate(
    payer_primary = forcats::fct_recode(payer_primary,
      "Not classified" = "NOT CLASSIFIED"))

graph_mean_payer <- sparcs |>
  dplyr::filter(payer_primary != "" & !is.na(payer_primary)) |>
  dplyr::group_by(payer_primary) |>
  dplyr::summarise(mean_cost = mean(costs, na.rm = TRUE),
                   n         = n()) |>
  dplyr::filter(n > 100) |>
  dplyr::mutate(payer_primary = fct_reorder(payer_primary, mean_cost)) |>
  ggplot(aes(x = payer_primary, y = mean_cost)) +
  geom_col(fill = "#993C1D") +
  geom_text(aes(label = dollar(round(mean_cost))),
            hjust = -0.15, size = 3.2) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.20)),
                     labels = dollar) +
  labs(title    = "Mean Cost per Admission by Primary Payer",
       subtitle = "SPARCS 2024 — payers with more than 100 admissions",
       x        = NULL,
       y        = "Mean cost (USD)") +
  theme_minimal(base_size = 12)

graph_mean_payer

#save
ggsave(filename = here("figure", "mean_payer.png"),
       plot = graph_mean_payer, dpi=600, width = 6, height=4.5)

