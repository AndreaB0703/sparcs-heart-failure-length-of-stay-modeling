# Modeling Length of Stay Among Patients Hospitalized for Heart Failure Using SPARCS Data

This repository presents an analysis of hospital length of stay (LOS) among patients hospitalized for heart failure using data from the New York State **SPARCS (Statewide Planning and Research Cooperative System)** database.

Heart failure hospitalizations are commonly considered **Ambulatory Care Sensitive Conditions (ACSC)**, meaning that effective primary care and disease management may help prevent complications and reduce hospitalizations.

The goal of this project is to demonstrate a **principled workflow for statistical model selection in health services research**, integrating causal reasoning, hierarchical data structure, and empirical model diagnostics using **R**.

---

# Data Source

Data come from the **New York State SPARCS inpatient database**.

The original dataset contains:

* **1,048,575 hospital admissions**
* **30 variables**

These data include information on patient demographics, diagnoses, procedures, payer type, admission characteristics, and hospital identifiers.

---

# Data Preparation

For this study, the dataset was restricted to hospitalizations with a **primary diagnosis of heart failure**, resulting in an analytical dataset containing:

* **27,925 hospitalizations**
* **178 hospitals**

Several derived variables were created to facilitate the analysis.

Key preparation steps included:

* identifying hospitalizations with primary diagnosis of heart failure
* creating categorical variables for age group, payer type, and admission type
* defining procedure groups based on clinical procedure categories
* using **APR severity of illness** as a measure of clinical severity
* preparing variables for mixed-effects modeling

All data preparation steps are documented in the script **script/script_01_data_cleaning.R**.

---

# Research Question

**What patient and hospital-level factors are associated with length of stay among patients hospitalized for heart failure?**

---

# Analytical Workflow

The analysis followed four main steps.

---

# 1. Causal Structure (DAG)

A **Directed Acyclic Graph (DAG)** was used to clarify relationships between patient characteristics, clinical severity, procedures, admission type, and hospital-level factors.

The DAG guided variable selection and helped avoid inappropriate statistical adjustment.

![Causal DAG](figure/dag_structure.png)

---

# 2. Data Structure

Patients are **nested within hospitals**, meaning observations are not statistically independent.

To account for this hierarchical structure, a **mixed-effects model with a random intercept for hospital** was used.

---

# 3. Candidate Models

Length of stay is a variable that is:

* continuous
* strictly positive
* strongly right-skewed

Exploratory analysis confirmed the skewed distribution of LOS.

![Distribution of Length of Stay](figure/los_distribution.png)

To explore plausible statistical distributions, a **Cullen–Frey plot** was used.

![Cullen-Frey Plot](output/Cullen_Frey_los.png)

Based on these diagnostics, two candidate models were evaluated:

* **Gamma mixed-effects model**
* **Log-normal mixed-effects model**

---

# 4. Model Comparison and Diagnostics

Candidate models were compared using:

* **Akaike Information Criterion (AIC)**
* **Residual diagnostics**

The **Gamma mixed-effects model with log link** provided the best fit to the LOS distribution.

---

# Final Model

The final specification was a **Gamma mixed-effects model with log link**, including:

* age group
* sex
* race
* payer type
* clinical severity (APR severity)
* procedure type
* admission type
* random intercept for hospital

An **interaction between clinical severity and procedure type** was included in the final model.

---

# Key Findings

## Clinical Severity and Procedures

Model-adjusted marginal means indicated that **length of stay increases with clinical severity**, and that this association **varies according to procedure type**.

For patients **without procedures**, the estimated LOS increased progressively with severity:

| APR Severity | Estimated LOS (days) |
| ------------ | -------------------- |
| Minor        | 2.73                 |
| Moderate     | 3.71                 |
| Major        | 4.99                 |
| Extreme      | 6.83                 |

However, when **diagnostic procedures** were performed, LOS increased substantially:

| APR Severity | Estimated LOS with Diagnostic Procedures (days) |
| ------------ | ----------------------------------------------- |
| Minor        | 3.30                                            |
| Moderate     | 4.80                                            |
| Major        | 7.40                                            |
| Extreme      | 12.62                                           |

These results indicate that **procedure use amplifies the association between clinical severity and hospital length of stay**.

In particular, patients with **extreme severity undergoing diagnostic procedures** had an estimated LOS of **approximately 12.6 days**, compared with **6.8 days among patients with extreme severity who did not undergo procedures**.

![Interaction Between Severity and Procedures](output/interaction_procedure.png)

---

## Hospital-Level Variation

Approximately **10% of the variation in length of stay** was attributable to differences between hospitals, indicating meaningful hospital-level variation in LOS.

---

# Reproducibility

All analyses were conducted in **R**, using the following packages:

* `lme4`
* `emmeans`
* `DHARMa`
* `ggplot2`

---

# Repository Structure

data/
figure/
output/
script/
README.md

---

# Author

**Andrea B.**
PhD in Epidemiology
Master of Public Health (MPH)
Bachelor of Science in Nursing (BSN)
