# Modeling Length of Stay Among Patients Hospitalized for Heart Failure Using SPARCS Data

This project analyzes **hospital length of stay (LOS) among patients hospitalized for heart failure** using SPARCS data (~28,000 admissions across 178 hospitals).

Heart failure hospitalizations are considered **Ambulatory Care Sensitive Conditions (ACSC)**, meaning that effective primary care and disease management may help prevent complications and reduce hospitalizations.

The objective of this project is to illustrate a **principled workflow for statistical model selection in health services research**, integrating causal reasoning, data structure, and empirical model diagnostics using R software. 

---

# Data Source

Data come from the **SPARCS (Statewide Planning and Research Cooperative System)** database from New York State.

The original dataset contains **1,048,575 hospital admissions and 30 variables**.

---

# Data Preparation

For this study, the dataset was restricted to **hospitalizations with a primary diagnosis of heart failure**, resulting in an analytical dataset with **27,925 observations across 178 hospitals**, while retaining the same 30 variables.

Key data preparation steps included:

* Identifying hospitalizations with **primary diagnosis of heart failure**
* Creating categorical variables for **age group, payer type, and admission type**
* Defining **procedure groups** based on clinical categories
* Using **APR severity** as a measure of clinical severity
* Preparing variables for mixed-effects modeling

All data preparation steps are documented in the script:

```
scripts/01_data_cleaning.R
```

---

# Research Question

Which patient and hospital factors are associated with **hospital length of stay among patients hospitalized for heart failure**?

---

# Analytical Workflow

The analysis followed four main steps.

## 1. Causal Structure (DAG)

A **Directed Acyclic Graph (DAG)** was used to clarify relationships between:

* patient characteristics
* clinical severity
* procedures
* admission type
* hospital-level factors

The DAG guided variable selection and helped avoid inappropriate adjustment.

---

## 2. Data Structure

Patients are **nested within hospitals**, meaning that observations are not independent.

To account for this hierarchical structure, a **mixed-effects model with a random intercept for hospital** was used.

---

## 3. Candidate Models

Length of stay is:

* continuous
* strictly positive
* strongly right-skewed

Therefore, the following models were evaluated:

* Gamma mixed model
* Log-normal model

---

# Distribution of Length of Stay

The distribution of LOS shows strong right skewness.

![LOS Distribution](figures/histogram_los.png)

---

# Candidate Distributions

A Cullen–Frey plot was used to explore plausible distributions for LOS.

![Cullen-Frey Plot](figures/Cullen_Frey_los.png)

---

# Model Comparison and Diagnostics

Candidate models were compared using:

* **Akaike Information Criterion (AIC)**
* **Residual diagnostics**

The **Gamma mixed-effects model with a log link** provided the best fit for the LOS distribution.

---

# Final Model

The final specification was a **Gamma mixed-effects model with a log link**, including:

* patient characteristics
* clinical severity (APR severity)
* procedure type
* admission type
* random intercept for hospital

An **interaction between clinical severity and procedure type** was included in the final model.

---

# Key Findings

### Clinical Severity

Clinical severity was the **strongest predictor of LOS**.

Mean LOS by severity level:

| APR Severity | Mean LOS (days) |
| ------------ | --------------- |
| Minor        | 3.15            |
| Moderate     | 4.31            |
| Major        | 6.13            |
| Extreme      | 10.32           |

---

### Procedure Type

Procedure type was also associated with LOS.

| Procedure Type              | Mean LOS (days) |
| --------------------------- | --------------- |
| No procedure                | 4.50            |
| Diagnostic procedures       | 6.38            |
| Cardiac procedures          | 5.59            |
| Respiratory / organ support | 5.33            |

---

# Interaction Between Severity and Procedures

The final model revealed an **interaction between clinical severity and procedure type**.

Among patients with **extreme clinical severity**, those undergoing diagnostic procedures stayed on average **5.79 additional days** in the hospital.

![Interaction Plot](figures/interaction_procedure.png)

---

# Hospital-Level Variation

Approximately **10% of the variation in LOS** was attributable to differences between hospitals.

---

# Reproducibility

All analyses were conducted in **R** using the following packages:

* `lme4`
* `emmeans`
* `DHARMa`
* `ggplot2`

---

# Repository Structure

```
data/
scripts/
figures/
results/
README.md
```

---

# Next Steps

Future updates will include:

* DAG visualization
* detailed model diagnostics
* a fully reproducible analysis pipeline
