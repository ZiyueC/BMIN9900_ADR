# Adverse Drug Reaction (ADR) Prediction Using EHR and Machine Learning  
*MBMI Capstone Project – Chen Ziyue*

---

## 🧠 Project Overview

This project investigates the detection and prediction of adverse drug reactions (ADRs) using structured electronic health record (EHR) data and machine learning (ML) techniques. The primary data source is the MIMIC-IV v3.1 ICU database. ADRs were inferred based on hospital readmissions involving cardiovascular complications after administration of high-risk medications. A multi-step pipeline was developed to extract, clean, and model this information using SQL and R.

This work was completed as part of the capstone requirement for the **Master of Biomedical Informatics (MBMI)** program.

---

## 📂 Repository Structure

```plaintext
├── /data/                    # Input CSV datasets for modeling and analysis
│   ├── df_cohort_first_admission_medication.csv
│   ├── df_cohort_readmission_labeled_ADR.csv
│   └── df_cohort_modeling_ready.csv

├── /code/R_code/              # Final cleaned and commented R scripts
│   ├── 1.Table_1_EDA.R
│   ├── 2.Regression.R
│   ├── 3.PCA_Clustering.R
│   └── 4.Predicitive_Modelling.R

├── /code/SQL_code/            # Final SQL scripts with cohort construction logic
│   ├── 1. Medication and Pharmacy.sql
│   ├── 2a. First Admission Variables.sql
│   ├── 2b. Admissions and ICDs.sql
│   └── 3. Re Admission Variables.sql

└── README.md                     # This file
