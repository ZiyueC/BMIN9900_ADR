library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(dplyr)

# Load dataset
cohort_data = read.csv("/data/csv/df_cohort_first_admission_with_medication.csv")
# Load dataset
df_re = read.csv("/data/csv/df_cohort_re_admission.csv")
# some sanity check
print(all(df_re$subject_id %in% cohort_data$subject_id))

# Generate frequency table
table(cohort_data$first_expire)

# calculate missing ratio
print(colnames(cohort_data))

colSums(is.na(cohort_data)) / nrow(cohort_data)

missing_ratio <- cohort_data |> 
# Summarize data
  summarise(across(everything(), ~ if (is.character(.)) {
    mean(is.na(.) | . == "")
  } else {
    mean(is.na(.))
  })) |> 
  pivot_longer(everything(), names_to = "column", values_to = "missing_ratio") |> 
  arrange(desc(missing_ratio))

# add ADR binary
cohort_data <- cohort_data |> 
# Modify or select variables
  mutate(ADR = if_else(subject_id %in% df_re$subject_id, 1, 0))
# Generate frequency table
table(cohort_data$ADR)

## The EDA and Table 1 part
library(table1)
## Some basic preprocessing and cleaning
cohort_data$Gender <- factor(cohort_data$gender, levels = c("F", "M"), labels = c("Female", "Male"))

cohort_data$Language <- ifelse(cohort_data$language == "English", "English", "Non-English")

cohort_data$Race <- ifelse(cohort_data$race == "WHITE", "White", "Non-White")

cohort_data$Age <- cohort_data$age

cohort_data$ADR <- factor(cohort_data$ADR, levels = c(0, 1),
                       labels = c("No ADR", "ADR during re-admission"))

cohort_data$marital_status <- case_when(
  toupper(cohort_data$marital_status) == "MARRIED" ~ "Married",
  TRUE ~ "Non-Married"
)

cohort_data$marital_status <- factor(cohort_data$marital_status, levels = c("Married", "Non-Married"))

cohort_data$insurance_type <- case_when(
  str_detect(tolower(cohort_data$insurance_type), "medicaid") ~ "Medicaid",
  str_detect(tolower(cohort_data$insurance_type), "medicare") ~ "Medicare",
  str_detect(tolower(cohort_data$insurance_type), "private") ~ "Private",
  TRUE ~ "Others"
)


cohort_data$insurance_type <- factor(cohort_data$insurance_type, levels = c("Medicaid", "Medicare", "Private", "Others"))

used_vars <- names(cohort_data)[grepl("^used_", names(cohort_data))]

cohort_data[used_vars] <- lapply(cohort_data[used_vars], function(x) {
  factor(x, levels = c(0, 1), labels = c("Not used", "Used"))
})

cohort_data$ADR <- factor(cohort_data$ADR, levels = c("ADR during re-admission", "No ADR"))
used_vars <- names(cohort_data)[grepl("^used_", names(cohort_data))]

cohort_data[used_vars] <- lapply(cohort_data[used_vars], function(x) {
  factor(x, levels = c("Used", "Not used"))
})
cohort_data$Gender <- factor(cohort_data$Gender, levels = c("Male", "Female"))
cohort_data$Race <- factor(cohort_data$Race, levels = c("White", "Non-White"))

cohort_data <- cohort_data |> 
  rename(Admission_Length_of_Stay = adm_los)
cohort_data <- cohort_data |> 
  rename(Medication_Dosage = medication_dosage)
cohort_data <- cohort_data |> 
  rename(Marital_Status = marital_status)
cohort_data <- cohort_data |> 
  rename(Insurance_type = insurance_type)
## Create the p-value function used in Table 1

pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  
  if (is.numeric(y)) {
    # Use ANOVA for more than 2 groups
    if (nlevels(g) > 2) {
# View model summary
      p <- summary(aov(y ~ g))[[1]][["Pr(>F)"]][1]
    } else {
      p <- t.test(y ~ g)$p.value
    }
  } else {
    # Use chi-squared test (or Fisher's exact test if necessary)
# Generate frequency table
    tbl <- table(y, g)
    if (any(tbl < 5)) {
      p <- fisher.test(tbl)$p.value
    } else {
      p <- chisq.test(tbl)$p.value
    }
  }
  
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}
table1(~ Gender + Age + Race + Language  + Admission_Length_of_Stay +
         Medication_Dosage +
         Marital_Status + Insurance_type +
         used_warfarin + used_ssri_snri + used_ppi + used_other_statins +
         used_nsaid + used_diuretic + used_clopidogrel + used_beta_blocker +
         used_anticoagulant + used_ace_arb   + hosp_admission_type
       | ADR, 
       data = cohort_data,
       render.missing = NULL,
       extra.col=list(`P-value`=pvalue),
       topclass = "Rtable1-grid Rtable1-shade Rtable1-times",
       overall = "Overall")



# Load dataset
df_re = read.csv("/data/csv/df_cohort_re_admission.csv")
missing_ratio <- df_re |> 
# Summarize data
  summarise(across(everything(), ~ if (is.character(.)) {
    mean(is.na(.) | . == "")
  } else {
    mean(is.na(.))
  })) |> 
  pivot_longer(everything(), names_to = "column", values_to = "missing_ratio") |> 
  arrange(desc(missing_ratio))


binary_vars <- c("had_clotting_event", "had_gi_bleed", "had_other_bleed", "had_cardio_complication")

df_re[binary_vars] <- lapply(df_re[binary_vars], function(x) {
  factor(x, levels = c(1, 0), labels = c("Yes", "No"))
})

df_re$hospital_expire_flag <- factor(
  df_re$hospital_expire_flag,
  levels = c(0, 1),
  labels = c("Survived", "Died")
)

df_re <- df_re |> 
  rename(
    Re_Admission_Length_of_Stay = los,
    Days_after_First_Medication = days_to_re_adm,
    Clotting_Event = had_clotting_event,
    GI_Bleed = had_gi_bleed,
    Other_Bleed = had_other_bleed,
    Cardio_Complication = had_cardio_complication
  )

df_re$hospital_expire_flag <- factor(
  df_re$hospital_expire_flag,
  levels = c("Died", "Survived")
)


table1(~ Re_Admission_Length_of_Stay + Days_after_First_Medication +
         Clotting_Event + GI_Bleed + Other_Bleed + Cardio_Complication
       | hospital_expire_flag,
       data = df_re,
       render.missing = NULL,
       topclass = "Rtable1-grid Rtable1-shade Rtable1-times",
       overall = "Overall",
       extra.col = list(`P-value` = pvalue))
