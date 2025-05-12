# Load dataset
cohort_data = read.csv("/data/csv/df_cohort_modeling_ready.csv")

library(table1)
library(stringr)
library(tidyverse)
library(dplyr)

#cohort_data = df_first_temp
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

library(pROC)
library(PRROC)
set.seed(123)

predictors <- c("Gender", "Age", "Race", "Language", "Admission_Length_of_Stay",
                "Medication_Dosage", "Marital_Status", "Insurance_type",
                "used_warfarin", "used_ssri_snri", "used_ppi", "used_other_statins",
                "used_nsaid", "used_diuretic", "used_clopidogrel", "used_beta_blocker",
                "used_anticoagulant", "used_ace_arb", "hosp_admission_type")

model_data <- cohort_data %>%
# Modify or select variables
  select(ADR, all_of(predictors)) %>%
  na.omit()

model_data$ADR <- factor(model_data$ADR, levels = c("ADR during re-admission", "No ADR"),
                       labels = c("ADR", "No_ADR"))

train_control <- trainControl(
  method = "cv",
  number = 5,
  summaryFunction = twoClassSummary,
  classProbs = TRUE,
  savePredictions = TRUE,
  sampling = "up"  # ⬅️ ADD upsampling
)

set.seed(123)
elastic_model <- train(
  ADR ~ ., 
  data = model_data,
  method = "glmnet",
  trControl = train_control,
  preProcess = c("center", "scale"),
  metric = "ROC",
  family = "binomial",
  tuneLength = 5
)

set.seed(123)
rf_model <- train(
  ADR ~ ., 
  data = model_data,
  method = "ranger",   # Faster RF
  trControl = train_control,
  metric = "ROC",
  tuneLength = 2,
  ntree = 100,
  importance = 'impurity'
)


elastic_preds <- elastic_model$pred
rf_preds <- rf_model$pred

best_elastic <- elastic_preds %>%
# Filter rows
  filter(lambda == elastic_model$bestTune$lambda,
         alpha == elastic_model$bestTune$alpha)

rf_best <- rf_preds %>%
# Filter rows
  filter(mtry == rf_model$bestTune$mtry)


elastic_roc <- roc(best_elastic$obs, best_elastic$ADR)
elastic_pr <- pr.curve(scores.class0 = best_elastic$ADR, 
                       weights.class1 = ifelse(best_elastic$obs == "ADR", 1, 0))


rf_roc <- roc(rf_best$obs, rf_best$ADR)
rf_pr <- pr.curve(scores.class0 = rf_best$ADR,
                  weights.class1 = ifelse(rf_best$obs == "ADR", 1, 0))


library(MLmetrics)

elastic_f1 <- F1_Score(y_pred = best_elastic$pred, y_true = best_elastic$obs, positive = "ADR")
elastic_acc <- mean(best_elastic$pred == best_elastic$obs)

rf_f1 <- F1_Score(y_pred = rf_best$pred, y_true = rf_best$obs, positive = "ADR")
rf_acc <- mean(rf_best$pred == rf_best$obs)

results <- tibble(
  Model = c("Elastic Net", "Random Forest"),
  AUROC = c(auc(elastic_roc), auc(rf_roc)),
  AUPRC = c(elastic_pr$auc.integral, rf_pr$auc.integral),
  F1 = c(elastic_f1, rf_f1),
  Accuracy = c(elastic_acc, rf_acc)
)

print(results)
library(pROC)
library(PRROC)
# Create a plot
library(ggplot2)
library(patchwork)

elastic_roc_obj <- roc(best_elastic$obs, best_elastic$ADR, levels = c("No_ADR", "ADR"))
rf_roc_obj <- roc(rf_best$obs, rf_best$ADR, levels = c("No_ADR", "ADR"))

elastic_pr_obj <- pr.curve(scores.class0 = best_elastic$ADR,
                           weights.class1 = ifelse(best_elastic$obs == "ADR", 1, 0),
                           curve = TRUE)

rf_pr_obj <- pr.curve(scores.class0 = rf_best$ADR,
                      weights.class1 = ifelse(rf_best$obs == "ADR", 1, 0),
                      curve = TRUE)

elastic_color <- "#1F77B4"  # professional blue
rf_color <- "#FF7F0E"       # professional orange

# Create a plot
plot_roc <- ggplot() +
  geom_line(aes(x = 1 - elastic_roc_obj$specificities, y = elastic_roc_obj$sensitivities), 
            color = elastic_color, size = 1.2) +
  geom_line(aes(x = 1 - rf_roc_obj$specificities, y = rf_roc_obj$sensitivities), 
            color = rf_color, size = 1.2) +
  labs(title = "ROC Curve",
       x = "1 - Specificity (False Positive Rate)",
       y = "Sensitivity (True Positive Rate)") +
  annotate("text", x = 0.65, y = 0.2, 
           label = paste0("Elastic Net AUROC: ", round(auc(elastic_roc_obj), 3)), 
           color = elastic_color, size = 5) +
  annotate("text", x = 0.65, y = 0.1, 
           label = paste0("Random Forest AUROC: ", round(auc(rf_roc_obj), 3)), 
           color = rf_color, size = 5) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# Create a plot
plot_pr <- ggplot() +
  geom_line(aes(x = elastic_pr_obj$curve[, 1], y = elastic_pr_obj$curve[, 2]), 
            color = elastic_color, size = 1.2) +
  geom_line(aes(x = rf_pr_obj$curve[, 1], y = rf_pr_obj$curve[, 2]), 
            color = rf_color, size = 1.2) +
  labs(title = "Precision-Recall Curve",
       x = "Recall",
       y = "Precision") +
  annotate("text", x = 0.5, y = 0.2, 
           label = paste0("Elastic Net AUPRC: ", round(elastic_pr_obj$auc.integral, 3)), 
           color = elastic_color, size = 5) +
  annotate("text", x = 0.5, y = 0.1, 
           label = paste0("Random Forest AUPRC: ", round(rf_pr_obj$auc.integral, 3)), 
           color = rf_color, size = 5) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

final_plot <- plot_roc + plot_pr
final_plot


elastic_coefs <- coef(elastic_model$finalModel, 
                      s = elastic_model$bestTune$lambda)

elastic_coefs_df <- as.data.frame(as.matrix(elastic_coefs))
elastic_coefs_df$Feature <- rownames(elastic_coefs_df)
colnames(elastic_coefs_df)[1] <- "Coefficient"

elastic_coefs_df <- elastic_coefs_df %>%
# Filter rows
  filter(Feature != "(Intercept)") %>%
# Modify or select variables
  mutate(Importance = abs(Coefficient)) %>%
  arrange(desc(Importance))

# Create a plot
library(ggplot2)

# Create a plot
ggplot(elastic_coefs_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_col(fill = "#2CA02C") +
  coord_flip() +
  labs(title = "Elastic Net Feature Importance (by Absolute Coefficient)",
       x = "Feature",
       y = "Absolute Coefficient") +
  theme_minimal(base_size = 14)


library(fastshap)


# Modify or select variables
dummy_vars <- dummyVars(" ~ .", data = model_data %>% select(-ADR))
# Make predictions using model
X_encoded <- predict(dummy_vars, newdata = model_data %>% select(-ADR)) %>% as.data.frame()

set.seed(123)

elastic_model <- train(
  x = X_encoded,
  y = model_data$ADR,
  method = "glmnet",
  trControl = train_control,
  preProcess = c("center", "scale"),
  metric = "ROC",
  family = "binomial",
  tuneLength = 5
)

elastic_pred <- function(object, newdata) {
# Make predictions using model
  predict(object, newx = as.matrix(newdata), s = elastic_model$bestTune$lambda, type = "response")[,1]
}

set.seed(123)
elastic_shap <- fastshap::explain(
  object = elastic_model$finalModel,
  feature_names = names(X_encoded),
  X = as.matrix(X_encoded),
  pred_wrapper = elastic_pred,
  nsim = 100
)

shap_mean_elastic <- as.data.frame(elastic_shap) %>%
  summarise_all(mean) %>%
  pivot_longer(cols = everything(), names_to = "Feature", values_to = "Mean_SHAP") %>%
  arrange(desc(abs(Mean_SHAP)))

shap_mean_elastic <- shap_mean_elastic %>%
# Filter rows
  filter(Feature != "ADR_binary")

# Create a plot
ggplot(shap_mean_elastic, aes(x = reorder(Feature, Mean_SHAP), y = Mean_SHAP)) +
  geom_col(fill = "#1F77B4") +
  coord_flip() +
  labs(title = "Elastic Net SHAP Feature Importance",
       x = "Feature",
       y = "Mean SHAP Value (Impact)") +
  theme_minimal(base_size = 14)

shap_top10 <- shap_mean_elastic %>%
  arrange(desc(abs(Mean_SHAP))) %>%
  slice(1:10)

# Create a plot
ggplot(shap_top10, aes(x = reorder(Feature, Mean_SHAP), y = Mean_SHAP)) +
  geom_col(fill = "#1F77B4") +
  coord_flip() +
  labs(title = "Top 10 Elastic Net SHAP Feature Importance",
       x = "Feature",
       y = "Mean SHAP Value (Impact)") +
  theme_minimal(base_size = 16)

### GINI

rf_varimp <- varImp(rf_model)


rf_varimp_df <- rf_varimp$importance %>%
  rownames_to_column(var = "Feature") %>%
  arrange(desc(Overall))

rf_varimp_df <- rf_varimp_df %>%
# Filter rows
  filter(Feature != "ADR_binary")

# Create a plot
ggplot(rf_varimp_df, aes(x = reorder(Feature, Overall), y = Overall)) +
  geom_col(fill = "#FF7F0E") +
  coord_flip() +
  labs(title = "Random Forest GINI Feature Importance",
       x = "Feature",
       y = "Importance (Mean Decrease in GINI)") +
  theme_minimal(base_size = 16)


