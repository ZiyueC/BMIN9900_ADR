# Univariate/pairwise Logistic Regression
vars <- c(
  "Medication_Dosage", "Age", "Gender", "Race", "Language",
  "Insurance_type", "Marital_Status", "Admission_Length_of_Stay",
  "used_warfarin", "used_ssri_snri", "used_ppi", "used_other_statins",
  "used_nsaid", "used_diuretic", "used_clopidogrel", "used_beta_blocker",
  "used_anticoagulant", "used_ace_arb"
)

results <- lapply(vars, function(var) {
  formula <- as.formula(paste("ADR ~", var))
# Fit a regression model
  model <- glm(formula, data = cohort_data, family = binomial)
# View model summary
  summary <- summary(model)$coefficients
  odds_ratio <- exp(coef(model)[2])
  ci <- exp(confint(model)[2, ])
  pval <- summary[2, 4]
  
  data.frame(
    Variable = var,
    OR = round(odds_ratio, 3),
    CI_Lower = round(ci[1], 3),
    CI_Upper = round(ci[2], 3),
    P_Value = signif(pval, 3)
  )
})
univariate_results <- do.call(rbind, results)
print(univariate_results)
univariate_results$Variable <- factor(univariate_results$Variable, levels = rev(univariate_results$Variable))
univariate_results$p_text <- ifelse(univariate_results$P_Value < 0.001, "< 0.001", formatC(univariate_results$P_Value, format = "f", digits = 3))

# Create a plot
ggplot(univariate_results, aes(x = OR, y = Variable)) +
  geom_point(size = 4, color = "#0072B2") +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, color = "#0072B2", linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "darkgray", linewidth = 0.8) +
  geom_text(aes(label = paste0("p = ", p_text)), hjust = -0.15, size = 5, family = "sans") +
  scale_x_continuous(trans = "log", breaks = c(0.1, 0.5, 1, 2, 5, 10), limits = c(0.1, 10)) +
  xlab("Odds Ratio (log scale)") +
  ylab("") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 14, family = "sans"),
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
    plot.margin = margin(10, 30, 10, 10)
  )
## Next Lasso regression

cohort_data$ADR <- ifelse(cohort_data$ADR == "ADR during re-admission", 1, 0)
# Generate frequency table
table(cohort_data$ADR)


lasso_vars <- c(
  "Medication_Dosage", "Age", "Gender", "Race", "Insurance_type",
  "Marital_Status", "Admission_Length_of_Stay",
  "used_warfarin", "used_ppi", "used_other_statins",
  "used_nsaid", "used_diuretic", "used_clopidogrel",
  "used_anticoagulant", "used_ace_arb"
)

df_lasso <- cohort_data[, c("ADR", lasso_vars)]
df_lasso <- na.omit(df_lasso)

categorical_vars <- c(
  "Gender", "Race", "Insurance_type", "Marital_Status",
  "used_warfarin", "used_ppi", "used_other_statins",
  "used_nsaid", "used_diuretic", "used_clopidogrel",
  "used_anticoagulant", "used_ace_arb"
)
df_lasso[categorical_vars] <- lapply(df_lasso[categorical_vars], as.factor)

x <- model.matrix(ADR ~ ., data = df_lasso)  # -1 removes intercept
y <- df_lasso$ADR

library(glmnet)

cvfit <- cv.glmnet(x, y, alpha = 1, family = "binomial")
plot(cvfit)

lasso_coef <- coef(cvfit, s = "lambda.min")
selected_vars <- rownames(lasso_coef)[lasso_coef[, 1] != 0]
selected_vars

plot(cvfit$glmnet.fit, xvar = "lambda", label = TRUE)
abline(v = log(cvfit$lambda.min), lty = 2, col = "red")

coef_df <- as.data.frame(as.matrix(lasso_coef))
coef_df <- coef_df[coef_df[,1] != 0, , drop = FALSE]
coef_df <- coef_df[-1, , drop = FALSE]  # remove intercept
coef_df$Feature <- rownames(coef_df)


# Create a plot
ggplot(coef_df, aes(x = reorder(Feature, s1), y = s1)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  ylab("LASSO Coefficient") +
  xlab("") +
  theme_minimal(base_size = 14)

## Step 3: AIC with Stepwise Logistic Regression

final_vars <- c(
  "Medication_Dosage", "Age", "Gender", "Race", "Insurance_type",
  "Marital_Status", "Admission_Length_of_Stay", "used_warfarin",
  "used_ppi", "used_other_statins", "used_nsaid", "used_diuretic",
  "used_clopidogrel", "used_anticoagulant", "used_ace_arb"
)

df_reg <- cohort_data[, c("ADR", final_vars)]
df_reg <- na.omit(df_reg)

categorical_vars <- c(
  "Gender", "Race", "Insurance_type", "Marital_Status",
  "used_warfarin", "used_ppi", "used_other_statins",
  "used_nsaid", "used_diuretic", "used_clopidogrel",
  "used_anticoagulant", "used_ace_arb"
)

df_reg[categorical_vars] <- lapply(df_reg[categorical_vars], as.factor)
full_formula <- as.formula(paste("ADR ~", paste(final_vars, collapse = " + ")))
# Fit a regression model
full_model <- glm(full_formula, data = df_reg, family = binomial)

library(MASS)
step_model <- stepAIC(full_model, direction = "both", trace = FALSE)
# View model summary
summary(step_model)

# View model summary
step_summary <- summary(step_model)

# View model summary
coefs <- coef(summary(step_model))
ORs <- exp(coefs[, "Estimate"])
CIs <- exp(confint(step_model))
pvals <- coefs[, "Pr(>|z|)"]
# Build the final results table
final_results <- data.frame(
  Variable = rownames(coefs),
  OR = round(ORs, 3),
  CI_Lower = round(CIs[, 1], 3),
  CI_Upper = round(CIs[, 2], 3),
  P_Value = signif(pvals, 3)
)

final_results <- final_results[final_results$Variable != "(Intercept)", ]
print(final_results)

# Create a plot
library(ggplot2)


final_results$Variable <- factor(final_results$Variable, levels = rev(final_results$Variable))

final_results$p_text <- ifelse(final_results$P_Value < 0.001, "< 0.001",
                               formatC(final_results$P_Value, format = "f", digits = 3))

# Plot
# Create a plot
ggplot(final_results, aes(x = OR, y = Variable)) +
  geom_point(size = 4, color = "#0072B2") +
  geom_errorbarh(aes(xmin = CI_Lower, xmax = CI_Upper), height = 0.2, color = "#0072B2", linewidth = 1) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "gray", linewidth = 0.8) +
  geom_text(aes(label = paste0("p = ", p_text)), hjust = -0.15, size = 5) +
  scale_x_continuous(trans = "log", breaks = c(0.5, 1, 2, 3, 4), limits = c(min(final_results$CI_Lower)*0.9, max(final_results$CI_Upper)*1.2)) +
  xlab("Adjusted Odds Ratio (log scale)") +
  ylab("") +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text = element_text(size = 14),
    axis.title.x = element_text(size = 16, face = "bold", margin = margin(t = 10)),
    plot.margin = margin(10, 30, 10, 10)
  )

print(AIC(step_model))

vars_formula <- attr(terms(step_model), "term.labels")

base_aic <- AIC(step_model)


safe_vars <- paste0("`", vars_formula, "`")

aic_results <- sapply(seq_along(safe_vars), function(i) {
  reduced_formula <- as.formula(
    paste("ADR ~", paste(safe_vars[-i], collapse = " + "))
  )
# Fit a regression model
  model <- glm(reduced_formula, data = df_reg, family = binomial)
  AIC(model)
})

delta_aic <- aic_results - base_aic

aic_table <- data.frame(
  Variable = vars_formula,
  AIC_Without = round(aic_results, 2),
  Delta_AIC = round(delta_aic, 2)
)

aic_table <- aic_table[order(-aic_table$Delta_AIC), ]
print(aic_table)

## step 4, final table for regression

library(knitr)
library(kableExtra)

table2 <- final_results |>
# Filter rows
  filter(Variable != "(Intercept)") |>
# Modify or select variables
  mutate(
    OR_CI = paste0(OR, " (", CI_Lower, "–", CI_Upper, ")"),
    p_numeric = as.numeric(ifelse(P_Value == "< 0.001", 0.0009, P_Value)),
    P_Value = ifelse(P_Value < 0.001, "< 0.001", formatC(P_Value, digits = 3, format = "f"))
  ) |>
  arrange(p_numeric) |>
# Modify or select variables
  dplyr::select(Variable, OR_CI, P_Value)

# Display Table 2
kable(table2, format = "html", escape = FALSE,
      col.names = c("Variable", "Adjusted OR (95% CI)", "P-value")) |>
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, position = "center") |>
  column_spec(1, bold = TRUE)





