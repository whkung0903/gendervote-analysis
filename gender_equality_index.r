## =========================================================
## Purpose: Build Gender Equality Index, reliability, models, and EDA
## Data   : data/survey_data.dta (same as main analysis script)
## =========================================================

## ---------------------------
## 0) Load packages
## ---------------------------
suppressPackageStartupMessages({
  library(haven)        # read_dta
  library(dplyr)        # data wrangling
  library(tidyr)        # tidying
  library(ggplot2)      # plotting
  library(psych)        # Cronbach's alpha
  library(openxlsx)     # Excel export
  library(scales)       # percent_format, etc.
  library(car)          # vif
  library(MASS)         # boxcox
  library(lmtest)       # dwtest
  library(moments)      # skewness/kurtosis
  library(pscl)         # pR2
  library(randomForest) # random forest
  library(caret)        # model tuning
  library(lattice)      # required by caret plots
  library(ranger)       # fast RF backend for caret
})

theme_set(theme_minimal())

## ---------------------------
## 1) Read raw data (same path as the 700+ line script)
## ---------------------------
raw_data <- read_dta("data/survey_data.dta")

## ---------------------------
## 2) Select items for Gender Equality Index
## ---------------------------
selected_questions <- c(
  "e1a","e1b","e1c","e1d",
  "e2a","e2b","e6c","e8c","e8d",
  "f2a","f2c","j2a","j2b","j10a",
  "j10b","j10c","j10d","j10g","j10h"
)

selected_data <- raw_data[, selected_questions, drop = FALSE]

## ---------------------------
## 3) Scoring function (recode to 1~5; handle special and missing codes)
## ---------------------------
convert_scores <- function(column, question) {
  case_when(
    ## Reverse-keyed to pro-equality (1->5 ... 5->1)
    question %in% c("e1a","e2a","j2b","j10g","j10h","e6c","e8c","e8d") & column == 1 ~ 5,
    question %in% c("e1a","e2a","j2b","j10g","j10h","e6c","e8c","e8d") & column == 2 ~ 4,
    question %in% c("e1a","e2a","j2b","j10g","j10h","e6c","e8c","e8d") & column == 3 ~ 3,
    question %in% c("e1a","e2a","j2b","j10g","j10h","e6c","e8c","e8d") & column == 4 ~ 2,
    question %in% c("e1a","e2a","j2b","j10g","j10h","e6c","e8c","e8d") & column == 5 ~ 1,

    ## Straight-keyed (1->1 ... 5->5)
    question %in% c("e1b","e1c","e1d","e2b","j2a","j10a","j10b","j10c","j10d") & column == 1 ~ 1,
    question %in% c("e1b","e1c","e1d","e2b","j2a","j10a","j10b","j10c","j10d") & column == 2 ~ 2,
    question %in% c("e1b","e1c","e1d","e2b","j2a","j10a","j10b","j10c","j10d") & column == 3 ~ 3,
    question %in% c("e1b","e1c","e1d","e2b","j2a","j10a","j10b","j10c","j10d") & column == 4 ~ 4,
    question %in% c("e1b","e1c","e1d","e2b","j2a","j10a","j10b","j10c","j10d") & column == 5 ~ 5,

    ## Special mapping for f2a, f2c (4->5, 3->4, 5->3, 2->2, 1->1)
    question %in% c("f2a","f2c") & column == 4 ~ 5,
    question %in% c("f2a","f2c") & column == 3 ~ 4,
    question %in% c("f2a","f2c") & column == 5 ~ 3,
    question %in% c("f2a","f2c") & column == 2 ~ 2,
    question %in% c("f2a","f2c") & column == 1 ~ 1,

    ## Common missing codes (adjust if needed)
    column %in% c(94, 97, 98, 999) ~ NA_real_,

    TRUE ~ NA_real_
  )
}

## Apply to all selected items
scored_data <- selected_data %>%
  mutate(across(everything(), ~ convert_scores(.x, cur_column())))

## ---------------------------
## 4) Reliability: Cronbach's Alpha
## ---------------------------
alpha_input <- na.omit(scored_data)       # alpha requires complete cases
alpha_result <- psych::alpha(alpha_input) # full output
print(alpha_result$total$raw_alpha)

## Correlation matrix of items (pairwise)
cor_matrix <- cor(scored_data, use = "pairwise.complete.obs")
print(round(cor_matrix, 3))

## ---------------------------
## 5) Save alpha outputs to Excel
## ---------------------------
if (!dir.exists("results")) dir.create("results")
wb <- createWorkbook()
output_file <- "results/Cronbach_Alpha_Results.xlsx"

## Summary sheet (pull key stats from alpha_result)
alpha_summary <- data.frame(
  Raw_Alpha            = unname(alpha_result$total$raw_alpha),
  Standardized_Alpha   = unname(alpha_result$total$std.alpha),
  G6_SMC               = unname(alpha_result$total$G6.smc),
  Average_R            = unname(alpha_result$total$average.r),
  Signal_to_Noise      = unname(alpha_result$total$S.N),
  ASE                  = unname(alpha_result$total$ase),
  Mean                 = unname(alpha_result$total$mean),
  SD                   = unname(alpha_result$total$sd),
  Median_R             = unname(alpha_result$total$median.r),
  Feldt_Lower          = unname(alpha_result$total$lower),
  Feldt_Upper          = unname(alpha_result$total$upper)
)

addWorksheet(wb, "Alpha Summary")
writeData(wb, "Alpha Summary", alpha_summary)

## Item statistics & alpha-if-item-dropped
item_statistics <- as.data.frame(alpha_result$item.stats)
item_alpha_drop <- as.data.frame(alpha_result$alpha.drop)

addWorksheet(wb, "Item Statistics")
writeData(wb, "Item Statistics", item_statistics)

addWorksheet(wb, "Alpha Drop")
writeData(wb, "Alpha Drop", item_alpha_drop)

saveWorkbook(wb, output_file, overwrite = TRUE)

## ---------------------------
## 6) Build Gender Equality Index (row mean of scored items)
## ---------------------------
data_with_index <- raw_data
data_with_index$gender_index <- rowMeans(scored_data, na.rm = TRUE)

## ---------------------------
## 7) Interaction: a2y and f3_group
## ---------------------------
processed_a2y <- data_with_index %>%
  select(a2y, f3, gender_index) %>%
  filter(!is.na(a2y), !is.na(f3), !is.na(gender_index)) %>%
  filter(a2y <= 100) %>%
  mutate(
    f3_group = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      TRUE    ~ "Other"
    )
  ) %>%
  filter(f3_group != "Other") %>%
  mutate(f3_group = factor(f3_group, levels = c("Sung","Han","Tsai")))

model_a2y_inter <- lm(gender_index ~ a2y * f3_group, data = processed_a2y)
summary(model_a2y_inter)
anova(model_a2y_inter)

p_inter_a2y <- ggplot(processed_a2y, aes(x = a2y, y = gender_index, color = f3_group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Interaction: Birth Year (a2y) × Political Preference on Gender Index",
    x = "Birth Year (a2y code)",
    y = "Gender Equality Index"
  )
print(p_inter_a2y)

## ---------------------------
## 8) Interaction: b2 and f3_group
## ---------------------------
processed_b2 <- data_with_index %>%
  select(b2, f3, gender_index) %>%
  filter(!is.na(b2), !is.na(f3), !is.na(gender_index)) %>%
  filter(b2 <= 30) %>%
  mutate(
    f3_group = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      TRUE    ~ "Other"
    )
  ) %>%
  filter(f3_group != "Other") %>%
  mutate(f3_group = factor(f3_group, levels = c("Sung","Han","Tsai")))

model_b2_inter <- lm(gender_index ~ b2 * f3_group, data = processed_b2)
summary(model_b2_inter)
anova(model_b2_inter)

p_inter_b2 <- ggplot(processed_b2, aes(x = b2, y = gender_index, color = f3_group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Interaction: Education Years (b2) × Political Preference on Gender Index",
    x = "Years of Education (b2)",
    y = "Gender Equality Index"
  )
print(p_inter_b2)

## ---------------------------
## 9) EDA — Categorical ANOVA
## ---------------------------
categorical_vars <- c(
  "a1","a4","a5","a6","a7","a8","a9","a10","a11","a12","a13",
  "a15","a17","b1","b3","b4","b5","c4","f3","j12","k3","k4","k5"
)

factor_data <- data_with_index[, c(categorical_vars, "gender_index")]

## Grouping for education of self/parents (b1,b3,b4)
factor_data <- factor_data %>%
  mutate(
    b1_group = case_when(
      b1 %in%  1:5  ~ "JuniorHS_or_below",
      b1 %in%  6:8  ~ "HighSchool",
      b1 %in%  9:19 ~ "College",
      b1 %in% 20:21 ~ "Graduate",
      b1 == 22      ~ "Other",
      TRUE          ~ NA_character_
    ),
    b3_group = case_when(
      b3 %in%  1:5  ~ "JuniorHS_or_below",
      b3 %in%  6:8  ~ "HighSchool",
      b3 %in%  9:19 ~ "College",
      b3 %in% 20:21 ~ "Graduate",
      b3 == 22      ~ "Other",
      TRUE          ~ NA_character_
    ),
    b4_group = case_when(
      b4 %in%  1:5  ~ "JuniorHS_or_below",
      b4 %in%  6:8  ~ "HighSchool",
      b4 %in%  9:19 ~ "College",
      b4 %in% 20:21 ~ "Graduate",
      b4 == 22      ~ "Other",
      TRUE          ~ NA_character_
    ),
    f3_group = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      f3 %in% 4:7 ~ "Other",
      TRUE        ~ NA_character_
    )
  ) %>%
  mutate(
    b1_group = factor(b1_group),
    b3_group = factor(b3_group),
    b4_group = factor(b4_group),
    f3_group = factor(f3_group, levels = c("Sung","Han","Tsai","Other"))
  )

## One-way ANOVA for each categorical var (as factor)
anova_results_cat <- lapply(categorical_vars, function(v) {
  form <- as.formula(paste0("gender_index ~ as.factor(", v, ")"))
  fit  <- aov(form, data = factor_data)
  list(var = v, summary = summary(fit))
})
# Print summaries
for (res in anova_results_cat) {
  cat("\n=== ANOVA (categorical) —", res$var, "===\n")
  print(res$summary)
}

## ---------------------------
## 10) EDA — Continuous correlation with gender_index
## ---------------------------
continuous_vars <- c("a2y","a18","b2","c3g","c3h","c7g","c7h","d1a","k1")
cont_df <- data_with_index[, c(continuous_vars, "gender_index")]

## Make sure numeric
cont_df[continuous_vars] <- lapply(cont_df[continuous_vars], function(x) suppressWarnings(as.numeric(x)))

## Outlier helper (IQR rule) — optional diagnostics
check_outliers <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  which(x < (Q1 - 1.5*IQR) | x > (Q3 + 1.5*IQR))
}
outlier_indices <- lapply(cont_df[continuous_vars], check_outliers)
# Example of capping extreme silly codes (keep NA if absurd)
cont_df$a2y[cont_df$a2y > 900] <- NA
cont_df$b2[cont_df$b2 >  90]  <- NA
cont_df$c3g[cont_df$c3g > 90] <- NA
cont_df$c3h[cont_df$c3h > 80] <- NA
cont_df$k1[cont_df$k1 > 90]   <- NA

## Correlations
correlation_results <- lapply(continuous_vars, function(v) cor.test(cont_df[[v]], cont_df$gender_index, use = "complete.obs"))
cor_summary <- data.frame(
  Variable    = continuous_vars,
  Correlation = sapply(correlation_results, \(x) unname(x$estimate)),
  P_Value     = sapply(correlation_results, \(x) x$p.value)
)
print(cor_summary)

## ---------------------------
## 11) Multiple Linear Regression (MLR)
## ---------------------------
mlr_df <- factor_data
mlr_fit <- lm(
  gender_index ~ b1_group + b3_group + b4_group + f3_group +
    a1 + a9 + a11 + a12 + a13 + a2y + b2 + j12 + k1,
  data = mlr_df
)
summary(mlr_fit)
vif(mlr_fit)
boxcox(mlr_fit)
step_model <- step(mlr_fit, direction = "both", trace = 0)
print(step_model)
dw_test <- dwtest(mlr_fit)
print(dw_test)

## Optional: remove a few hard-coded outliers (indices from prior diagnostics)
# mlr_df_filtered <- mlr_df[-c(760, 937, 1488), ]
# mlr_fit_filtered <- lm(
#   gender_index ~ b1_group + b3_group + b4_group + f3_group +
#     a1 + a9 + a11 + a12 + a13 + a2y + b2 + j12 + k1,
#   data = mlr_df_filtered
# )
# summary(mlr_fit_filtered)

## ---------------------------
## 12) GLM (Gaussian)
## ---------------------------
glm_fit <- glm(
  gender_index ~ b1_group + b3_group + b4_group + f3_group +
    a1 + a9 + a11 + a12 + a13 + a2y + b2 + j12 + k1,
  family = gaussian(link = "identity"),
  data = mlr_df
)
summary(glm_fit)
print(pR2(glm_fit))

## ---------------------------
## 13) Random Forest (RF) + tuning via caret (ranger)
## ---------------------------
rf_df <- na.omit(mlr_df)
rf_model <- randomForest(
  gender_index ~ b1_group + b3_group + b4_group + f3_group +
    a1 + a9 + a11 + a12 + a13 + a2y + b2 + j12 + k1,
  data = rf_df,
  importance = TRUE
)
print(rf_model)
print(importance(rf_model))

## Tidy importance and plot
imp_df <- as.data.frame(importance(rf_model))
imp_df$Variable <- rownames(imp_df)
rownames(imp_df) <- NULL
imp_long <- imp_df %>%
  pivot_longer(cols = intersect(colnames(imp_df), c("%IncMSE","IncNodePurity")),
               names_to = "Metric", values_to = "Value")

p_imp <- ggplot(imp_long, aes(x = reorder(Variable, Value), y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = round(Value, 2)),
            position = position_dodge(width = 0.9),
            hjust = -0.1, color = "grey40") +
  coord_flip() +
  labs(title = "Variable Importance (%IncMSE / IncNodePurity)",
       x = "Variables", y = "Importance")
print(p_imp)

## caret + ranger tuning
rf_grid <- expand.grid(
  mtry = c(2,3,4,5),
  splitrule = "variance",
  min.node.size = c(1,5,10)
)
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)
rf_optimized <- train(
  gender_index ~ b1_group + b3_group + b4_group + f3_group +
    a1 + a9 + a11 + a12 + a13 + a2y + b2 + j12 + k1,
  data = rf_df,
  method = "ranger",
  trControl = ctrl,
  tuneGrid = rf_grid,
  importance = "permutation"
)
print(rf_optimized$bestTune)

## Refit reduced RF on a few important vars (example)
important_vars <- c("a2y","b2","a9")
rf_reduced <- randomForest(
  gender_index ~ .,
  data = rf_df[, c(important_vars, "gender_index")],
  ntree = 500, mtry = min(3, length(important_vars)),
  importance = TRUE
)
print(rf_reduced)
varImpPlot(rf_reduced, main = "Variable Importance (Reduced RF)")

## ---------------------------
## 14) One-way ANOVA (group means with basic stats)
## ---------------------------
group_factors <- c("a1","a9","a11","a12","a13","b1_group","b3_group","b4_group","f3_group")

for (fct in group_factors) {
  cat("\n=== One-way ANOVA —", fct, "===\n")
  aov_fit <- aov(gender_index ~ as.factor(mlr_df[[fct]]), data = mlr_df)
  print(summary(aov_fit))

  stats_tbl <- mlr_df %>%
    group_by(across(all_of(fct))) %>%
    summarise(
      mean = mean(gender_index, na.rm = TRUE),
      sd   = sd(gender_index, na.rm = TRUE),
      n    = n(),
      .groups = "drop"
    )
  print(stats_tbl)
}

## ---------------------------
## 15) Simple regression & plots
## ---------------------------
cleaned_for_plot <- mlr_df %>%
  filter(!a2y %in% c(997, 998)) # remove special codes if present

## Correlation
corr_a2y <- cor(cleaned_for_plot$a2y, cleaned_for_plot$gender_index, use = "complete.obs")
cat("\nCorrelation (a2y vs gender_index):", round(corr_a2y, 3), "\n")

## Scatter + LM line
p_scatter <- ggplot(cleaned_for_plot, aes(x = a2y, y = gender_index)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  labs(title = "Scatter: Birth Year (a2y) vs Gender Index",
       x = "Birth Year (a2y code)", y = "Gender Equality Index")
print(p_scatter)
ggsave("results/scatter_a2y_gender_index.png", p_scatter, width = 8, height = 6, dpi = 300)

## ---------------------------
## 16) GLM (adjusted, compact set)
## ---------------------------
glm_vars <- c("a1","a11","k3","a2y","b2","gender_index")
glm_df <- data_with_index[, glm_vars, drop = FALSE] %>%
  mutate(
    k3  = as.numeric(k3),
    a2y = as.numeric(a2y),
    b2  = as.numeric(b2),
    a11 = ifelse(a11 == 97, NA, a11),
    a2y = ifelse(a2y %in% c(997, 998), NA, a2y),
    b2  = ifelse(b2 > 90, NA, b2),
    k3  = ifelse(k3 > 90, NA, k3)
  ) %>%
  na.omit()

summary(glm_df)

glm_adj <- glm(
  gender_index ~ a1 + a11 + k3 + a2y + b2,
  family = gaussian(link = "identity"),
  data = glm_df
)
summary(glm_adj)
print(pR2(glm_adj))

## Education vs Index plot
p_b2 <- ggplot(glm_df, aes(x = b2, y = gender_index)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", color = "orange", se = TRUE) +
  labs(title = "Regression: Education Years (b2) vs Gender Index",
       x = "Years of Education (b2)", y = "Gender Equality Index")
print(p_b2)

## ---------------------------
## End
## ---------------------------