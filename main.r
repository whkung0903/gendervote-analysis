## =========================================================
## Project: TSCS 2022 Gender Module — EDA & Inference
## Author : Grace Kung
## =========================================================

## ---------------------------
## 1) Load packages
## ---------------------------
suppressPackageStartupMessages({
  library(haven)       # read_dta
  library(dplyr)       # data wrangling
  library(tidyr)       # tidying
  library(broom)       # model tidying
  library(ggplot2)     # plotting
  library(reshape2)    # melt/cast (legacy support)
  library(RColorBrewer)# color palettes
  library(scales)      # percent_format, etc.
})

## ---------------------------
## 2) Read data (from repo's data/ folder)
## ---------------------------
file_path <- "data/survey_data.dta"
raw_data  <- read_dta(file_path)

## ---------------------------
## 3) Basic cleaning
## ---------------------------
## Assumptions:
## - a2y: birth year or age (original script filtered <= 100; keep same)
## - b2 : years of education (remove > 90 as in original)
## - f3 : presidential vote choice (1=Sung, 2=Han, 3=Tsai, 4~7=others)
## - a11: residence area type (1~5)
## - k1, k3: social status / income (categoricals; keep as factor)

data_clean <- raw_data %>%
  filter(
    (is.na(a2y) | a2y <= 100),
    (is.na(b2)  | b2  <= 90),
    !(f3  %in% c(97, 98)),
    !(a11 %in% c(97)),
    !(k1  %in% c(97, 98))
  )

## ---------------------------
## 4) Variable selection & recoding
## ---------------------------
vars <- c("a1","a2y","f3","a11","b2","k1","k3")
data <- data_clean %>%
  select(any_of(vars)) %>%
  mutate(
    ## Sex: 1=Male, 2=Female (adjust if needed)
    a1  = case_when(
      a1 == 1 ~ "Male",
      a1 == 2 ~ "Female",
      TRUE    ~ NA_character_
    ),

    ## Keep numeric
    a2y = suppressWarnings(as.numeric(a2y)),
    b2  = suppressWarnings(as.numeric(b2)),

    ## f3: explicit mapping
    f3 = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      f3 %in% 4:7 ~ "Other",
      TRUE ~ NA_character_
    ),

    ## a11: area type (assumed 1~5; adjust if your codebook differs)
    a11 = case_when(
      a11 == 1 ~ "Metro",
      a11 == 2 ~ "Suburban",
      a11 == 3 ~ "SmallTown",
      a11 == 4 ~ "Rural",
      a11 == 5 ~ "IndependentFarm",
      TRUE     ~ NA_character_
    ),

    ## Social status / income as factors
    k1 = as.factor(k1),
    k3 = as.factor(k3)
  ) %>%
  mutate(
    a1  = factor(a1,  levels = c("Male","Female")),
    f3  = factor(f3,  levels = c("Sung","Han","Tsai","Other")),
    a11 = factor(a11, levels = c("Metro","Suburban","SmallTown","Rural","IndependentFarm"))
  )

## ---------------------------
## 5) EDA — Distributions
## ---------------------------
theme_set(theme_minimal())

## (1) Birth year/age distribution (a2y)
a2y_mean <- mean(data$a2y, na.rm = TRUE)
p_a2y <- ggplot(data, aes(x = a2y)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "white") +
  geom_vline(xintercept = a2y_mean, color = "darkblue", linetype = "dashed", linewidth = 1) +
  annotate("text", x = a2y_mean + 5, y = Inf, label = paste("Mean:", round(a2y_mean, 2)),
           color = "darkblue", vjust = 2) +
  labs(title = "Distribution of Birth Year (a2y)", x = "Birth Year / Age (a2y code)", y = "Count")
print(p_a2y)

## (2) Political preference distribution (f3)
p_f3 <- ggplot(data, aes(x = f3, fill = f3)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Political Preference (f3)", x = "Candidate", y = "Count") +
  theme(legend.position = "none")
print(p_f3)

## (3) Residence area distribution (a11)
p_a11 <- ggplot(data, aes(x = a11, fill = a11)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) +
  labs(title = "Distribution of Residence Area (a11)", x = "Area Type", y = "Count") +
  theme(legend.position = "none")
print(p_a11)

## (4) Years of education (b2)
b2_mean <- mean(data$b2, na.rm = TRUE)
p_b2 <- ggplot(data, aes(x = b2)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "white") +
  geom_vline(xintercept = b2_mean, color = "darkblue", linetype = "dashed", linewidth = 1) +
  annotate("text", x = b2_mean + 2, y = Inf, label = paste("Mean:", round(b2_mean, 2)),
           color = "darkblue", vjust = 2) +
  labs(title = "Distribution of Years of Education (b2)", x = "Years of Education", y = "Count")
print(p_b2)

## (5) Social status (k1)
p_k1 <- ggplot(data, aes(x = k1, fill = k1)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) +
  labs(title = "Distribution of Social Status (k1)", x = "Social Status", y = "Count") +
  theme(legend.position = "none")
print(p_k1)

## (6) Income (k3)
p_k3 <- ggplot(data, aes(x = k3, fill = k3)) +
  geom_bar() +
  labs(title = "Distribution of Income (k3)", x = "Income Category", y = "Count") +
  theme(legend.position = "none")
print(p_k3)

## (7) Sex (a1) — pie chart
a1_summary <- data %>%
  filter(!is.na(a1)) %>%
  group_by(a1) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(percentage = count / sum(count) * 100)

p_a1_pie <- ggplot(a1_summary, aes(x = "", y = percentage, fill = a1)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Sex Distribution (a1)", fill = "Sex") +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )
print(p_a1_pie)

## ---------------------------
## 6) Pie chart for f3 (Top 3 labels)
## ---------------------------
f3_data <- raw_data %>%
  filter(!is.na(f3)) %>%
  mutate(
    Category = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      f3 %in% 4:7 ~ "Other",
      TRUE ~ "Unknown"
    )
  )

f3_summary <- f3_data %>%
  group_by(Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Percentage = Count / sum(Count) * 100) %>%
  arrange(desc(Count))

p_f3_pie <- ggplot(f3_summary, aes(x = "", y = Count, fill = Category)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(title = "Distribution of Presidential Vote (f3)", x = NULL, y = NULL, fill = "Category") +
  theme(
    axis.ticks   = element_blank(),
    axis.text    = element_blank(),
    panel.grid   = element_blank()
  ) +
  geom_text(
    data = f3_summary %>% slice(1:min(3, n())),
    aes(label = paste0(round(Percentage, 1), "%")),
    position = position_stack(vjust = 0.5),
    size = 5
  )

print(p_f3_pie)
ggsave("f3_pie_chart.png", plot = p_f3_pie, width = 8, height = 8)

## ---------------------------
## 7) Chi-square across many items by f3_group
## ---------------------------
chi_data <- raw_data %>%
  select(
    f3, a1, a2y, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a15, a17, a18,
    b1, b2, b3, b4, b5, c3g, c3h, c4, c7g, c7h, d1a, e1a, e1b, e1c, e1d, e1e,
    e2a, e2b, e3, e4, e5a, e5b, e6a, e6b, e6c, e8a, e8b, e8c, e8d, e11a, e11b,
    e11c, e11d, e11e, e11f, e11g, e14, e15a, e15b, e21a, e21b, e21c, e39a,
    e39b, e39c, e40a, e40b, e40c, e40d, e40e, e41a, e41b, e41c, e41d, e41e,
    e42, f2a, f2b, f2c, g1, g3, j2a, j2b, j2c, j2d, j5, j6, j10a, j10b, j10c,
    j10d, j10e, j10f, j10g, j10h, j12, k1, k3, k4, k5
  ) %>%
  filter(f3 %in% c(1, 2, 3)) %>%
  mutate(
    f3_group = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      TRUE ~ NA_character_
    )
  )

## Question mapping (labels)
question_mapping <- c(
  "a1"="Sex", "a2y"="Birth year (ID-based)", "a4"="Father ethnicity", "a5"="Father birthplace",
  "a6"="Mother ethnicity", "a7"="Mother birthplace", "a8"="Self ethnicity", "a9"="Religion",
  "a10"="Religious attendance", "a11"="Residence area", "a12"="Marital status",
  "a13"="Cohabiting partner/spouse", "a15"="Spouse sex", "a17"="Spouse ethnicity",
  "a18"="Number of children", "b1"="Education level", "b2"="Years of education",
  "b3"="Father education", "b4"="Mother education", "b5"="Spouse education",
  "c3g"="Weekly working hours", "c3h"="Years of work experience", "c4"="Union membership",
  "c7g"="Spouse weekly working hours", "c7h"="Spouse years of work exp.",
  "d1a"="Household size",
  "e1a"="Working mothers can bond with children equally well",
  "e1b"="Working mothers are bad for pre-school children",
  "e1c"="Full-time working wife hurts family life",
  "e1d"="Most women ultimately want home & kids",
  "e1e"="Homemakers feel as fulfilled as paid workers",
  "e2a"="Both men and women should contribute to income",
  "e2b"="Men earn, women care for home",
  "e3"="Household responsibilities should be shared",
  "e4"="Mother vs father caregiving ability",
  "e5a"="Should women work when kids are pre-school?",
  "e5b"="Should women work after kids enter school?",
  "e6a"="Married people are happier",
  "e6b"="Those wanting children should marry",
  "e6c"="Cohabitation is fine even without plans to marry",
  "e8a"="Single moms can raise kids as well as couples",
  "e8b"="Single dads can raise kids as well as couples",
  "e8c"="Lesbian couples can raise kids as well as couples",
  "e8d"="Gay male couples can raise kids as well as couples",
  "e11a"="Who should provide family income?",
  "e11b"="Who handles daily childcare?",
  "e11c"="Who handles play/leisure with kids?",
  "e11d"="Who teaches kids behavior?",
  "e11e"="Who listens & counsels kids?",
  "e11f"="Who sets an example for kids?",
  "e11g"="During pandemic, who should take leave to care for kids?",
  "e14"="Who should take parental leave if both work?",
  "e15a"="Best arrangement (pre-school child) for work-family",
  "e15b"="Worst arrangement (pre-school child) for work-family",
  "e21a"="Weekly housework time", "e21b"="Weekly family care time",
  "e21c"="Weekly leisure time",
  "e39a"="Cabinet ministers better male or female?",
  "e39b"="University presidents better male or female?",
  "e39c"="Corporate executives better male or female?",
  "e40a"="Men's career barrier: lack of ambition",
  "e40b"="Men's barrier: cannot command subordinates",
  "e40c"="Men's barrier: family responsibilities",
  "e40d"="Men's barrier: lack of network",
  "e40e"="Men's barrier: physical limits for long hours",
  "e41a"="Women's barrier: lack of ambition",
  "e41b"="Women's barrier: cannot command subordinates",
  "e41c"="Women's barrier: family responsibilities",
  "e41d"="Women's barrier: lack of network",
  "e41e"="Women's barrier: physical limits for long hours",
  "e42"="Restrict women's night work by law?",
  "f2a"="Politics is for men",
  "f2b"="Quota seats for women in elections",
  "f2c"="Men and women are already equal in Taiwan",
  "f3"="Voted for which candidate",
  "g1"="Child should take father's or mother's surname?",
  "g3"="Would you let your kid take mother's surname?",
  "j2a"="Legal punishment & consent (adult female)",
  "j2b"="Legal punishment & consent (spouses)",
  "j2c"="Legal punishment & consent (teacher-student)",
  "j2d"="Legal punishment & consent (junior high)",
  "j5"="Married men: affairs acceptable?",
  "j6"="Married women: affairs acceptable?",
  "j10a"="Coming out causes parents' pain",
  "j10b"="Homosexuals have messy private lives",
  "j10c"="Gay men are effeminate",
  "j10d"="Lesbians are masculine",
  "j10e"="Accept homosexuals kissing in public",
  "j10f"="Accept heterosexuals kissing in public",
  "j10g"="Homosexuals should have right to marry",
  "j10h"="Homosexuals should have right to have children",
  "j12"="Self-identified sexual orientation",
  "k1"="Social status", "k3"="Monthly personal income",
  "k4"="Spouse income", "k5"="Household income"
)

x_vars <- names(question_mapping)

## Ensure order of groups
chi_data$f3_group <- factor(chi_data$f3_group, levels = c("Sung","Han","Tsai"))

## Chi-square loop
chi_results <- lapply(x_vars, function(var) {
  tab <- table(chi_data$f3_group, chi_data[[var]], useNA = "ifany")
  if (nrow(tab) > 1 && ncol(tab) > 1) {
    chi <- suppressWarnings(chisq.test(tab))
    tibble::tibble(
      Variable = var,
      Question = question_mapping[var],
      Sung     = paste0(colnames(tab), "=", tab[1, ], collapse = "; "),
      Han      = paste0(colnames(tab), "=", tab[2, ], collapse = "; "),
      Tsai     = paste0(colnames(tab), "=", tab[3, ], collapse = "; "),
      p_value  = chi$p.value,
      Chi_Sq   = as.numeric(chi$statistic),
      DF       = as.numeric(chi$parameter)
    )
  } else {
    tibble::tibble(
      Variable = var, Question = question_mapping[var],
      Sung = NA_character_, Han = NA_character_, Tsai = NA_character_,
      p_value = NA_real_, Chi_Sq = NA_real_, DF = NA_real_
    )
  }
}) %>% dplyr::bind_rows() %>%
  mutate(Significance = dplyr::case_when(
    is.na(p_value)        ~ "NA",
    p_value < 0.001       ~ "***",
    p_value < 0.01        ~ "**",
    p_value < 0.05        ~ "*",
    p_value < 0.1         ~ ".",
    TRUE                  ~ "ns"
  ))

## Save chi-square results
write.csv(chi_results, "chi_square_results_v3.csv", row.names = FALSE)

## ---------------------------
## 8) Visualization — stacked percentage bars for selected variables
## ---------------------------
selected_variables <- c("f3","a4","a5","a6","a7","a8",
                        "e8a","e8c","e8d","e39a","j10g","j10h",
                        "a12","j10a","j10b","j10c","j10e","j10f",
                        "c4","e6c","e8b","e11a","e11d","j2b","j2c")

viz_data <- read.csv("chi_square_results_v3.csv", stringsAsFactors = FALSE) %>%
  dplyr::filter(Variable %in% selected_variables)

## Parse helper
parse_answers <- function(variable, answers, candidate) {
  if (is.na(answers) || answers == "") {
    return(data.frame(
      Variable = character(0), Candidate = character(0),
      Option = character(0), Count = numeric(0), Percentage = numeric(0)
    ))
  }
  opts <- strsplit(answers, "; ")[[1]]
  opt_names <- sapply(opts, function(x) strsplit(x, "=")[[1]][1])
  opt_vals  <- as.numeric(sapply(opts, function(x) strsplit(x, "=")[[1]][2]))
  total <- sum(opt_vals, na.rm = TRUE)
  data.frame(
    Variable   = variable,
    Candidate  = candidate,
    Option     = opt_names,
    Count      = opt_vals,
    Percentage = ifelse(total > 0, (opt_vals / total) * 100, NA_real_),
    stringsAsFactors = FALSE
  )
}

## Build long-form parsed data
parsed_data <- do.call(rbind, lapply(seq_len(nrow(viz_data)), function(i) {
  rbind(
    parse_answers(viz_data$Variable[i], viz_data$Sung[i], "Sung"),
    parse_answers(viz_data$Variable[i], viz_data$Han[i],  "Han"),
    parse_answers(viz_data$Variable[i], viz_data$Tsai[i], "Tsai")
  )
}))

## Output directory for plots
out_dir <- "plots"
if (!dir.exists(out_dir)) dir.create(out_dir)

## Colors
pal <- brewer.pal(n = 12, name = "Set3")

## Plot each variable
for (v in unique(parsed_data$Variable)) {
  dfv <- parsed_data %>% dplyr::filter(Variable == v)
  if (nrow(dfv) == 0) next
  p <- ggplot(dfv, aes(x = Candidate, y = Percentage, fill = Option)) +
    geom_bar(stat = "identity", position = "fill") +
    scale_y_continuous(labels = percent_format()) +
    scale_fill_manual(values = pal) +
    labs(title = paste("Question:", v), x = "Candidate", y = "Percentage", fill = "Option") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(filename = file.path(out_dir, paste0(v, "_percentage_plot.png")), plot = p, width = 10, height = 6)
}

## ---------------------------
## 9) ANOVA across groups for selected questions
## ---------------------------
anova_questions <- c("e8a","e8c","e8d","j10g","j10h","a2y",
                     "j10a","j10b","j10c","j10e","j10f",
                     "e6c","e8b","e11a","e11d","j2b","j2c")

anova_data <- raw_data %>%
  mutate(
    f3_group = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      f3 %in% 4:7 ~ "Other",
      TRUE ~ NA_character_
    )
  )

anova_results <- list()
for (q in anova_questions) {
  form <- as.formula(paste(q, "~ f3_group"))
  fit  <- tryCatch(aov(form, data = anova_data), error = function(e) NULL)
  anova_results[[q]] <- if (is.null(fit)) NA else summary(fit)
  cat("\n=== ANOVA Result for", q, "===\n")
  if (!is.null(fit)) print(summary(fit)) else cat("Skipped (model error)\n")
}

anova_summary <- data.frame(
  Question = names(anova_results),
  P_Value  = sapply(anova_results, function(res) {
    if (is.list(res) && !is.null(res[[1]])) res[[1]]$`Pr(>F)`[1] else NA
  })
)
anova_summary$Significance <- cut(
  anova_summary$P_Value,
  breaks = c(-Inf, 0.001, 0.01, 0.05, 0.1, Inf),
  labels = c("***","**","*",".",""),
  right = TRUE
)
print(anova_summary)
write.csv(anova_summary, "f3_ANOVA_sum.csv", row.names = FALSE)

## ---------------------------
## 10) Boxplots — Household size by f3_group (d1a)
## ---------------------------
box_data <- raw_data %>%
  filter(f3 %in% c(1,2,3)) %>%
  mutate(
    f3_group = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      TRUE ~ NA_character_
    )
  )

stats_d1a <- box_data %>%
  group_by(f3_group) %>%
  summarise(
    Median = median(d1a, na.rm = TRUE),
    Q1     = quantile(d1a, 0.25, na.rm = TRUE),
    Q3     = quantile(d1a, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p_d1a <- ggplot(box_data, aes(x = f3_group, y = d1a, fill = f3_group)) +
  geom_boxplot(outlier.colour = "orange", outlier.shape = 8, alpha = 0.7) +
  geom_text(data = stats_d1a, aes(x = f3_group, y = Median, label = paste0("Median: ", round(Median, 1))),
            vjust = -0.5, color = "black", size = 3) +
  geom_text(data = stats_d1a, aes(x = f3_group, y = Q1, label = paste0("Q1: ", round(Q1, 1))),
            vjust =  1.5, color = "darkblue", size = 3) +
  geom_text(data = stats_d1a, aes(x = f3_group, y = Q3, label = paste0("Q3: ", round(Q3, 1))),
            vjust = -1.5, color = "darkgreen", size = 3) +
  labs(title = "Household Size (d1a) by Political Preference", x = "Candidate Support", y = "Household Size (d1a)") +
  scale_fill_manual(values = c("Sung"="#ff7f0e","Han"="#1f77b4","Tsai"="#2ca02c"), name = "Candidate") +
  coord_cartesian(ylim = c(0, NA)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
print(p_d1a)

## ---------------------------
## 11) Boxplots — Birth year/age by f3_group (a2y)
## ---------------------------
age_data <- raw_data %>%
  filter(f3 %in% c(1,2,3)) %>%
  mutate(
    f3_group = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      TRUE ~ NA_character_
    ),
    a2y = ifelse(!is.na(a2y) & a2y <= 100, a2y, NA_real_)
  )

stats_a2y <- age_data %>%
  group_by(f3_group) %>%
  summarise(
    Median = median(a2y, na.rm = TRUE),
    Q1     = quantile(a2y, 0.25, na.rm = TRUE),
    Q3     = quantile(a2y, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p_a2y_box <- ggplot(age_data, aes(x = f3_group, y = a2y, fill = f3_group)) +
  geom_boxplot(outlier.colour = "orange", outlier.shape = 8, alpha = 0.7) +
  geom_text(data = stats_a2y, aes(x = f3_group, y = Median, label = paste0("Median: ", round(Median, 1))),
            vjust = -0.5, color = "black", size = 3) +
  geom_text(data = stats_a2y, aes(x = f3_group, y = Q1, label = paste0("Q1: ", round(Q1, 1))),
            vjust =  1.5, color = "darkblue", size = 3) +
  geom_text(data = stats_a2y, aes(x = f3_group, y = Q3, label = paste0("Q3: ", round(Q3, 1))),
            vjust = -1.5, color = "darkgreen", size = 3) +
  labs(title = "Birth Year / Age (a2y) by Political Preference", x = "Candidate Support", y = "a2y") +
  scale_fill_manual(values = c("Sung"="#ff7f0e","Han"="#1f77b4","Tsai"="#2ca02c"), name = "Candidate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
print(p_a2y_box)

## ---------------------------
## 12) Boxplots — Years of education by f3_group (b2)
## ---------------------------
edu_data <- raw_data %>%
  filter(f3 %in% c(1,2,3)) %>%
  mutate(
    f3_group = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      TRUE ~ NA_character_
    ),
    b2 = ifelse(!is.na(b2) & b2 <= 40, b2, NA_real_)
  )

stats_b2 <- edu_data %>%
  group_by(f3_group) %>%
  summarise(
    Median = median(b2, na.rm = TRUE),
    Q1     = quantile(b2, 0.25, na.rm = TRUE),
    Q3     = quantile(b2, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

p_b2_box <- ggplot(edu_data, aes(x = f3_group, y = b2, fill = f3_group)) +
  geom_boxplot(outlier.colour = "orange", outlier.shape = 8, alpha = 0.7) +
  geom_text(data = stats_b2, aes(x = f3_group, y = Median, label = paste0("Median: ", round(Median, 1))),
            vjust = -0.5, color = "black", size = 3) +
  geom_text(data = stats_b2, aes(x = f3_group, y = Q1, label = paste0("Q1: ", round(Q1, 1))),
            vjust =  1.5, color = "darkblue", size = 3) +
  geom_text(data = stats_b2, aes(x = f3_group, y = Q3, label = paste0("Q3: ", round(Q3, 1))),
            vjust = -1.5, color = "darkgreen", size = 3) +
  labs(title = "Years of Education (b2) by Political Preference", x = "Candidate Support", y = "Years of Education (b2)") +
  scale_fill_manual(values = c("Sung"="#ff7f0e","Han"="#1f77b4","Tsai"="#2ca02c"), name = "Candidate") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
print(p_b2_box)

## ---------------------------
## 13) Stacked proportions — Union membership (c4) by f3_group
## ---------------------------
union_data <- raw_data %>%
  filter(f3 %in% c(1,2,3), !is.na(c4)) %>%
  mutate(
    f3_group = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      TRUE ~ NA_character_
    ),
    c4 = factor(c4, labels = c("CurrentMember","PastMember","Never","Other"))
  ) %>%
  group_by(f3_group, c4) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(f3_group) %>%
  mutate(Percentage = Count / sum(Count) * 100)

p_union <- ggplot(union_data, aes(x = f3_group, y = Percentage, fill = c4)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(title = "Union Membership (c4) by Political Preference", x = "Candidate Support", y = "Proportion", fill = "Union") +
  scale_y_continuous(labels = percent) +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
print(p_union)

## ---------------------------
## 14) Interaction (ANOVA) — a2y ~ b2 * f3_group & b2 ~ a2y * f3_group
## ---------------------------
interact_data <- raw_data %>%
  select(f3, a2y, b2) %>%
  filter(!is.na(f3), !is.na(a2y), !is.na(b2)) %>%
  filter(a2y <= 100, b2 <= 90) %>%
  mutate(
    f3_group = case_when(
      f3 == 1 ~ "Sung",
      f3 == 2 ~ "Han",
      f3 == 3 ~ "Tsai",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(f3_group))

model_a2y <- aov(a2y ~ b2 * f3_group, data = interact_data)
model_b2  <- aov(b2  ~ a2y * f3_group, data = interact_data)

cat("\n=== ANOVA: a2y ~ b2 * f3_group ===\n")
print(summary(model_a2y))
cat("\n=== ANOVA: b2 ~ a2y * f3_group ===\n")
print(summary(model_b2))

p_interact <- ggplot(interact_data, aes(x = b2, y = a2y, color = f3_group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Interaction: Education (b2) and Political Preference on Birth Year/Age (a2y)",
    x = "Years of Education (b2)", y = "Birth Year / Age (a2y)", color = "Political Preference"
  ) +
  theme_minimal()
print(p_interact)

## ---------------------------
## End of script
## ---------------------------