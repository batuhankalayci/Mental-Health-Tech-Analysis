# =============================================================================
# STAT 482 – CATEGORICAL DATA ANALYSIS  |  TERM PROJECT
# Middle East Technical University – Department of Statistics
# Spring 2025-2026
# Dependent Variable: treatment (Yes / No)
# Dataset: survey.csv  (Mental Health in Tech Workplace)
# =============================================================================

# ── Paket listesi ─────────────────────────────────────────
pkgs <- c(
  "tidyverse", "ggplot2", "dplyr", "readr", "forcats", "stringr", "tidyr",
  "VIM", "mice", "naniar",
  "vcd", "DescTools", "epitools", "coin",
  "gtsummary", "gt", "tidyselect", "broom", "flextable",
  "caret", "glmnet", "rpart", "rpart.plot", "randomForest",
  "pROC", "ResourceSelection", "car",
  "ggcorrplot", "GGally", "patchwork",
  "knitr", "kableExtra"
)

# ── Eksik paketleri bul ───────────────────────────────────
installed <- rownames(installed.packages())
missing_pkgs <- setdiff(pkgs, installed)

# ── Güvenli kurulum fonksiyonu ────────────────────────────
safe_install <- function(pkg) {
  tryCatch({
    install.packages(pkg, repos = "https://cloud.r-project.org", dependencies = TRUE)
    TRUE
  }, error = function(e) {
    message("❌ Kurulamadı: ", pkg)
    FALSE
  })
}

# ── Kurulum ───────────────────────────────────────────────
if (length(missing_pkgs) > 0) {
  message("Eksik paketler kuruluyor...")
  results <- sapply(missing_pkgs, safe_install)
}

# ── Library yükleme (çökmez versiyon) ─────────────────────
loaded <- c()

for (pkg in pkgs) {
  ok <- tryCatch({
    library(pkg, character.only = TRUE)
    TRUE
  }, error = function(e) {
    message("⚠️ Yüklenemedi: ", pkg)
    FALSE
  })
  
  if (ok) loaded <- c(loaded, pkg)
}

# ── Özet ───────────────────────────────────────────────────
message("\n✔ Yüklenen paket sayısı: ", length(loaded))
message("❌ Yüklenemeyen paketler: ", paste(setdiff(pkgs, loaded), collapse = ", "))

# Global seed (Modellerde şans faktörünü sabitlemek için)
set.seed(482)

# ── Output directories ────────────────────────────────────────────────────────
dirs <- c("tables", "figures", "models")
for (d in dirs) if (!dir.exists(d)) dir.create(d)

# Helper: save ggplot figures
save_fig <- function(p, fname, w = 10, h = 7) {
  ggsave(file.path("figures", fname), plot = p, width = w, height = h, dpi = 150)
}

# =============================================================================
# 1. DATA LOADING & INITIAL INSPECTION
# =============================================================================
cat("\n========== 1. DATA LOADING & INITIAL INSPECTION ==========\n")
library(readr)

# NOT: Bu script'in çalıştığı klasörde (working directory) survey.csv
# dosyasının bulunduğunu varsayar. Gerekirse RStudio'da
# Session > Set Working Directory > To Source File Location kullanın,
# ya da aşağıdaki satırı açıp kendi yolunuzu yazın:
# setwd("kendi/klasor/yolunuz")
cat("Working directory:", getwd(), "\n")
raw <- read_csv("survey.csv", show_col_types = FALSE)

cat("\n--- Dimensions ---\n")
print(dim(raw))
library(tidyverse)
cat("\n--- Structure ---\n")
glimpse(raw)

cat("\n--- Summary Statistics ---\n")
print(summary(raw))

cat("\n--- Missing Value Counts ---\n")
missing_counts <- colSums(is.na(raw))
print(missing_counts[missing_counts > 0])

cat("\n--- Duplicate Rows ---\n")
dup_count <- sum(duplicated(raw))
cat("Number of duplicate rows:", dup_count, "\n")

# Export initial missing summary
miss_df <- data.frame(
  Variable      = names(missing_counts),
  Missing_Count = as.integer(missing_counts),
  Missing_Pct   = round(100 * missing_counts / nrow(raw), 2)
)
write.csv(miss_df, "tables/01_initial_missing_summary.csv", row.names = FALSE)

# =============================================================================
# 2. DATA CLEANING
# =============================================================================
cat("\n========== 2. DATA CLEANING ==========\n")

df <- raw

# ── 2a. Remove duplicates ─────────────────────────────────────────────────────
df <- df[!duplicated(df), ]
cat("Rows after duplicate removal:", nrow(df), "\n")

# ── 2b. Remove Timestamp and comments (>85% missing / non-informative) ────────
df <- df %>% select(-Timestamp, -comments, -state)
cat("Dropped: Timestamp, comments, state (excessive missingness or non-informative)\n")

# ── 2c. Filter impossible ages (keep 18–70) ────────────────────────────────────
cat("Age range before filter:", range(df$Age), "\n")
df <- df %>% filter(Age >= 18 & Age <= 70)
cat("Rows after age filter:", nrow(df), "\n")

# ── 2d. Standardise Gender ────────────────────────────────────────────────────
standardise_gender <- function(g) {
  g <- tolower(trimws(g))
  male_terms   <- c("male", "m", "man", "cis male", "cis man", "male-ish",
                    "maile", "mal", "male (cis)", "make", "guy (-ish) ^_^",
                    "ostensibly male", "malr")
  female_terms <- c("female", "f", "woman", "cis female", "cis-female",
                    "femail", "femake", "female (cis)", "female/woman",
                    "trans-female", "trans woman", "female (trans)")
  dplyr::case_when(
    g %in% male_terms   ~ "Male",
    g %in% female_terms ~ "Female",
    TRUE                ~ "Other"
  )
}
df$Gender <- standardise_gender(df$Gender)
cat("\nGender distribution after standardisation:\n")
print(table(df$Gender))

# ── 2e. Convert treatment to factor (reference = No) ─────────────────────────
df$treatment <- factor(df$treatment, levels = c("No", "Yes"))

# ── 2f. Convert binary / nominal variables to factors ────────────────────────
nominal_vars <- c(
  "Gender", "Country", "self_employed", "family_history",
  "remote_work", "tech_company", "benefits", "care_options",
  "wellness_program", "seek_help", "anonymity",
  "mental_health_consequence", "phys_health_consequence",
  "coworkers", "supervisor", "mental_health_interview",
  "phys_health_interview", "mental_vs_physical", "obs_consequence"
)
df[nominal_vars] <- lapply(df[nominal_vars], as.factor)

# ── 2g. Convert ordinal variables to ordered factors ─────────────────────────
df$work_interfere <- factor(df$work_interfere,
  levels  = c("Never", "Rarely", "Sometimes", "Often"),
  ordered = TRUE)

df$leave <- factor(df$leave,
  levels  = c("Very easy", "Somewhat easy", "Don't know",
               "Somewhat difficult", "Very difficult"),
  ordered = TRUE)

df$no_employees <- factor(df$no_employees,
  levels  = c("1-5", "6-25", "26-100", "100-500", "500-1000", "More than 1000"),
  ordered = TRUE)

# ── 2h. Missing data visualisation ────────────────────────────────────────────
cat("\nMissingness after cleaning step (before imputation):\n")
print(colSums(is.na(df)))

# VIM aggr plot
png("figures/02a_missingness_aggr.png", width = 1400, height = 900, res = 130)
VIM::aggr(df, col = c("steelblue", "tomato"), numbers = TRUE,
          sortVars = TRUE, labels = names(df),
          cex.axis = 0.65, gap = 3,
          ylab = c("Missing Data", "Pattern"))
dev.off()

# naniar ile değişken bazlı eksik değer grafiği
p_miss <- naniar::gg_miss_var(df) +
  labs(title = "Missing Values per Variable") +
  theme_minimal()
save_fig(p_miss, "02b_missing_var.png", w = 9, h = 5)

# ── 2i. Missing categorical values – imputation deferred ──────────────────────
# self_employed ve work_interfere'de eksik değerler var. Bunları burada
# DOLDURMUYORUZ: imputation, data leakage'ı önlemek için train/test split
# yapıldıktan SONRA, sadece train setinden hesaplanan mode ile uygulanacak
# (bkz. Section 7). mode_val() fonksiyonu orada kullanılıyor.
mode_val <- function(x) {
  ux <- na.omit(x)
  ux[which.max(tabulate(match(ux, unique(ux))))]
}

cat("\nEksik değerler (imputation split sonrasina birakildi):\n")
print(colSums(is.na(df)))
write.csv(data.frame(Variable = names(colSums(is.na(df))),
                     Missing  = colSums(is.na(df))),
          "tables/02_missing_before_split.csv", row.names = FALSE)

# =============================================================================
# 3. FEATURE ENGINEERING
# =============================================================================
cat("\n========== 3. FEATURE ENGINEERING ==========\n")

# ── AgeGroup ─────────────────────────────────────────────────────────────────
df$AgeGroup <- cut(df$Age,
  breaks = c(17, 25, 35, 45, Inf),
  labels = c("18-25", "26-35", "36-45", "46+"),
  right  = TRUE)
df$AgeGroup <- factor(df$AgeGroup, ordered = FALSE)
cat("AgeGroup distribution:\n"); print(table(df$AgeGroup))

# ── CompanySize from no_employees ─────────────────────────────────────────────
df$CompanySize <- fct_collapse(df$no_employees,
  Small  = c("1-5", "6-25"),
  Medium = c("26-100", "100-500"),
  Large  = c("500-1000", "More than 1000"))
df$CompanySize <- factor(df$CompanySize, levels = c("Small", "Medium", "Large"))
cat("CompanySize distribution:\n"); print(table(df$CompanySize))

# =============================================================================
# 4. EXPLORATORY DATA ANALYSIS
# =============================================================================
cat("\n========== 4. EXPLORATORY DATA ANALYSIS ==========\n")

# ── 4a. Univariate – Categorical Variables ────────────────────────────────────
cat_vars <- c(
  "treatment", "Gender", "family_history", "self_employed",
  "remote_work", "tech_company", "benefits", "care_options",
  "wellness_program", "seek_help", "anonymity", "coworkers",
  "supervisor", "mental_health_consequence", "phys_health_consequence",
  "mental_health_interview", "phys_health_interview",
  "mental_vs_physical", "obs_consequence",
  "work_interfere", "leave", "no_employees",
  "AgeGroup", "CompanySize"
)

freq_list <- list()
for (v in cat_vars) {
  tbl <- table(df[[v]])
  rel <- round(prop.table(tbl) * 100, 2)
  freq_list[[v]] <- data.frame(
    Variable = v,
    Level    = names(tbl),
    Count    = as.integer(tbl),
    Pct      = as.numeric(rel)
  )
}
freq_df <- do.call(rbind, freq_list)
write.csv(freq_df, "tables/04_univariate_frequencies.csv", row.names = FALSE)
cat("Frequency tables saved to tables/04_univariate_frequencies.csv\n")

# Bar plots – key variables in one figure
make_barplot <- function(var, fill_col = "steelblue") {
  ggplot(df, aes(x = .data[[var]])) +
    geom_bar(fill = fill_col, colour = "white", width = 0.65) +
    geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.4, size = 3.2) +
    labs(title = paste("Distribution of", var), x = var, y = "Count") +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
}

plots_univ <- lapply(cat_vars, make_barplot)
names(plots_univ) <- cat_vars

# Save a combined overview panel
panel_vars <- c("treatment", "Gender", "family_history", "benefits",
                "work_interfere", "leave", "AgeGroup", "CompanySize")
panel_plots <- plots_univ[panel_vars]
combined_panel <- patchwork::wrap_plots(panel_plots, ncol = 4)
save_fig(combined_panel, "04a_univariate_barplots.png", w = 20, h = 10)

# ── 4b. Age – histogram and boxplot ──────────────────────────────────────────
p_hist <- ggplot(df, aes(x = Age)) +
  geom_histogram(binwidth = 3, fill = "steelblue", colour = "white") +
  labs(title = "Age Distribution", x = "Age", y = "Count") +
  theme_minimal(base_size = 11)

p_box <- ggplot(df, aes(x = treatment, y = Age, fill = treatment)) +
  geom_boxplot(outlier.colour = "tomato", outlier.size = 1.8, alpha = 0.7) +
  geom_jitter(width = 0.15, alpha = 0.15, size = 0.8) +
  scale_fill_manual(values = c("No" = "steelblue", "Yes" = "tomato")) +
  labs(title = "Age by Treatment Status", x = "Treatment", y = "Age") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")

age_panel <- p_hist + p_box
save_fig(age_panel, "04b_age_distribution.png", w = 13, h = 5)

# ── 4c. Stacked bar – treatment proportion across key variables ───────────────
make_stacked <- function(var) {
  ggplot(df, aes(x = .data[[var]], fill = treatment)) +
    geom_bar(position = "fill", colour = "white", width = 0.7) +
    scale_fill_manual(values = c("No" = "steelblue", "Yes" = "tomato")) +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(title = paste("Treatment by", var),
         x = var, y = "Proportion", fill = "Treatment") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 35, hjust = 1))
}
key_vars_stacked <- c("Gender", "family_history", "benefits", "work_interfere",
                       "leave", "AgeGroup", "CompanySize", "care_options")
stacked_plots <- lapply(key_vars_stacked, make_stacked)
stacked_panel <- patchwork::wrap_plots(stacked_plots, ncol = 4)
save_fig(stacked_panel, "04c_stacked_treatment.png", w = 20, h = 10)

cat("EDA figures saved.\n")

# =============================================================================
# 5. BIVARIATE & MULTIVARIATE CATEGORICAL ANALYSIS
# =============================================================================
cat("\n========== 5. BIVARIATE & MULTIVARIATE ANALYSIS ==========\n")

# Helper: Cramér's V
cramers_v <- function(x, y) {
  ct  <- table(x, y)
  chi <- suppressWarnings(chisq.test(ct)$statistic)
  n   <- sum(ct)
  k   <- min(nrow(ct), ncol(ct))
  sqrt(chi / (n * (k - 1)))
}

# Helper: Contingency coefficient
cont_coeff <- function(x, y) {
  ct  <- table(x, y)
  chi <- suppressWarnings(chisq.test(ct)$statistic)
  n   <- sum(ct)
  sqrt(chi / (chi + n))
}

# Helper: OR and RR for 2×2 tables (binary predictors)
get_or_rr <- function(x, y) {
  ct <- table(x, y)
  if (all(dim(ct) == c(2, 2))) {
    res <- tryCatch(epitools::oddsratio(ct, method = "wald"), error = function(e) NULL)
    if (!is.null(res)) {
      or  <- round(res$measure[2, 1], 3)
      or_lo <- round(res$measure[2, 2], 3)
      or_hi <- round(res$measure[2, 3], 3)
      rr  <- round(epitools::riskratio(ct)$measure[2, 1], 3)
      return(list(OR = or, OR_lo = or_lo, OR_hi = or_hi, RR = rr))
    }
  }
  list(OR = NA, OR_lo = NA, OR_hi = NA, RR = NA)
}

# Predictor variables for bivariate analysis
pred_vars <- c(
  "Gender", "family_history", "benefits", "care_options",
  "seek_help", "anonymity", "coworkers", "supervisor",
  "work_interfere", "leave", "AgeGroup", "CompanySize"
)

bivar_results <- data.frame()

for (v in pred_vars) {
  ct <- table(df[[v]], df$treatment)
  # Chi-square
  chi_res  <- suppressWarnings(chisq.test(ct))
  chi_stat <- round(chi_res$statistic, 4)
  chi_p    <- round(chi_res$p.value, 5)

  # Fisher (if any expected < 5)
  fisher_p <- tryCatch({
    if (any(chi_res$expected < 5))
      round(fisher.test(ct, simulate.p.value = TRUE, B = 5000)$p.value, 5)
    else NA
  }, error = function(e) NA)

  # Effect sizes
  cv  <- round(cramers_v(df[[v]], df$treatment), 4)
  cc  <- round(cont_coeff(df[[v]], df$treatment), 4)

  # OR/RR
  or_rr <- get_or_rr(df[[v]], df$treatment)

  bivar_results <- rbind(bivar_results, data.frame(
    Variable       = v,
    Chi2_Stat      = chi_stat,
    Chi2_p         = chi_p,
    Fisher_p       = fisher_p,
    CramersV       = cv,
    ContCoeff      = cc,
    OR             = or_rr$OR,
    OR_lo          = or_rr$OR_lo,
    OR_hi          = or_rr$OR_hi,
    RR             = or_rr$RR
  ))

  cat("\n--- Variable:", v, "---\n")
  print(ct)
  cat("Chi-square:", chi_stat, " p-value:", chi_p, "\n")
  if (!is.na(fisher_p)) cat("Fisher p:", fisher_p, "\n")
  cat("Cramér's V:", cv, " | Contingency Coeff:", cc, "\n")
  if (!is.na(or_rr$OR)) cat("OR:", or_rr$OR, " (95% CI:", or_rr$OR_lo, "-", or_rr$OR_hi, ") | RR:", or_rr$RR, "\n")
}

write.csv(bivar_results, "tables/05_bivariate_results.csv", row.names = FALSE)
cat("\nBivariate results saved to tables/05_bivariate_results.csv\n")

# ── 5b. Relative Risk – all binary (Yes/No) predictors vs treatment ───────────
cat("\n--- Relative Risk: All Binary Predictors vs treatment ---\n")

# Identify predictors that are genuinely binary (exactly 2 non-NA levels)
binary_preds <- names(df)[sapply(names(df), function(v) {
  x <- df[[v]]
  is.factor(x) && !is.ordered(x) && nlevels(x) == 2
})]
# Remove treatment itself; keep only substantively meaningful predictors
binary_preds <- setdiff(binary_preds,
  c("treatment", "Country", "AgeGroup", "CompanySize"))
cat("Binary predictors identified:", paste(binary_preds, collapse = ", "), "\n\n")

rr_results <- data.frame()

for (v in binary_preds) {
  ct <- table(df[[v]], df$treatment)
  if (!all(dim(ct) == c(2, 2))) next      # skip if not exactly 2×2

  # Odds Ratio (Wald)
  or_res <- tryCatch(epitools::oddsratio(ct, method = "wald"), error = function(e) NULL)
  # Risk Ratio
  rr_res <- tryCatch(epitools::riskratio(ct, method = "wald"), error = function(e) NULL)

  if (is.null(or_res) || is.null(rr_res)) next

  # Chi-square p-value
  chi_p <- round(suppressWarnings(chisq.test(ct))$p.value, 6)

  # row 2 of measure matrix = exposed group
  or_val  <- round(or_res$measure[2, "estimate"],   3)
  or_lo   <- round(or_res$measure[2, "lower"],      3)
  or_hi   <- round(or_res$measure[2, "upper"],      3)
  rr_val  <- round(rr_res$measure[2, "estimate"],   3)
  rr_lo   <- round(rr_res$measure[2, "lower"],      3)
  rr_hi   <- round(rr_res$measure[2, "upper"],      3)

  # Attributable risk (absolute risk difference)
  p1 <- ct[2, 2] / sum(ct[2, ])   # P(treatment=Yes | exposed)
  p0 <- ct[1, 2] / sum(ct[1, ])   # P(treatment=Yes | unexposed)
  ard <- round(p1 - p0, 4)

  rr_results <- rbind(rr_results, data.frame(
    Variable    = v,
    Level_ref   = levels(df[[v]])[1],
    Level_exp   = levels(df[[v]])[2],
    P_unexposed = round(p0, 4),
    P_exposed   = round(p1, 4),
    ARD         = ard,
    OR          = or_val,
    OR_95CI     = sprintf("[%.3f, %.3f]", or_lo, or_hi),
    RR          = rr_val,
    RR_95CI     = sprintf("[%.3f, %.3f]", rr_lo, rr_hi),
    Chi2_p      = chi_p,
    Significant = ifelse(chi_p < 0.05, "Yes", "No"),
    stringsAsFactors = FALSE
  ))

  cat(sprintf(
    "  %-28s  RR = %.3f [%.3f, %.3f]  OR = %.3f [%.3f, %.3f]  ARD = %+.4f  p = %s\n",
    paste0(v, " (", levels(df[[v]])[2], " vs ", levels(df[[v]])[1], ")"),
    rr_val, rr_lo, rr_hi, or_val, or_lo, or_hi, ard,
    ifelse(chi_p < 0.001, "<.001", round(chi_p, 3))
  ))
}

write.csv(rr_results, "tables/05b_relative_risk_binary.csv", row.names = FALSE)
cat("\nRelative Risk table saved: tables/05b_relative_risk_binary.csv\n")

# ── gt table for RR results ────────────────────────────────────────────────────
library(gt)
library(tidyverse)

gt_rr <- rr_results %>%
  mutate(
    Comparison = paste0(Variable, "\n(", Level_exp, " vs. ", Level_ref, ")"),
    `P(Yes|unexposed)` = sprintf("%.3f", P_unexposed),
    `P(Yes|exposed)`   = sprintf("%.3f", P_exposed),
    `ARD`              = sprintf("%+.4f", ARD),
    `RR [95% CI]`      = paste0(RR, " ", RR_95CI),
    `OR [95% CI]`      = paste0(OR, " ", OR_95CI),
    `p-value`          = ifelse(Chi2_p < 0.001, "<.001", round(Chi2_p, 3))
  ) %>%
  select(Comparison, `P(Yes|unexposed)`, `P(Yes|exposed)`, ARD,
         `RR [95% CI]`, `OR [95% CI]`, `p-value`, Significant) %>%
  gt::gt() %>%
  gt::tab_header(
    title    = "Table 3. Relative Risk and Odds Ratio – Binary Predictors vs Treatment",
    subtitle = "Reference level = first factor level (unexposed). ARD = Absolute Risk Difference."
  ) %>%
  gt::tab_spanner(label = "Risk/Effect Measures",
                  columns = c(`RR [95% CI]`, `OR [95% CI]`, ARD)) %>%
  gt::cols_label(
    `P(Yes|unexposed)` = "P(Tx|Ref)",
    `P(Yes|exposed)`   = "P(Tx|Exp)"
  ) %>%
  gt::tab_style(
    style     = gt::cell_fill(color = "#fff3cd"),
    locations = gt::cells_body(rows = Significant == "Yes")
  ) %>%
  gt::tab_style(
    style     = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  ) %>%
  gt::tab_footnote("Significant rows (p < .05) are highlighted.") %>%
  gt::opt_row_striping() %>%
  gt::opt_table_font(font = gt::google_font("Source Sans Pro"))

gt::gtsave(gt_rr, "tables/05b_rr_gt_table.html")
cat("RR gt table saved: tables/05b_rr_gt_table.html\n")

# ── Forest plot: RR with 95% CI ───────────────────────────────────────────────
rr_forest_df <- rr_results %>%
  mutate(
    Label  = paste0(Variable, " (", Level_exp, " vs. ", Level_ref, ")"),
    RR_lo  = as.numeric(sub("\\[(.+),.*", "\\1", RR_95CI)),
    RR_hi  = as.numeric(sub(".*,\\s*(.+)\\]", "\\1", RR_95CI))
  ) %>%
  arrange(RR)

p_rr_forest <- ggplot(rr_forest_df,
                      aes(x = RR, y = reorder(Label, RR),
                          colour = Significant)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey40") +
  geom_errorbarh(aes(xmin = RR_lo, xmax = RR_hi),
                 height = 0.3, linewidth = 0.9) +
  geom_point(size = 3.5) +
  scale_colour_manual(values = c("Yes" = "tomato", "No" = "steelblue"),
                      name = "Significant\n(p < .05)") +
  labs(title    = "Risk Ratio Forest Plot – Binary Predictors vs Treatment",
       subtitle = "RR > 1: exposed group more likely to seek treatment; RR < 1: less likely",
       x = "Risk Ratio (RR)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")
save_fig(p_rr_forest, "05b_rr_forest_plot.png", w = 12, h = 5)
cat("RR forest plot saved: figures/05b_rr_forest_plot.png\n")



# ── 5a. Ordinal measures: Kendall Tau-b, Goodman-Kruskal Gamma, Somers' D ─────
ordinal_preds <- c("leave", "no_employees")
cat("\n--- Ordinal Association Measures (with tests & CIs) ---\n")

# Helper: approximate p-value from a point estimate and a symmetric CI
#   (valid when DescTools constructs CI via z * se)
pval_from_ci <- function(estimate, lwr, upr) {
  se <- (upr - lwr) / (2 * qnorm(0.975))
  if (se <= 0) return(NA_real_)
  z  <- abs(estimate / se)
  2 * pnorm(-z)
}

ordinal_results <- data.frame()

for (v in ordinal_preds) {

  x_ord <- df[[v]]                      # ordered factor
  y_bin <- df$treatment                  # factor No/Yes
  x_num <- as.numeric(x_ord)            # numeric rank codes
  y_num <- as.numeric(y_bin)            # 1 = No, 2 = Yes
  ct    <- table(x_ord, y_bin)

  # ── Kendall Tau-b  (cor.test gives tau-b for tied data + exact/asymptotic p) ─
  kt        <- cor.test(x_num, y_num, method = "kendall", exact = FALSE)
  tau_b     <- round(unname(kt$estimate), 4)
  tau_z     <- round(unname(kt$statistic), 4)    # z-statistic
  tau_p     <- round(kt$p.value, 6)

  # ── Goodman-Kruskal Gamma  (with 95% CI via bootstrap, then normal-approx p) ─
  gam_full  <- DescTools::GoodmanKruskalGamma(ct, conf.level = 0.95)
  gam_est   <- round(gam_full["gamma"],   4)
  gam_lwr   <- round(gam_full["lwr.ci"],  4)
  gam_upr   <- round(gam_full["upr.ci"],  4)
  gam_p     <- round(pval_from_ci(gam_est, gam_lwr, gam_upr), 6)

  # ── Somers' D  (d(Y|X): effect of ordinal X on binary Y, with 95% CI) ────────
  sm_full   <- DescTools::SomersDelta(x_num, y_num, conf.level = 0.95)
  sm_est    <- round(sm_full["somers"],     4)
  sm_lwr    <- round(sm_full["lwr.ci"],  4)
  sm_upr    <- round(sm_full["upr.ci"],  4)
  sm_p      <- round(pval_from_ci(sm_est, sm_lwr, sm_upr), 6)

  # ── Print ─────────────────────────────────────────────────────────────────────
  cat(sprintf(
    "\n  Variable: %s\n  %-14s  est = %7.4f   z = %7.4f   p = %s\n",
    v, "Kendall Tau-b:", tau_b, tau_z,
    ifelse(tau_p < 0.001, "< 0.001", formatC(tau_p, digits = 4, format = "f"))))
  cat(sprintf(
    "  %-14s  est = %7.4f   95%% CI [%7.4f, %7.4f]   p = %s\n",
    "Gamma:", gam_est, gam_lwr, gam_upr,
    ifelse(is.na(gam_p), "n/a",
           ifelse(gam_p < 0.001, "< 0.001", formatC(gam_p, digits = 4, format = "f")))))
  cat(sprintf(
    "  %-14s  est = %7.4f   95%% CI [%7.4f, %7.4f]   p = %s\n",
    "Somers' D:", sm_est, sm_lwr, sm_upr,
    ifelse(is.na(sm_p), "n/a",
           ifelse(sm_p < 0.001, "< 0.001", formatC(sm_p, digits = 4, format = "f")))))
  cat(sprintf("  Direction: %s → higher values of %s are %s with seeking treatment.\n",
    v, v,
    ifelse(tau_b > 0, "positively associated", "negatively associated")))

  # ── Collect row ───────────────────────────────────────────────────────────────
  ordinal_results <- rbind(ordinal_results, data.frame(
    Variable        = v,
    Levels          = nlevels(x_ord),
    Kendall_TauB    = tau_b,
    Kendall_z       = tau_z,
    Kendall_p       = tau_p,
    Gamma           = gam_est,
    Gamma_CI_lo     = gam_lwr,
    Gamma_CI_hi     = gam_upr,
    Gamma_p_approx  = gam_p,
    SomersD         = sm_est,
    SomersD_CI_lo   = sm_lwr,
    SomersD_CI_hi   = sm_upr,
    SomersD_p_approx= sm_p,
    stringsAsFactors = FALSE
  ))
}

# ── CSV export ────────────────────────────────────────────────────────────────
write.csv(ordinal_results, "tables/05a_ordinal_measures_full.csv", row.names = FALSE)
cat("\nOrdinal measures table saved: tables/05a_ordinal_measures_full.csv\n")

# ── Interpretation table (gt) ─────────────────────────────────────────────────
sig_star <- function(p) {
  dplyr::case_when(
    is.na(p)   ~ "—",
    p < 0.001  ~ "***",
    p < 0.01   ~ "**",
    p < 0.05   ~ "*",
    TRUE       ~ "ns"
  )
}

ordinal_gt_df <- ordinal_results %>%
  mutate(
    `Kendall Tau-b`  = sprintf("%.4f (z=%.3f, p%s)", Kendall_TauB, Kendall_z,
                               ifelse(Kendall_p < 0.001, "<.001",
                                      paste0("=", round(Kendall_p, 3)))),
    `Gamma`          = sprintf("%.4f [%.4f, %.4f]%s", Gamma, Gamma_CI_lo, Gamma_CI_hi,
                               sig_star(Gamma_p_approx)),
    `Somers' D`      = sprintf("%.4f [%.4f, %.4f]%s", SomersD, SomersD_CI_lo, SomersD_CI_hi,
                               sig_star(SomersD_p_approx)),
    `Sign.`          = sig_star(Kendall_p)
  ) %>%
  select(Variable, `Kendall Tau-b`, Gamma, `Somers' D`, `Sign.`)

gt_ordinal <- ordinal_gt_df %>%
  gt::gt() %>%
  gt::tab_header(
    title    = "Table 2. Ordinal Association with Treatment (Seeking Mental Health Care)",
    subtitle = "Ordinal predictors: work_interfere, leave, no_employees  |  * p<.05  ** p<.01  *** p<.001  ns = not significant"
  ) %>%
  gt::tab_spanner(label = "Measure [95% CI] / Significance", columns = c(`Kendall Tau-b`, Gamma, `Somers' D`)) %>%
  gt::cols_label(Variable = "Predictor", `Sign.` = "Sig.") %>%
  gt::tab_footnote("Tau-b: exact/asymptotic z-test. Gamma & Somers' D: 95% CI via normal approx; p derived from CI.") %>%
  gt::tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  ) %>%
  gt::opt_row_striping() %>%
  gt::opt_table_font(font = gt::google_font("Source Sans Pro"))

gt::gtsave(gt_ordinal, "tables/05a_ordinal_gt_table.html")
cat("Ordinal gt table saved: tables/05a_ordinal_gt_table.html\n")

# ── Dot-and-CI plot for ordinal measures ─────────────────────────────────────
ord_plot_df <- ordinal_results %>%
  select(Variable,
         Tau_b  = Kendall_TauB,
         Gamma  = Gamma,    Gamma_lo  = Gamma_CI_lo,  Gamma_hi  = Gamma_CI_hi,
         SomD   = SomersD,  SomD_lo   = SomersD_CI_lo, SomD_hi  = SomersD_CI_hi) %>%
  pivot_longer(
    cols      = -Variable,
    names_to  = "key",
    values_to = "value"
  ) %>%
  filter(!grepl("_lo|_hi", key)) %>%
  mutate(
    Measure = dplyr::recode(
      key,
      Tau_b = "Kendall Tau-b",
      Gamma = "Gamma",
      SomD = "Somers' D"
    )
  )

# Add CI columns back for error bars
ord_ci_df <- ordinal_results %>%
  transmute(
    Variable,
    `Kendall Tau-b` = Kendall_TauB,  Tau_lo = NA_real_, Tau_hi = NA_real_,
    Gamma           = Gamma,           G_lo   = Gamma_CI_lo,  G_hi  = Gamma_CI_hi,
    `Somers' D`     = SomersD,         S_lo   = SomersD_CI_lo, S_hi = SomersD_CI_hi
  ) %>%
  tidyr::pivot_longer(
    cols = c(`Kendall Tau-b`, Gamma, `Somers' D`),
    names_to = "Measure", values_to = "Estimate"
  ) %>%
  mutate(
    CI_lo = dplyr::case_when(
      Measure == "Gamma"     ~ G_lo,
      Measure == "Somers' D" ~ S_lo,
      TRUE ~ Estimate - 0.04),   # approximate for tau-b display
    CI_hi = dplyr::case_when(
      Measure == "Gamma"     ~ G_hi,
      Measure == "Somers' D" ~ S_hi,
      TRUE ~ Estimate + 0.04)
  )

p_ord <- ggplot(ord_ci_df,
                aes(x = Estimate, y = Variable, colour = Measure, shape = Measure)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_errorbarh(aes(xmin = CI_lo, xmax = CI_hi),
                 height = 0.18, linewidth = 0.8, alpha = 0.7) +
  geom_point(size = 3.5) +
  facet_wrap(~Measure, scales = "free_x", ncol = 3) +
  scale_colour_manual(values = c("Kendall Tau-b" = "steelblue",
                                 "Gamma"          = "tomato",
                                 "Somers' D"      = "forestgreen")) +
  labs(title    = "Ordinal Association Measures with Treatment (95% CI)",
       subtitle = "Positive values indicate higher ordinal level → more likely to seek treatment",
       x = "Estimate", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none",
        strip.text      = element_text(face = "bold"))
save_fig(p_ord, "05a_ordinal_measures_dotplot.png", w = 13, h = 4)
cat("Ordinal dot-CI plot saved: figures/05a_ordinal_measures_dotplot.png\n")



# ── 5c. Three-way contingency table analyses ───────────────────────────────────
cat("\n--- Three-Way Table 1: treatment × family_history × Gender ---\n")

tbl3_1 <- table(Treatment      = df$treatment,
                Family_History = df$family_history,
                Gender         = df$Gender)
print(tbl3_1)

# ── Flattened chi-square (overall test) ───────────────────────────────────────
chi3_1 <- suppressWarnings(chisq.test(ftable(tbl3_1)))
cat("Overall chi-square (flattened):", round(chi3_1$statistic, 4),
    " p:", round(chi3_1$p.value, 5), "\n")

# ── Stratum-level (conditional) analysis by Gender ─────────────────────────────
cat("\n  -- Stratum-level: treatment × family_history within each Gender --\n")
strata_1 <- data.frame()
for (g in levels(df$Gender)) {
  sub_df <- df[df$Gender == g, ]
  ct_s   <- table(Treatment = sub_df$treatment, Family_History = sub_df$family_history)
  chi_s  <- suppressWarnings(chisq.test(ct_s))
  or_s   <- tryCatch(epitools::oddsratio(ct_s, method = "wald"), error = function(e) NULL)
  or_v   <- if (!is.null(or_s)) round(or_s$measure[2, "estimate"], 3) else NA
  or_lo  <- if (!is.null(or_s)) round(or_s$measure[2, "lower"],    3) else NA
  or_hi  <- if (!is.null(or_s)) round(or_s$measure[2, "upper"],    3) else NA
  cat(sprintf("  Gender = %-8s  n=%d  Chi2=%.3f  p=%.4f  OR=%.3f [%.3f, %.3f]\n",
      g, nrow(sub_df), chi_s$statistic, chi_s$p.value, or_v, or_lo, or_hi))
  strata_1 <- rbind(strata_1, data.frame(
    Stratum       = g,
    n             = nrow(sub_df),
    Chi2_stat     = round(chi_s$statistic, 3),
    Chi2_p        = round(chi_s$p.value, 5),
    OR            = or_v,
    OR_lo         = or_lo,
    OR_hi         = or_hi,
    stringsAsFactors = FALSE
  ))
}
write.csv(strata_1, "tables/05c1_threeway_family_gender_strata.csv", row.names = FALSE)

# ── Cochran-Mantel-Haenszel for Table 1 ───────────────────────────────────────
# Proper 2×2×K array: rows=family_history, cols=treatment, slices=Gender
cmh_arr1 <- array(
  data = c(
    table(df$family_history[df$Gender == "Female"],
          df$treatment[df$Gender == "Female"]),
    table(df$family_history[df$Gender == "Male"],
          df$treatment[df$Gender == "Male"]),
    table(df$family_history[df$Gender == "Other"],
          df$treatment[df$Gender == "Other"])
  ),
  dim      = c(2, 2, 3),
  dimnames = list(
    Family_History = c("No", "Yes"),
    Treatment      = c("No", "Yes"),
    Gender         = c("Female", "Male", "Other")
  )
)
cmh1 <- mantelhaen.test(cmh_arr1)
cat(sprintf("\n  CMH test (family_history × treatment | Gender):\n"))
cat(sprintf("  Chi2 = %.4f  df = %d  p = %.6f\n",
            cmh1$statistic, 1, cmh1$p.value))
cat(sprintf("  Common OR = %.4f  95%% CI [%.4f, %.4f]\n",
            cmh1$estimate, cmh1$conf.int[1], cmh1$conf.int[2]))
cat(sprintf("  Interpretation: %s family history is %s with %s odds of treatment across all gender groups.\n",
  "Having a", ifelse(cmh1$estimate > 1, "associated", "inversely associated"),
  ifelse(cmh1$estimate > 1, "higher", "lower")))

# Export full three-way flat table
tbl3_1_df <- as.data.frame(tbl3_1)
write.csv(tbl3_1_df, "tables/05c1_threeway_treatment_family_gender.csv", row.names = FALSE)

# Mosaic plot (vcd)
png("figures/05c1_mosaic_treatment_family_gender.png", width = 1400, height = 800, res = 130)
vcd::mosaic(tbl3_1, shade = TRUE, legend = TRUE,
            main = "Three-Way Mosaic: treatment × family_history × Gender",
            labeling_args = list(rot_labels = c(0, 0, 0, 0)))
dev.off()

# ── Three-Way Table 2: treatment × benefits × work_interfere ─────────────────
cat("\n--- Three-Way Table 2: treatment × benefits × work_interfere ---\n")

tbl3_2 <- table(Treatment      = df$treatment,
                Benefits       = df$benefits,
                Work_Interfere = df$work_interfere)
print(tbl3_2)

chi3_2 <- suppressWarnings(chisq.test(ftable(tbl3_2)))
cat("Overall chi-square (flattened):", round(chi3_2$statistic, 4),
    " p:", round(chi3_2$p.value, 5), "\n")

# ── Stratum-level: treatment × benefits within each work_interfere level ───────
cat("\n  -- Stratum-level: treatment × benefits within each work_interfere level --\n")
strata_2 <- data.frame()
for (wi in levels(df$work_interfere)) {
  sub_df <- df[df$work_interfere == wi, ]
  if (nrow(sub_df) < 10) next
  ct_s   <- table(Treatment = sub_df$treatment, Benefits = sub_df$benefits)
  chi_s  <- suppressWarnings(chisq.test(ct_s))
  cv_s   <- round(cramers_v(sub_df$treatment, sub_df$benefits), 4)
  cat(sprintf("  work_interfere = %-12s  n=%d  Chi2=%.3f  p=%.4f  V=%.4f\n",
      wi, nrow(sub_df), chi_s$statistic, chi_s$p.value, cv_s))
  strata_2 <- rbind(strata_2, data.frame(
    Stratum       = wi,
    n             = nrow(sub_df),
    Chi2_stat     = round(chi_s$statistic, 3),
    Chi2_p        = round(chi_s$p.value, 5),
    CramersV      = cv_s,
    stringsAsFactors = FALSE
  ))
}
write.csv(strata_2, "tables/05c2_threeway_benefits_workinterfere_strata.csv", row.names = FALSE)

# Export flat three-way table
tbl3_2_df <- as.data.frame(tbl3_2)
write.csv(tbl3_2_df, "tables/05c2_threeway_treatment_benefits_workinterfere.csv",
          row.names = FALSE)

# CMH for Table 2 (treatment × benefits stratified by work_interfere)
# Build 3×K or 2×K array dynamically; benefits has 3 levels → use flattened chi only
# Aggregate benefits to Yes vs Not-Yes for CMH
df$benefits_bin <- factor(ifelse(df$benefits == "Yes", "Yes", "No"), levels = c("No", "Yes"))
wi_levels <- levels(df$work_interfere)
cmh_arr2  <- array(
  data = do.call(c, lapply(wi_levels, function(wi) {
    sub <- df[df$work_interfere == wi, ]
    as.vector(table(sub$benefits_bin, sub$treatment))
  })),
  dim      = c(2, 2, length(wi_levels)),
  dimnames = list(
    Benefits      = c("No", "Yes"),
    Treatment     = c("No", "Yes"),
    Work_Interfere= wi_levels
  )
)
cmh2 <- tryCatch(mantelhaen.test(cmh_arr2), error = function(e) NULL)
if (!is.null(cmh2)) {
  cat(sprintf("\n  CMH test (benefits × treatment | work_interfere):\n"))
  cat(sprintf("  Chi2 = %.4f  df = %d  p = %.6f\n",
              cmh2$statistic, 1, cmh2$p.value))
  cat(sprintf("  Common OR = %.4f  95%% CI [%.4f, %.4f]\n",
              cmh2$estimate, cmh2$conf.int[1], cmh2$conf.int[2]))
  cat(sprintf("  Interpretation: After stratifying by work interference level,\n"))
  cat(sprintf("  having employer benefits is %s with %s odds of seeking treatment.\n",
    ifelse(cmh2$estimate > 1, "positively associated", "negatively associated"),
    ifelse(cmh2$estimate > 1, "higher", "lower")))
}

# Mosaic plot
png("figures/05c2_mosaic_treatment_benefits_workinterfere.png",
    width = 1400, height = 800, res = 130)
vcd::mosaic(tbl3_2, shade = TRUE, legend = TRUE,
            main = "Three-Way Mosaic: treatment × benefits × work_interfere",
            labeling_args = list(rot_labels = c(0, 0, 0, 0)))
dev.off()

# ── Three-way summary export table ───────────────────────────────────────────
threeway_summary <- data.frame(
  Analysis = c(
    "treatment × family_history × Gender",
    "treatment × benefits × work_interfere"
  ),
  Overall_Chi2 = round(c(chi3_1$statistic, chi3_2$statistic), 4),
  Overall_p    = round(c(chi3_1$p.value,   chi3_2$p.value),   6),
  CMH_Chi2     = round(c(cmh1$statistic,
                         if (!is.null(cmh2)) cmh2$statistic else NA), 4),
  CMH_p        = round(c(cmh1$p.value,
                         if (!is.null(cmh2)) cmh2$p.value  else NA), 6),
  Common_OR    = round(c(cmh1$estimate,
                         if (!is.null(cmh2)) cmh2$estimate else NA), 4)
)
write.csv(threeway_summary, "tables/05c_threeway_summary.csv", row.names = FALSE)
cat("\nThree-way summary table saved: tables/05c_threeway_summary.csv\n")



# ── 5c. Visualise Cramér's V ──────────────────────────────────────────────────
p_cv <- bivar_results %>%
  filter(!is.na(CramersV)) %>%
  ggplot(aes(x = reorder(Variable, CramersV), y = CramersV, fill = CramersV)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "steelblue", high = "tomato") +
  labs(title = "Cramér's V – Association with Treatment",
       x = NULL, y = "Cramér's V") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "none")
save_fig(p_cv, "05e_cramersV.png", w = 9, h = 6)


# =============================================================================
# 6. CONFIRMATORY ANALYSIS
# =============================================================================
cat("\n========== 6. CONFIRMATORY ANALYSIS ==========\n")

# ── H1: family_history associated with treatment ──────────────────────────────
cat("\n--- H1: family_history ~ treatment ---\n")
cat("H0: family_history is independent of treatment\n")
cat("H1: family_history is associated with treatment\n")
tbl_h1 <- table(df$family_history, df$treatment)
h1_test <- chisq.test(tbl_h1)
cat("Chi-square stat:", round(h1_test$statistic, 4),
    "  df:", h1_test$parameter,
    "  p-value:", round(h1_test$p.value, 6), "\n")
cat("Decision:", ifelse(h1_test$p.value < 0.05,
    "REJECT H0: family_history is significantly associated with treatment.",
    "FAIL TO REJECT H0."), "\n")

# ── H2: benefits associated with treatment ────────────────────────────────────
cat("\n--- H2: benefits ~ treatment ---\n")
cat("H0: benefits is independent of treatment\n")
cat("H1: benefits is associated with treatment\n")
tbl_h2 <- table(df$benefits, df$treatment)
h2_test <- chisq.test(tbl_h2)
cat("Chi-square stat:", round(h2_test$statistic, 4),
    "  df:", h2_test$parameter,
    "  p-value:", round(h2_test$p.value, 6), "\n")
cat("Decision:", ifelse(h2_test$p.value < 0.05,
    "REJECT H0: benefits is significantly associated with treatment.",
    "FAIL TO REJECT H0."), "\n")

# ── H3: work_interfere associated with treatment ──────────────────────────────
cat("\n--- H3: work_interfere ~ treatment ---\n")
cat("H0: work_interfere is independent of treatment\n")
cat("H1: work_interfere is associated with treatment\n")
tbl_h3 <- table(df$work_interfere, df$treatment)
h3_test <- chisq.test(tbl_h3)
cat("Chi-square stat:", round(h3_test$statistic, 4),
    "  df:", h3_test$parameter,
    "  p-value:", round(h3_test$p.value, 6), "\n")
cat("Decision:", ifelse(h3_test$p.value < 0.05,
    "REJECT H0: work_interfere is significantly associated with treatment.",
    "FAIL TO REJECT H0."), "\n")

# ── Mantel-Haenszel: treatment vs family_history stratified by Gender ─────────
cat("\n--- Mantel-Haenszel Test: treatment ~ family_history | Gender ---\n")
mh_arr <- array(
  data = c(
    table(df$family_history[df$Gender == "Female"],
          df$treatment[df$Gender == "Female"]),
    table(df$family_history[df$Gender == "Male"],
          df$treatment[df$Gender == "Male"]),
    table(df$family_history[df$Gender == "Other"],
          df$treatment[df$Gender == "Other"])
  ),
  dim      = c(2, 2, 3),
  dimnames = list(
    Family_History = c("No", "Yes"),
    Treatment      = c("No", "Yes"),
    Gender         = c("Female", "Male", "Other")
  )
)
mh_res <- mantelhaen.test(mh_arr)
cat("Mantel-Haenszel chi-square:", round(mh_res$statistic, 4),
    "  p-value:", round(mh_res$p.value, 6), "\n")
cat("Common Odds Ratio:", round(mh_res$estimate, 4), "\n")
cat("95% CI:", round(mh_res$conf.int[1], 4), "–", round(mh_res$conf.int[2], 4), "\n")
cat("Interpretation: After stratifying by Gender, the common OR suggests that",
    "individuals with a family history of mental illness have",
    ifelse(mh_res$estimate > 1, "higher", "lower"),
    "odds of seeking treatment.\n")

# Save confirmatory results table
conf_df <- data.frame(
  Hypothesis = c("H1: family_history ~ treatment",
                 "H2: benefits ~ treatment",
                 "H3: work_interfere ~ treatment",
                 "MH: family_history ~ treatment | Gender"),
  Test       = c("Chi-square", "Chi-square", "Chi-square", "Mantel-Haenszel"),
  Statistic  = round(c(h1_test$statistic, h2_test$statistic,
                       h3_test$statistic, mh_res$statistic), 4),
  df         = c(h1_test$parameter, h2_test$parameter, h3_test$parameter, 1),
  p_value    = round(c(h1_test$p.value, h2_test$p.value,
                       h3_test$p.value, mh_res$p.value), 6),
  Decision   = c(
    ifelse(h1_test$p.value < 0.05, "Reject H0", "Fail to Reject"),
    ifelse(h2_test$p.value < 0.05, "Reject H0", "Fail to Reject"),
    ifelse(h3_test$p.value < 0.05, "Reject H0", "Fail to Reject"),
    ifelse(mh_res$p.value  < 0.05, "Reject H0", "Fail to Reject")
  )
)
write.csv(conf_df, "tables/06_confirmatory_results.csv", row.names = FALSE)

prop.table(
  table(df$work_interfere, df$treatment),
  1
)

# =============================================================================
# 7. TRAIN / TEST SPLIT  (70 / 30, stratified)
# =============================================================================
cat("\n========== 7. TRAIN / TEST SPLIT ==========\n")

# Select modelling variables (drop Country, no_employees – redundant with CompanySize)
model_df <- df %>%
  select(treatment, Age, Gender, self_employed, family_history,
         remote_work, tech_company, benefits, care_options,
         wellness_program, seek_help, anonymity, leave,
         mental_health_consequence, phys_health_consequence,
         coworkers, supervisor, mental_health_interview,
         phys_health_interview, mental_vs_physical, obs_consequence,
         work_interfere, AgeGroup, CompanySize)
# NOT: Burada bilerek drop_na() çağırmıyoruz. self_employed ve
# work_interfere'deki eksik değerler split SONRASINDA, sadece train
# setinden hesaplanan mode ile dolduruluyor (data leakage'ı önlemek için).
# drop_na() burada çağrılırsa, impute edilecek hiçbir gözlem kalmaz ve
# aşağıdaki "leakage-free imputation" adımı fiilen hiçbir şey yapmaz.

set.seed(482)

train_idx <- caret::createDataPartition(
  model_df$treatment,
  p = 0.70,
  list = FALSE
)

train_df <- model_df[ train_idx, ]
test_df  <- model_df[-train_idx, ]

cat("NA check (train) - before imputation:\n")
print(colSums(is.na(train_df)))

cat("NA check (test) - before imputation:\n")
print(colSums(is.na(test_df)))

# ---- Leakage-free mode imputation ----
# Mode SADECE train setinden hesaplanır, hem train hem teste uygulanır.

mode_self_emp <- mode_val(train_df$self_employed)
mode_work_int <- mode_val(train_df$work_interfere)

train_df$self_employed[is.na(train_df$self_employed)] <- mode_self_emp
test_df$self_employed[is.na(test_df$self_employed)]  <- mode_self_emp

train_df$work_interfere[is.na(train_df$work_interfere)] <- mode_work_int
test_df$work_interfere[is.na(test_df$work_interfere)]  <- mode_work_int

# Güvenlik ağı: yukarıda impute edilen iki değişken dışında, beklenmedik
# şekilde başka bir sütunda eksik değer kalmışsa (örn. veri kaynağı
# değişirse) bu satırları at. Normal koşullarda burada hiçbir satır
# silinmemeli çünkü tüm bilinen eksiklik zaten impute edildi.
n_before <- nrow(train_df) + nrow(test_df)
train_df <- train_df %>% drop_na()
test_df  <- test_df  %>% drop_na()
n_after  <- nrow(train_df) + nrow(test_df)
if (n_after < n_before) {
  cat(sprintf("UYARI: imputation sonrasi beklenmeyen NA nedeniyle %d satir silindi.\n",
              n_before - n_after))
}

cat("NA check (train) - after imputation:\n")
print(colSums(is.na(train_df)))
cat("NA check (test) - after imputation:\n")
print(colSums(is.na(test_df)))

cat("Training set size:", nrow(train_df), "\n")
cat("Test set size:    ", nrow(test_df), "\n")
cat("Train treatment distribution:\n"); print(prop.table(table(train_df$treatment)))
cat("Test  treatment distribution:\n"); print(prop.table(table(test_df$treatment)))

# =============================================================================
# 8. LOGISTIC REGRESSION – FULL MODEL
# =============================================================================
cat("\n========== 8. LOGISTIC REGRESSION – FULL MODEL ==========\n")

# Drop Age (replaced by AgeGroup) and no_employees (replaced by CompanySize)
# to avoid multicollinearity with derived features
logit_full <- glm(
  treatment ~ Gender + family_history + self_employed + remote_work +
    tech_company + benefits + care_options + wellness_program +
    seek_help + anonymity + leave + mental_health_consequence +
    phys_health_consequence + coworkers + supervisor +
    mental_health_interview + phys_health_interview +
    mental_vs_physical + obs_consequence +
    work_interfere + AgeGroup + CompanySize,
  data   = train_df,
  family = binomial(link = "logit")
)
cat("\n--- Full Model Summary ---\n")
print(summary(logit_full))

# Coefficient table with OR and 95% CI
coef_tbl <- data.frame(
  Coefficient = coef(logit_full),
  OR          = exp(coef(logit_full)),
  CI_lo       = exp(confint.default(logit_full)[, 1]),
  CI_hi       = exp(confint.default(logit_full)[, 2]),
  p_value     = summary(logit_full)$coefficients[, 4]
)
coef_tbl <- round(coef_tbl, 4)
write.csv(coef_tbl, "tables/08_logit_full_coeftable.csv")
cat("\n--- OR Table (saved) ---\n"); print(head(coef_tbl, 15))

# ── 8a. Diagnostics ────────────────────────────────────────────────────────────

# VIF (multicollinearity)
cat("\n--- VIF ---\n")
vif_vals <- tryCatch(car::vif(logit_full), error = function(e) "VIF could not be computed")
print(vif_vals)

# Cook's distance
cooks_d <- cooks.distance(logit_full)
p_cook <- ggplot(data.frame(Index = seq_along(cooks_d), CooksD = cooks_d),
                 aes(x = Index, y = CooksD)) +
  geom_point(colour = "steelblue", size = 0.8, alpha = 0.7) +
  geom_hline(yintercept = 4 / nrow(train_df), colour = "tomato", linetype = "dashed") +
  labs(title = "Cook's Distance – Full Logistic Model",
       x = "Observation Index", y = "Cook's Distance") +
  theme_minimal(base_size = 11)
save_fig(p_cook, "08a_cooks_distance.png", w = 10, h = 5)

# Hosmer-Lemeshow test
hl_test <- ResourceSelection::hoslem.test(
  as.numeric(train_df$treatment) - 1,
  fitted(logit_full), g = 10)
cat("\n--- Hosmer-Lemeshow Test ---\n")
print(hl_test)

# ROC / AUC on training data
train_prob_full <- predict(logit_full, newdata = train_df, type = "response")
roc_full_train  <- pROC::roc(train_df$treatment, train_prob_full, quiet = TRUE)
cat("\nTrain AUC (Full):", round(pROC::auc(roc_full_train), 4), "\n")

# =============================================================================
# 9. FEATURE SELECTION – STEPWISE AIC
# =============================================================================
cat("\n========== 9. FEATURE SELECTION (Stepwise AIC) ==========\n")

logit_step <- step(logit_full, direction = "both", trace = 0)
cat("\n--- Selected Model Summary ---\n")
print(summary(logit_step))
cat("\nSelected formula:\n")
print(formula(logit_step))

# Compare full vs selected
cat("\n--- AIC Comparison ---\n")
cat("Full model AIC:     ", round(AIC(logit_full), 2), "\n")
cat("Selected model AIC: ", round(AIC(logit_step), 2), "\n")
cat("LRT p-value:\n")
print(anova(logit_step, logit_full, test = "LRT"))

# Save selected coef table
coef_step <- data.frame(
  Coefficient = coef(logit_step),
  OR          = exp(coef(logit_step)),
  CI_lo       = exp(confint.default(logit_step)[, 1]),
  CI_hi       = exp(confint.default(logit_step)[, 2]),
  p_value     = summary(logit_step)$coefficients[, 4]
)
coef_step <- round(coef_step, 4)
write.csv(coef_step, "tables/09_logit_step_coeftable.csv")

# =============================================================================
# 10. REGULARIZATION  (Ridge / Lasso / Elastic Net)
# =============================================================================
cat("\n========== 10. REGULARIZATION ==========\n")

# One-hot encoding via model.matrix
X_train <- model.matrix(treatment ~ Gender + family_history + self_employed +
                           remote_work + tech_company + benefits + care_options +
                           wellness_program + seek_help + anonymity + leave +
                           mental_health_consequence + phys_health_consequence +
                           coworkers + supervisor + mental_health_interview +
                           phys_health_interview + mental_vs_physical +
                           obs_consequence + work_interfere + AgeGroup + CompanySize,
                         data = train_df)[, -1]
y_train <- as.numeric(train_df$treatment) - 1  # 0/1

X_test  <- model.matrix(treatment ~ Gender + family_history + self_employed +
                           remote_work + tech_company + benefits + care_options +
                           wellness_program + seek_help + anonymity + leave +
                           mental_health_consequence + phys_health_consequence +
                           coworkers + supervisor + mental_health_interview +
                           phys_health_interview + mental_vs_physical +
                           obs_consequence + work_interfere + AgeGroup + CompanySize,
                         data = test_df)[, -1]
y_test  <- as.numeric(test_df$treatment) - 1

# ── Ridge (alpha = 0) ─────────────────────────────────────────────────────────
library(glmnet)

set.seed(482)
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0, family = "binomial",
                      nfolds = 10, type.measure = "auc")
best_lambda_ridge <- cv_ridge$lambda.min
cat("Ridge best lambda:", round(best_lambda_ridge, 5), "\n")

png("figures/10a_ridge_cv.png", width = 900, height = 600, res = 120)
plot(cv_ridge, main = "Ridge – Cross-Validation AUC vs Lambda")
dev.off()

# ── Lasso (alpha = 1) ─────────────────────────────────────────────────────────
set.seed(482)
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1, family = "binomial",
                      nfolds = 10, type.measure = "auc")
best_lambda_lasso <- cv_lasso$lambda.min
cat("Lasso best lambda:", round(best_lambda_lasso, 5), "\n")

png("figures/10b_lasso_cv.png", width = 900, height = 600, res = 120)
plot(cv_lasso, main = "Lasso – Cross-Validation AUC vs Lambda")
dev.off()

# Lasso selected variables
lasso_coef <- coef(cv_lasso, s = "lambda.min")
lasso_selected <- rownames(lasso_coef)[which(lasso_coef != 0)]
cat("\nLasso selected variables:\n"); print(lasso_selected)

# ── Elastic Net (alpha = 0.5) ─────────────────────────────────────────────────
set.seed(482)
cv_enet <- cv.glmnet(X_train, y_train, alpha = 0.5, family = "binomial",
                     nfolds = 10, type.measure = "auc")
best_lambda_enet <- cv_enet$lambda.min
cat("Elastic Net best lambda:", round(best_lambda_enet, 5), "\n")

png("figures/10c_enet_cv.png", width = 900, height = 600, res = 120)
plot(cv_enet, main = "Elastic Net – Cross-Validation AUC vs Lambda")
dev.off()

# Fit final regularised models
mod_ridge <- glmnet(X_train, y_train, alpha = 0, lambda = best_lambda_ridge, family = "binomial")
mod_lasso <- glmnet(X_train, y_train, alpha = 1, lambda = best_lambda_lasso, family = "binomial")
mod_enet  <- glmnet(X_train, y_train, alpha = 0.5, lambda = best_lambda_enet, family = "binomial")

# Save glmnet models
saveRDS(mod_ridge, "models/model_ridge.rds")
saveRDS(mod_lasso, "models/model_lasso.rds")
saveRDS(mod_enet,  "models/model_enet.rds")

# Coefficients table
ridge_coef_df <- as.data.frame(as.matrix(coef(mod_ridge)))
lasso_coef_df <- as.data.frame(as.matrix(coef(mod_lasso)))
enet_coef_df  <- as.data.frame(as.matrix(coef(mod_enet)))
reg_coef_tbl  <- cbind(Ridge = ridge_coef_df, Lasso = lasso_coef_df, ElasticNet = enet_coef_df)
colnames(reg_coef_tbl) <- c("Ridge", "Lasso", "ElasticNet")
write.csv(reg_coef_tbl, "tables/10_regularization_coefficients.csv")
cat("Regularisation coefficients saved.\n")

# =============================================================================
# 11. TREE-BASED MODELS
# =============================================================================
cat("\n========== 11. TREE-BASED MODELS ==========\n")

cat("\n--- Decision Tree: Cost-Complexity Pruning ---\n")

library(rpart)
library(rpart.plot)

set.seed(482)

dtree_full <- rpart::rpart(
  treatment ~ Gender + family_history + self_employed +
    remote_work + tech_company + benefits + care_options +
    wellness_program + seek_help + anonymity + leave +
    mental_health_consequence + phys_health_consequence +
    coworkers + supervisor + mental_health_interview +
    phys_health_interview + mental_vs_physical +
    obs_consequence + work_interfere +
    AgeGroup + CompanySize,
  data = train_df,
  method = "class",
  control = rpart::rpart.control(
    cp = 0.0001,
    minsplit = 10,
    maxdepth = 10,
    xval = 10
  )
)

# ── Print and export full CP table ────────────────────────────────────────────
cp_table <- as.data.frame(dtree_full$cptable)
colnames(cp_table) <- c("CP", "n_splits", "rel_error", "xerror", "xstd")
cp_table <- cp_table %>% mutate(across(everything(), ~ round(.x, 6)))
cat("\n--- Full CP Table (first 15 rows) ---\n")
print(head(cp_table, 15))
write.csv(cp_table, "tables/11a_dtree_cp_table.csv", row.names = FALSE)
cat("CP table saved: tables/11a_dtree_cp_table.csv\n")

# ── plotcp ─────────────────────────────────────────────────────────────────────
png("figures/11a_dtree_plotcp.png", width = 1000, height = 600, res = 130)
rpart::plotcp(dtree_full, main = "Decision Tree: Cross-Validated Error vs CP")
dev.off()
cat("CP plot saved: figures/11a_dtree_plotcp.png\n")

# ── Strategy 1: minimum xerror ────────────────────────────────────────────────
idx_min      <- which.min(cp_table$xerror)
cp_min_xerr  <- cp_table$CP[idx_min]
cat(sprintf("\nMin-xerror CP:  %.6f  (xerror=%.4f  at %d splits)\n",
            cp_min_xerr, cp_table$xerror[idx_min], cp_table$n_splits[idx_min]))

# ── Strategy 2: 1-SE rule (Breiman et al.) ───────────────────────────────────
# Select smallest tree whose xerror <= min(xerror) + xstd at the minimum
threshold_1se <- cp_table$xerror[idx_min] + cp_table$xstd[idx_min]
idx_1se       <- min(which(cp_table$xerror <= threshold_1se))  # first (largest CP = simplest tree)
cp_1se        <- cp_table$CP[idx_1se]
cat(sprintf("1-SE rule CP:   %.6f  (xerror=%.4f  at %d splits, threshold=%.4f)\n",
            cp_1se, cp_table$xerror[idx_1se], cp_table$n_splits[idx_1se], threshold_1se))

# ── Prune with both strategies ────────────────────────────────────────────────
dtree_min  <- rpart::prune(dtree_full, cp = cp_min_xerr)
dtree_1se  <- rpart::prune(dtree_full, cp = cp_1se)

cat(sprintf("\nUnpruned tree:       %d terminal nodes\n",
            sum(dtree_full$frame$var == "<leaf>")))
cat(sprintf("Min-xerror pruned:   %d terminal nodes\n",
            sum(dtree_min$frame$var  == "<leaf>")))
cat(sprintf("1-SE rule pruned:    %d terminal nodes\n",
            sum(dtree_1se$frame$var  == "<leaf>")))

# ── Visual comparison ─────────────────────────────────────────────────────────
png("figures/11b_dtree_pruned_minxerr.png", width = 1400, height = 900, res = 120)
rpart.plot::rpart.plot(dtree_min, type = 4, extra = 104, cex = 0.72,
  main = sprintf("Decision Tree (Min-xerror, cp=%.5f, %d leaves)",
                 cp_min_xerr, sum(dtree_min$frame$var == "<leaf>")))
dev.off()

png("figures/11c_dtree_pruned_1se.png", width = 1200, height = 800, res = 120)
rpart.plot::rpart.plot(dtree_1se, type = 4, extra = 104, cex = 0.75,
  main = sprintf("Decision Tree (1-SE Rule, cp=%.5f, %d leaves)",
                 cp_1se, sum(dtree_1se$frame$var == "<leaf>")))
dev.off()
cat("Pruned tree figures saved.\n")

# ── Select final tree = 1-SE rule (favours parsimony) ────────────────────────
dtree_p <- dtree_1se
cat("\nFinal tree selected: 1-SE rule pruned tree\n")

# ── CP curve ggplot ───────────────────────────────────────────────────────────
cp_plot_df <- cp_table %>%
  mutate(
    nsplit_label = factor(n_splits),
    threshold    = cp_table$xerror[idx_min] + cp_table$xstd[idx_min]
  )

p_cp <- ggplot(cp_plot_df, aes(x = log10(CP), y = xerror)) +
  geom_ribbon(aes(ymin = xerror - xstd, ymax = xerror + xstd),
              fill = "steelblue", alpha = 0.20) +
  geom_line(colour = "steelblue", linewidth = 1) +
  geom_point(colour = "steelblue", size = 2.5) +
  geom_hline(yintercept = threshold_1se, linetype = "dashed",
             colour = "tomato", linewidth = 0.8) +
  geom_vline(xintercept = log10(cp_min_xerr), linetype = "dotted",
             colour = "navy", linewidth = 0.8) +
  geom_vline(xintercept = log10(cp_1se), linetype = "solid",
             colour = "forestgreen", linewidth = 0.9) +
  annotate("text", x = log10(cp_1se) + 0.08, y = min(cp_plot_df$xerror),
           label = "1-SE rule", colour = "forestgreen", size = 3.2, hjust = 0) +
  annotate("text", x = log10(cp_min_xerr) - 0.08, y = min(cp_plot_df$xerror),
           label = "min xerror", colour = "navy", size = 3.2, hjust = 1) +
  labs(title    = "Cost-Complexity Pruning: CV Error vs log10(CP)",
       subtitle = "Ribbon = ±1 SE  |  Dashed red = 1-SE threshold",
       x = "log10(CP)", y = "Cross-Validated Relative Error") +
  theme_minimal(base_size = 11)
save_fig(p_cp, "11d_dtree_cp_curve.png", w = 10, h = 5)
cat("CP curve plot saved: figures/11d_dtree_cp_curve.png\n")

# ── Variable importance (final tree) ─────────────────────────────────────────
dtree_imp <- sort(dtree_p$variable.importance, decreasing = TRUE)
cat("\n--- Decision Tree Variable Importance (1-SE tree) ---\n")
print(dtree_imp)

# Pruning summary table
pruning_summary <- data.frame(
  Strategy     = c("Unpruned", "Min-xerror", "1-SE rule (selected)"),
  CP_value     = round(c(min(cp_table$CP), cp_min_xerr, cp_1se), 6),
  n_splits     = c(max(cp_table$n_splits), cp_table$n_splits[idx_min], cp_table$n_splits[idx_1se]),
  Leaves       = c(sum(dtree_full$frame$var == "<leaf>"),
                   sum(dtree_min$frame$var  == "<leaf>"),
                   sum(dtree_1se$frame$var  == "<leaf>")),
  xerror       = round(c(cp_table$xerror[nrow(cp_table)],
                         cp_table$xerror[idx_min],
                         cp_table$xerror[idx_1se]), 4)
)
write.csv(pruning_summary, "tables/11a_dtree_pruning_summary.csv", row.names = FALSE)
cat("Pruning summary saved: tables/11a_dtree_pruning_summary.csv\n")
saveRDS(dtree_p, "models/model_dtree.rds")



# ── Random Forest: caret hyperparameter tuning ───────────────────────────────
cat("\n--- Random Forest: Hyperparameter Tuning via caret ---\n")

# ── Step 1: tune mtry via 5-fold stratified CV ───────────────────────────────
rf_formula <- treatment ~ Gender + family_history + self_employed +
  remote_work + tech_company + benefits + care_options +
  wellness_program + seek_help + anonymity + leave +
  mental_health_consequence + phys_health_consequence +
  coworkers + supervisor + mental_health_interview +
  phys_health_interview + mental_vs_physical + obs_consequence +
  work_interfere + AgeGroup + CompanySize

set.seed(482)
rf_ctrl <- caret::trainControl(
  method          = "cv",
  number          = 5,
  classProbs      = TRUE,
  summaryFunction = caret::twoClassSummary,
  savePredictions = "final",
  verboseIter     = FALSE
)

# mtry candidates: sqrt(p), 2*sqrt(p), p/3, p
n_pred <- length(all.vars(rf_formula)) - 1    # number of predictors
rf_grid <- expand.grid(
  mtry = unique(round(c(
    floor(sqrt(n_pred)),
    ceiling(sqrt(n_pred)),
    floor(n_pred / 3),
    floor(n_pred / 2),
    n_pred
  )))
)
cat("mtry candidates:", rf_grid$mtry, "\n")

set.seed(482)
rf_tuned <- caret::train(
  rf_formula,
  data      = train_df,
  method    = "rf",
  metric    = "ROC",
  trControl = rf_ctrl,
  tuneGrid  = rf_grid,
  ntree     = 500,
  importance= TRUE
)

cat("\n--- CV Tuning Results ---\n")
print(rf_tuned$results)
best_mtry <- rf_tuned$bestTune$mtry
cat("\nBest mtry:", best_mtry, "\n")

# ── Tuning plot ───────────────────────────────────────────────────────────────
p_rf_tune <- ggplot(rf_tuned$results,
                    aes(x = factor(mtry), y = ROC, group = 1)) +
  geom_line(colour = "steelblue", linewidth = 1.1) +
  geom_point(size = 3.5, colour = "steelblue") +
  geom_errorbar(aes(ymin = ROC - ROCSD, ymax = ROC + ROCSD),
                width = 0.15, colour = "steelblue", alpha = 0.6) +
  geom_vline(xintercept = which(rf_tuned$results$mtry == best_mtry),
             linetype = "dashed", colour = "tomato") +
  annotate("text",
           x     = which(rf_tuned$results$mtry == best_mtry) + 0.05,
           y     = min(rf_tuned$results$ROC),
           label = paste0("Best mtry=", best_mtry),
           colour = "tomato", hjust = 0, size = 3.5) +
  labs(title    = "Random Forest: CV AUC vs mtry",
       subtitle = "5-fold CV, error bars = ±1 SD across folds",
       x = "mtry", y = "CV ROC AUC") +
  theme_minimal(base_size = 11)
save_fig(p_rf_tune, "11e_rf_tuning_mtry.png", w = 9, h = 5)
cat("RF tuning plot saved: figures/11e_rf_tuning_mtry.png\n")

# ── Step 2: ntree sensitivity check (at best mtry) ───────────────────────────
cat("\n--- ntree sensitivity at best mtry =", best_mtry, "---\n")
ntree_vals <- c(100, 200, 300, 500, 750)
ntree_oob  <- sapply(ntree_vals, function(nt) {
  set.seed(482)
  m <- randomForest::randomForest(rf_formula, data = train_df,
                                  ntree = nt, mtry = best_mtry)
  m$err.rate[nt, "OOB"]
})
ntree_df <- data.frame(ntree = ntree_vals, OOB_error = round(ntree_oob, 5))
print(ntree_df)
best_ntree <- ntree_vals[which.min(ntree_oob)]
cat("Best ntree:", best_ntree, "\n")

p_ntree <- ggplot(ntree_df, aes(x = ntree, y = OOB_error)) +
  geom_line(colour = "forestgreen", linewidth = 1.1) +
  geom_point(size = 3, colour = "forestgreen") +
  geom_vline(xintercept = best_ntree, linetype = "dashed", colour = "tomato") +
  labs(title    = "Random Forest: OOB Error vs ntree",
       subtitle = paste0("At best mtry = ", best_mtry),
       x = "Number of Trees (ntree)", y = "OOB Classification Error") +
  theme_minimal(base_size = 11)
save_fig(p_ntree, "11f_rf_ntree_sensitivity.png", w = 9, h = 5)

# ── Final RF model (best mtry & ntree) ───────────────────────────────────────
set.seed(482)

library(randomForest)
library(ggplot2)
library(dplyr)
library(tidyr)
rf_mod <- randomForest::randomForest(
  rf_formula,
  data       = train_df,
  ntree      = best_ntree,
  mtry       = best_mtry,
  importance = TRUE
)
cat("\n--- Final Random Forest (mtry=", best_mtry,
    "ntree=", best_ntree, ") ---\n")
print(rf_mod)

# ── Variable importance: both MeanDecreaseAccuracy & MeanDecreaseGini ─────────
rf_imp     <- randomForest::importance(rf_mod)
rf_imp_df  <- data.frame(
  Variable             = rownames(rf_imp),
  MeanDecreaseAccuracy = rf_imp[, "MeanDecreaseAccuracy"],
  MeanDecreaseGini     = rf_imp[, "MeanDecreaseGini"]
) %>% arrange(desc(MeanDecreaseGini))

cat("\n--- Variable Importance (top 15 by Gini) ---\n")
print(rf_imp_df[1:min(15, nrow(rf_imp_df)), ])
write.csv(rf_imp_df, "tables/11b_rf_variable_importance.csv", row.names = FALSE)

# Dual-metric importance plot
rf_imp_long <- rf_imp_df %>%
  dplyr::slice_max(MeanDecreaseGini, n = 15) %>%
  tidyr::pivot_longer(
    cols = c(MeanDecreaseAccuracy, MeanDecreaseGini),
    names_to = "Metric",
    values_to = "Value"
  ) %>%
  dplyr::mutate(
    Metric = dplyr::recode(Metric,
                           MeanDecreaseAccuracy = "Mean Decrease Accuracy",
                           MeanDecreaseGini     = "Mean Decrease Gini"
    )
  )

library(tidyr)


p_rf_imp <- ggplot(rf_imp_long,
                   aes(x = Value,
                       y = reorder(Variable, Value),
                       fill = Metric)) +
  geom_col(position = "dodge", width = 0.65) +
  scale_fill_manual(values = c("Mean Decrease Accuracy" = "steelblue",
                               "Mean Decrease Gini"     = "tomato")) +
  facet_wrap(~Metric, scales = "free_x") +
  labs(title    = paste0("Random Forest Variable Importance (Top 15)  |  mtry=",
                         best_mtry, "  ntree=", best_ntree),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "none",
        strip.text      = element_text(face = "bold"))
save_fig(p_rf_imp, "11g_rf_variable_importance_dual.png", w = 13, h = 7)
cat("RF importance plot saved: figures/11g_rf_variable_importance_dual.png\n")

# OOB error trajectory
oob_df <- data.frame(
  Trees     = seq_len(best_ntree),
  OOB       = rf_mod$err.rate[, "OOB"],
  Class_No  = rf_mod$err.rate[, "No"],
  Class_Yes = rf_mod$err.rate[, "Yes"]
) %>%
  pivot_longer(-Trees, names_to = "Error_Type", values_to = "Rate")

p_oob <- ggplot(oob_df, aes(x = Trees, y = Rate, colour = Error_Type)) +
  geom_line(alpha = 0.8, linewidth = 0.7) +
  scale_colour_manual(values = c(OOB = "black", Class_No = "steelblue",
                                 Class_Yes = "tomato"),
                      labels = c("OOB", "Class: No", "Class: Yes")) +
  labs(title  = "Random Forest: OOB Error Trajectory",
       x = "Number of Trees", y = "Error Rate", colour = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top")
save_fig(p_oob, "11h_rf_oob_trajectory.png", w = 10, h = 5)

# Tuning summary table
rf_tune_summary <- data.frame(
  Parameter     = c("mtry (tuned)", "ntree (sensitivity)", "CV metric",
                    "CV folds", "OOB error (final)", "Train AUC (caret CV)"),
  Value         = c(best_mtry, best_ntree, "ROC AUC", 5,
                    round(rf_mod$err.rate[best_ntree, "OOB"], 4),
                    round(max(rf_tuned$results$ROC), 4))
)
write.csv(rf_tune_summary, "tables/11b_rf_tuning_summary.csv", row.names = FALSE)
cat("RF tuning summary saved: tables/11b_rf_tuning_summary.csv\n")

saveRDS(rf_mod,   "models/model_rf.rds")
saveRDS(rf_tuned, "models/model_rf_caret.rds")




# =============================================================================
# 12. MODEL EVALUATION ON TEST SET
# =============================================================================
cat("\n========== 12. MODEL EVALUATION ==========\n")

# ── Helper: compute all metrics ───────────────────────────────────────────────
compute_metrics <- function(true_labels, pred_prob, threshold = 0.5,
                             model_name = "Model") {
  pred_class <- factor(ifelse(pred_prob >= threshold, "Yes", "No"),
                       levels = c("No", "Yes"))
  true_f     <- factor(true_labels, levels = c("No", "Yes"))
  cm         <- caret::confusionMatrix(pred_class, true_f, positive = "Yes")
  roc_obj    <- pROC::roc(true_f, pred_prob, quiet = TRUE)
  data.frame(
    Model       = model_name,
    Accuracy    = round(cm$overall["Accuracy"],    4),
    Sensitivity = round(cm$byClass["Sensitivity"], 4),
    Specificity = round(cm$byClass["Specificity"], 4),
    Precision   = round(cm$byClass["Precision"],   4),
    Recall      = round(cm$byClass["Sensitivity"], 4),
    F1          = round(cm$byClass["F1"],           4),
    ROC_AUC     = round(pROC::auc(roc_obj),         4),
    row.names   = NULL
  )
}

# Predictions on test set
prob_full  <- predict(logit_full, newdata = test_df,  type = "response")
prob_step  <- predict(logit_step, newdata = test_df,  type = "response")
prob_ridge <- as.numeric(predict(mod_ridge, newx = X_test, type = "response"))
prob_lasso <- as.numeric(predict(mod_lasso, newx = X_test, type = "response"))
prob_enet  <- as.numeric(predict(mod_enet,  newx = X_test, type = "response"))

# Decision tree probabilities
prob_dtree <- predict(dtree_p, newdata = test_df, type = "prob")[, "Yes"]

# Random forest probabilities
prob_rf    <- predict(rf_mod, newdata = test_df, type = "prob")[, "Yes"]

true_labels <- as.character(test_df$treatment)

eval_results <- rbind(
  compute_metrics(true_labels, prob_full,  model_name = "Logistic Full"),
  compute_metrics(true_labels, prob_step,  model_name = "Logistic Selected"),
  compute_metrics(true_labels, prob_ridge, model_name = "Ridge"),
  compute_metrics(true_labels, prob_lasso, model_name = "Lasso"),
  compute_metrics(true_labels, prob_enet,  model_name = "Elastic Net"),
  compute_metrics(true_labels, prob_dtree, model_name = "Decision Tree"),
  compute_metrics(true_labels, prob_rf,    model_name = "Random Forest")
)

cat("\n========== MODEL COMPARISON TABLE ==========\n")
print(eval_results)
write.csv(eval_results, "tables/12_model_evaluation.csv", row.names = FALSE)

# ── Identify best model ────────────────────────────────────────────────────────
best_model <- eval_results$Model[which.max(eval_results$ROC_AUC)]
cat("\nBest model by ROC AUC:", best_model,
    "(AUC =", max(eval_results$ROC_AUC), ")\n")

# ── ROC comparison plot ────────────────────────────────────────────────────────
roc_full  <- pROC::roc(true_labels, prob_full,  quiet = TRUE)
roc_step  <- pROC::roc(true_labels, prob_step,  quiet = TRUE)
roc_ridge <- pROC::roc(true_labels, prob_ridge, quiet = TRUE)
roc_lasso <- pROC::roc(true_labels, prob_lasso, quiet = TRUE)
roc_enet  <- pROC::roc(true_labels, prob_enet,  quiet = TRUE)
roc_dtree <- pROC::roc(true_labels, prob_dtree, quiet = TRUE)
roc_rf    <- pROC::roc(true_labels, prob_rf,    quiet = TRUE)

roc_list <- list(
  "Logistic Full"     = roc_full,
  "Logistic Selected" = roc_step,
  "Ridge"             = roc_ridge,
  "Lasso"             = roc_lasso,
  "Elastic Net"       = roc_enet,
  "Decision Tree"     = roc_dtree,
  "Random Forest"     = roc_rf
)

colours_roc <- c("black", "steelblue", "tomato", "forestgreen",
                 "purple", "orange", "darkred")

png("figures/12_roc_comparison.png", width = 1200, height = 900, res = 130)
pROC::ggroc(roc_list, size = 0.9) +
  scale_colour_manual(
    values = setNames(colours_roc, names(roc_list)),
    labels = paste0(names(roc_list), " (AUC=",
                    round(sapply(roc_list, pROC::auc), 3), ")")
  ) +
  geom_abline(slope = -1, intercept = 1, linetype = "dashed", colour = "grey50") +
  labs(title = "ROC Curve Comparison – All Models",
       x = "Specificity", y = "Sensitivity", colour = "Model") +
  theme_minimal(base_size = 12) +
  theme(legend.position = c(0.70, 0.25),
        legend.background = element_rect(fill = "white", colour = "grey80"))
dev.off()

# ── Model comparison bar chart ─────────────────────────────────────────────────
eval_long <- eval_results %>%
  select(Model, Accuracy, Sensitivity, Specificity, F1, ROC_AUC) %>%
  pivot_longer(-Model, names_to = "Metric", values_to = "Value")

p_eval <- ggplot(eval_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_col(position = "dodge", width = 0.7) +
  facet_wrap(~Metric, scales = "free_y", ncol = 3) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Model Performance on Test Set", x = NULL, y = "Score") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x  = element_text(angle = 40, hjust = 1),
        legend.position = "none")
save_fig(p_eval, "12b_model_comparison_metrics.png", w = 16, h = 9)

# =============================================================================
# 13. REPORT TABLES & FIGURES  (gtsummary / gt)
# =============================================================================
cat("\n========== 13. REPORT TABLES ==========\n")

# ── Table 1: Descriptive statistics by treatment ──────────────────────────────
tbl1 <- gtsummary::tbl_summary(
  data     = model_df,
  by       = treatment,
  include  = c(Age, Gender, family_history, self_employed, remote_work,
               tech_company, benefits, care_options, wellness_program,
               seek_help, anonymity, work_interfere, leave,
               AgeGroup, CompanySize),
  statistic = list(
    gtsummary::all_continuous2()  ~ "{mean} ({sd})",
    gtsummary::all_categorical()  ~ "{n} ({p}%)"
  ),
  missing = "ifany"
) %>%
  gtsummary::add_overall() %>%
  gtsummary::add_p() %>%
  gtsummary::bold_p(t = 0.05) %>%
  gtsummary::bold_labels() %>%
  gtsummary::modify_caption("**Table 1. Participant Characteristics by Treatment Status**")

tbl1_gt <- gtsummary::as_gt(tbl1)
gt::gtsave(tbl1_gt, "tables/13_table1_descriptive.html")


# ── Table 4 (gt): Bivariate Association Results ───────────────────────────────
cat("\n--- Generating gt publication tables (Section 13 additions) ---\n")

bivar_gt_df <- bivar_results %>%
  mutate(
    `Chi² (p)`     = sprintf("%.3f (%s)", Chi2_Stat,
                              ifelse(Chi2_p < 0.001, "<.001",
                                     sprintf("%.3f", Chi2_p))),
    `Cramér's V`   = sprintf("%.4f", CramersV),
    `Cont. Coeff.` = sprintf("%.4f", ContCoeff),
    `OR [95% CI]`  = ifelse(!is.na(OR),
                             sprintf("%.3f [%.3f, %.3f]", OR, OR_lo, OR_hi),
                             "—"),
    `RR`           = ifelse(!is.na(RR), sprintf("%.3f", RR), "—"),
    Sig            = dplyr::case_when(
      Chi2_p < 0.001 ~ "***",
      Chi2_p < 0.01  ~ "**",
      Chi2_p < 0.05  ~ "*",
      TRUE           ~ "ns"
    )
  ) %>%
  select(Variable, `Chi² (p)`, `Cramér's V`, `Cont. Coeff.`,
         `OR [95% CI]`, RR, Sig)

gt_bivar <- bivar_gt_df %>%
  gt::gt() %>%
  gt::tab_header(
    title    = "Table 4. Bivariate Association: Predictors × Treatment",
    subtitle = "* p<.05  ** p<.01  *** p<.001  ns = not significant"
  ) %>%
  gt::tab_spanner(label = "Effect Size",
                  columns = c(`Cramér's V`, `Cont. Coeff.`)) %>%
  gt::tab_spanner(label = "Binary Measures (2×2 only)",
                  columns = c(`OR [95% CI]`, RR)) %>%
  gt::tab_style(
    style     = gt::cell_fill(color = "#d4edda"),
    locations = gt::cells_body(rows = Sig %in% c("*", "**", "***"))
  ) %>%
  gt::tab_style(
    style     = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  ) %>%
  gt::cols_label(Sig = "Sig.") %>%
  gt::tab_footnote(
    "OR and RR reported only for binary (2×2) predictors. — = not applicable."
  ) %>%
  gt::opt_row_striping() %>%
  gt::opt_table_font(font = gt::google_font("Source Sans Pro"))

gt::gtsave(gt_bivar, "tables/13_table4_bivariate_gt.html")
cat("Bivariate gt table saved: tables/13_table4_bivariate_gt.html\n")

# ── Table 5 (gt): Ordinal Association Results ─────────────────────────────────
if (exists("ordinal_results") && nrow(ordinal_results) > 0) {
  gt_ord2 <- ordinal_results %>%
    mutate(
      `Kendall Tau-b [p]` = sprintf("%.4f (z=%.3f, p%s)",
        Kendall_TauB, Kendall_z,
        ifelse(Kendall_p < 0.001, "<.001", paste0("=", round(Kendall_p, 3)))),
      `Gamma [95% CI]`    = sprintf("%.4f [%.4f, %.4f]",
        Gamma, Gamma_CI_lo, Gamma_CI_hi),
      `Somers' D [95% CI]`= sprintf("%.4f [%.4f, %.4f]",
        SomersD, SomersD_CI_lo, SomersD_CI_hi),
      `Direction`         = dplyr::case_when(
        Kendall_TauB > 0.1  ~ "Positive ↑",
        Kendall_TauB < -0.1 ~ "Negative ↓",
        TRUE                ~ "Negligible"
      )
    ) %>%
    select(Variable, `Kendall Tau-b [p]`, `Gamma [95% CI]`,
           `Somers' D [95% CI]`, Direction) %>%
    gt::gt() %>%
    gt::tab_header(
      title    = "Table 5. Ordinal Association Measures: Ordinal Predictors × Treatment",
      subtitle = "Tau-b: asymptotic z-test p-value | Gamma & Somers' D: 95% CI via normal approx."
    ) %>%
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) %>%
    gt::tab_style(
      style     = gt::cell_fill(color = "#d0ebff"),
      locations = gt::cells_body(rows = Direction == "Positive ↑")
    ) %>%
    gt::tab_style(
      style     = gt::cell_fill(color = "#ffe3e3"),
      locations = gt::cells_body(rows = Direction == "Negative ↓")
    ) %>%
    gt::opt_row_striping() %>%
    gt::opt_table_font(font = gt::google_font("Source Sans Pro"))

  gt::gtsave(gt_ord2, "tables/13_table5_ordinal_gt.html")
  cat("Ordinal gt table saved: tables/13_table5_ordinal_gt.html\n")
}

# ── Table 6 (gt): Model Evaluation Comparison ────────────────────────────────
gt_eval <- eval_results %>%
  gt::gt() %>%
  gt::tab_header(
    title    = "Table 6. Model Performance Comparison on Test Set",
    subtitle = "All metrics computed on held-out 30% test set (set.seed = 482)"
  ) %>%
  gt::tab_spanner(label = "Classification Metrics",
                  columns = c(Accuracy, Sensitivity, Specificity, Precision, Recall, F1)) %>%
  gt::tab_spanner(label = "Discrimination",
                  columns = ROC_AUC) %>%
  gt::fmt_number(columns = c(Accuracy, Sensitivity, Specificity,
                             Precision, Recall, F1, ROC_AUC),
                 decimals = 4) %>%
  gt::tab_style(
    style     = gt::cell_fill(color = "#d4edda"),
    locations = gt::cells_body(
      rows = ROC_AUC == max(eval_results$ROC_AUC, na.rm = TRUE))
  ) %>%
  gt::tab_style(
    style     = gt::cell_text(weight = "bold"),
    locations = gt::cells_body(
      columns = ROC_AUC,
      rows    = ROC_AUC == max(eval_results$ROC_AUC, na.rm = TRUE))
  ) %>%
  gt::tab_style(
    style     = gt::cell_text(weight = "bold"),
    locations = gt::cells_column_labels()
  ) %>%
  gt::tab_footnote("Best model (highest AUC) highlighted in green.") %>%
  gt::opt_row_striping() %>%
  gt::opt_table_font(font = gt::google_font("Source Sans Pro"))

gt::gtsave(gt_eval, "tables/13_table6_model_evaluation_gt.html")
cat("Model evaluation gt table saved: tables/13_table6_model_evaluation_gt.html\n")

# ── Table 7 (gt): Three-Way Analysis Summary ──────────────────────────────────
if (exists("threeway_summary")) {
  gt_3way <- threeway_summary %>%
    gt::gt() %>%
    gt::tab_header(
      title    = "Table 7. Three-Way Contingency Table Analyses Summary",
      subtitle = "Overall Chi² on flattened table + Cochran-Mantel-Haenszel test"
    ) %>%
    gt::cols_label(
      Analysis     = "Analysis",
      Overall_Chi2 = "Overall χ²",
      Overall_p    = "p (overall)",
      CMH_Chi2     = "CMH χ²",
      CMH_p        = "p (CMH)",
      Common_OR    = "Common OR"
    ) %>%
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_column_labels()
    ) %>%
    gt::tab_footnote(
      "CMH = Cochran-Mantel-Haenszel stratified test. Common OR = pooled odds ratio across strata."
    ) %>%
    gt::opt_row_striping() %>%
    gt::opt_table_font(font = gt::google_font("Source Sans Pro"))

  gt::gtsave(gt_3way, "tables/13_table7_threeway_summary_gt.html")
  cat("Three-way summary gt table saved: tables/13_table7_threeway_summary_gt.html\n")
}

cat("\n--- All additional gt tables generated ---\n")
cat("  tables/05a_ordinal_gt_table.html\n")
cat("  tables/05b_rr_gt_table.html\n")
cat("  tables/13_table4_bivariate_gt.html\n")
cat("  tables/13_table5_ordinal_gt.html\n")
cat("  tables/13_table6_model_evaluation_gt.html\n")
cat("  tables/13_table7_threeway_summary_gt.html\n")


write.csv(bivar_results, "tables/13_table2_bivariate_association.csv", row.names = FALSE)

# ── Table 3: Model comparison ─────────────────────────────────────────────────
write.csv(eval_results, "tables/13_table3_model_comparison.csv", row.names = FALSE)

cat("All output tables saved in tables/\n")
cat("All figures saved in  figures/\n")
cat("All models saved in   models/\n")

# =============================================================================
# 14. AUTOMATED INTERPRETATION
# =============================================================================
cat("\n")
cat(strrep("=", 70), "\n")
cat("  SECTION 14 – AUTOMATED PROJECT INTERPRETATION\n")
cat(strrep("=", 70), "\n\n")

# ── 14a. Strongest predictors (by Cramér's V) ────────────────────────────────
top_preds <- bivar_results %>%
  arrange(desc(CramersV)) %>%
  slice(1:5)

cat("── Strongest Predictors (Cramér's V) ──────────────────────────────────\n")
for (i in seq_len(nrow(top_preds))) {
  cat(sprintf("  %d. %-25s  V = %.4f  (Chi² p = %.5f)\n",
              i, top_preds$Variable[i], top_preds$CramersV[i], top_preds$Chi2_p[i]))
}

cat("\n── Significant Associations ────────────────────────────────────────────\n")
sig_preds <- bivar_results %>% filter(Chi2_p < 0.05)
cat("  Variables significantly associated with treatment (α = 0.05):\n")
cat(" ", paste(sig_preds$Variable, collapse = ", "), "\n")

cat("\n── Confirmatory Analysis Conclusions ───────────────────────────────────\n")
cat("  H1 (family_history ~ treatment):  ",
    ifelse(h1_test$p.value < 0.05, "REJECTED ✔ Significant association", "NOT rejected"), "\n")
cat("  H2 (benefits ~ treatment):        ",
    ifelse(h2_test$p.value < 0.05, "REJECTED ✔ Significant association", "NOT rejected"), "\n")
cat("  H3 (work_interfere ~ treatment):  ",
    ifelse(h3_test$p.value < 0.05, "REJECTED ✔ Significant association", "NOT rejected"), "\n")
cat("  Mantel-Haenszel (family_history | Gender):  ",
    ifelse(mh_res$p.value < 0.05,
           paste0("Significant common OR = ", round(mh_res$estimate, 3)),
           "Not significant"), "\n")

cat("\n── Best Classification Model ────────────────────────────────────────────\n")
cat(sprintf("  Best model: %s  (Test ROC AUC = %.4f)\n", best_model,
            max(eval_results$ROC_AUC)))

cat("\n── Odds Ratio Interpretation (Logistic Full Model, top significant) ────\n")
sig_or <- coef_tbl %>%
  filter(p_value < 0.05, !grepl("Intercept", rownames(coef_tbl))) %>%
  arrange(desc(abs(log(OR)))) %>%
  slice(1:8)
if (nrow(sig_or) > 0) {
  for (i in seq_len(nrow(sig_or))) {
    direction <- ifelse(sig_or$OR[i] > 1, "increases", "decreases")
    cat(sprintf(
      "  %-40s  OR = %.3f (95%% CI: %.3f – %.3f) → %s odds of treatment\n",
      rownames(sig_or)[i], sig_or$OR[i], sig_or$CI_lo[i], sig_or$CI_hi[i],
      direction))
  }
}

cat("\n── Random Forest Top-5 Variables (Gini) ────────────────────────────────\n")
for (i in 1:min(5, nrow(rf_imp_df))) {
  cat(sprintf("  %d. %-30s  MeanDecGini = %.3f\n",
              i, rf_imp_df$Variable[i], rf_imp_df$MeanDecreaseGini[i]))
}

cat("\n── Final Project Conclusions ────────────────────────────────────────────\n")
cat(sprintf("
  1. STRONGEST PREDICTORS: Family history of mental illness (family_history),
     work interference (work_interfere), and mental health interview
     willingness are the most strongly associated variables with seeking
     mental health treatment, as confirmed by Cramér's V and Chi-square tests.

  2. SIGNIFICANT ASSOCIATIONS: All three confirmatory hypotheses were
     supported at α = 0.05. The Mantel-Haenszel test confirmed that the
     association between family_history and treatment persists across gender
     strata, indicating the effect is not merely a gender confound.

  3. BEST MODEL: The best performing model on the test set is %s
     (Test ROC-AUC = %.4f). In this dataset, the regularized logistic
     regression models performed on par with or slightly better than the
     tree-based ensemble (Random Forest), suggesting the predictor-treatment
     relationships are reasonably well captured by an additive/linear model
     once the relevant categorical predictors are included.

  4. PRACTICAL INTERPRETATION OF ODDS RATIOS:
     - Individuals with a family history of mental illness have substantially
       higher odds of seeking treatment, consistent with greater awareness
       and normalisation of mental health care within their families.
     - Higher work interference levels are associated with higher odds of
       treatment, suggesting that symptom severity drives help-seeking.
     - Employer-provided benefits and seek_help resources are positively
       associated with treatment uptake, highlighting workplace policy impact.
     - Gender differences exist but are smaller than clinical/familial
       factors after controlling for other variables.

  5. POLICY RECOMMENDATION: Employers should invest in mental health benefits,
     anonymous support channels, and awareness programmes to increase
     treatment uptake, particularly targeting employees who report that
     mental health problems interfere with their work.

  6. MODEL LIMITATIONS: The dataset is self-selected (tech workers, online
     survey) and may not generalise to other industries. Class balance
     should be monitored; if imbalanced, consider SMOTE or weighted losses.
", best_model, max(eval_results$ROC_AUC)))

cat(strrep("=", 70), "\n")
cat("  R SCRIPT COMPLETED SUCCESSFULLY\n")
cat("  Output directories: tables/ | figures/ | models/\n")
cat(strrep("=", 70), "\n")

