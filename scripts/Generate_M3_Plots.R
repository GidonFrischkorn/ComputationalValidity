# =============================================================================
# Generate M3 Model Plots for Manuscript
# =============================================================================
# This script generates plots for the M3 complex span model to demonstrate:
# - Generality of validity arguments beyond conflict tasks to working memory
# - Same four phenomena: equifinality, reliability paradox, correlation transfer
# - No intermediate modeling figure (no EZ-DM equivalent for M3)
# =============================================================================

# Setup ------------------------------------------------------------------------
rm(list = ls())
graphics.off()

pacman::p_load(here, tidytable, data.table, ggplot2,
               patchwork, scales, viridis)

# Define clean theme for all plots (matches DMC/SSP scripts)
theme_manuscript <- theme_bw(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.key = element_rect(fill = 'white'),
    strip.background = element_rect(fill = 'white'),
    text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    strip.text = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )

# Create output directory
dir.create(here("figures", "manuscript"), showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# LOAD M3 RECOVERY DATA
# =============================================================================

cat("Loading M3 recovery data from package...\n")
data(m3_recovery_behavior, package = "ComputationalValidity")
data(m3_recovery_parRecovery, package = "ComputationalValidity")
data(m3_recovery_reliability, package = "ComputationalValidity")

df_behavior_rec <- data.table(m3_recovery_behavior)
df_recovery_all <- data.table(m3_recovery_parRecovery)
df_reliability_rec <- data.table(m3_recovery_reliability)

# Compute average effects
avg_effects_behavior_rec <- df_behavior_rec %>%
  summarize(mean = mean(value),
            sd = sd(value),
            .by = c(nRep, SampleSize, nTrials, measure, indicator))

df_reliability_rec <- df_reliability_rec %>%
  left_join(avg_effects_behavior_rec)

df_recovery_all <- df_recovery_all %>%
  left_join(avg_effects_behavior_rec)

# =============================================================================
# LOAD M3 CORRELATION DATA
# =============================================================================

cat("Loading M3 correlation data from package...\n")
data(m3_correlation_recCorrs, package = "ComputationalValidity")
df_correlations_all <- data.table(m3_correlation_recCorrs)

# =============================================================================
# FIGURE M3-1: Equifinality & Model-Guided Indicator Selection
# =============================================================================
cat("Generating M3 Figure 1: Equifinality & Model-Guided Indicator Selection...\n")

# Shows ALL 3 M3 parameters × 10 indicators to reveal:
# 1. Each indicator only recovers SOME parameters (incomplete coverage)
# 2. Different indicators needed for different constructs (selective sensitivity)
# 3. Practical guidance: which indicators to use for which parameters

recovery_comprehensive <- df_recovery_all %>%
  filter(
    genPar %in% c("c", "a", "f"),
    SampleSize == "N = 100",
    nTrials == "100",
    measure == "composite"
  ) %>%
  group_by(genPar, measure, indicator) %>%
  summarize(
    median_rec = median(rec, na.rm = TRUE),
    mean_rec = mean(rec, na.rm = TRUE),
    ci_lower = quantile(rec, 0.025, na.rm = TRUE),
    ci_upper = quantile(rec, 0.975, na.rm = TRUE),
    q25 = quantile(rec, 0.25, na.rm = TRUE),
    q75 = quantile(rec, 0.75, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    param_label = factor(
      case_when(
        genPar == "c" ~ "Context Binding (c)",
        genPar == "a" ~ "Item Activation (a)",
        genPar == "f" ~ "Filtering (f)"
      ),
      levels = c("Context Binding (c)", "Item Activation (a)", "Filtering (f)")
    ),
    indicator_label = factor(
      case_when(
        indicator == "accuracy" ~ "Accuracy",
        indicator == "intrusion_mem" ~ "Memoranda\nIntrusions",
        indicator == "intrusion_dist" ~ "Distractor\nIntrusions",
        indicator == "intrusion_total" ~ "Total\nIntrusions",
        indicator == "npl_rate" ~ "Guessing\nRate"
      ),
      levels = c("Accuracy", "Memoranda\nIntrusions", "Distractor\nIntrusions",
                 "Total\nIntrusions", "Guessing\nRate")
    ),
    validity_strength = case_when(
      abs(median_rec) > 0.5 ~ "Strong",
      abs(median_rec) > 0.3 ~ "Moderate",
      abs(median_rec) > 0.1 ~ "Weak",
      TRUE ~ "None"
    ),
    validity_strength = factor(validity_strength,
                               levels = c("None", "Weak", "Moderate", "Strong"))
  )

fig1 <- ggplot(recovery_comprehensive,
               aes(x = param_label, y = indicator_label, fill = abs(median_rec))) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f\n[%.2f, %.2f]\n(%s)",
                                median_rec, ci_lower, ci_upper, validity_strength)),
            size = 3, fontface = "bold") +
  scale_fill_gradientn(
    colors = c("#D73027", "#FEE08B", "#1A9850"),
    values = scales::rescale(c(0, 0.3, 1)),
    limits = c(0, 1),
    name = "Recovery\nStrength\n(|r|)",
    breaks = c(0, 0.3, 0.5, 0.7, 1),
    labels = c("0", "0.3", "0.5", "0.7", "1.0")
  ) +
  labs(
    x = "Target Construct (M3 Parameter)",
    y = "Candidate Behavioral Indicator"
  ) +
  theme_manuscript +
  theme(
    axis.text.x = element_text(size = 11, face = "bold", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  coord_equal()

ggsave(here("figures", "manuscript", "M3_Equifinality.png"),
       fig1, width = 12, height = 7, dpi = 600)
ggsave(here("figures", "manuscript", "M3_Equifinality.pdf"),
       fig1, width = 12, height = 7)
ggsave(here("figures", "manuscript", "M3_Equifinality.tiff"),
       fig1, width = 12, height = 7, dpi = 600, compression = "lzw")

cat("  Figure 1 saved successfully!\n")

# =============================================================================
# FIGURE M3-2: The Reliability Paradox
# =============================================================================
cat("Generating M3 Figure 2: Reliability Paradox...\n")

# Shows that high reliability does not guarantee parameter recovery
# For M3: compare high-reliability indicators (accuracy) with
# lower-reliability ones (distractor proportions) against recovery

reliability_recovery_data <- df_recovery_all %>%
  filter(
    genPar %in% c("c", "a", "f"),
    measure == "composite",
    indicator %in% c("accuracy", "npl_rate", "intrusion_dist"),
    SampleSize == "N = 100"
  ) %>%
  left_join(
    df_reliability_rec %>%
      filter(measure == "composite", indicator %in% c("accuracy", "npl_rate", "intrusion_dist")) %>%
      select(nRep, SampleSize, nTrials, indicator, reliability),
    by = c("nRep", "SampleSize", "nTrials", "indicator")
  ) %>%
  mutate(
    indicator_label = case_when(
      indicator == "accuracy" ~ "Accuracy",
      indicator == "npl_rate" ~ "Guessing Rate",
      indicator == "intrusion_dist" ~ "Distractor Intrusions",
      TRUE ~ indicator
    )
  )

fig2 <- ggplot(reliability_recovery_data,
               aes(x = reliability, y = rec, color = as.factor(nTrials))) +
  facet_grid(genPar ~ indicator_label,
             labeller = labeller(
               genPar = c(
                 "c" = "Context Binding (c)",
                 "a" = "Item Activation (a)",
                 "f" = "Filtering (f)"
               )
             ),
             scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_viridis_d(
    name = "Number of Trials",
    option = "D",
    end = 0.8
  ) +
  labs(
    x = "Indicator Reliability",
    y = "Parameter Recovery (Correlation)"
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_manuscript +
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "gray90", color = "black")
  )

ggsave(here("figures", "manuscript", "M3_Reliability_Paradox.png"),
       fig2, width = 10, height = 10, dpi = 600)
ggsave(here("figures", "manuscript", "M3_Reliability_Paradox.pdf"),
       fig2, width = 10, height = 10)
ggsave(here("figures", "manuscript", "M3_Reliability_Paradox.tiff"),
       fig2, width = 10, height = 10, dpi = 600, compression = "lzw")

cat("  Figure 2 saved successfully!\n")

# =============================================================================
# FIGURE M3-3: Correlation Transfer - The Limits of Validity
# =============================================================================
cat("Generating M3 Figure 3: Correlation Transfer...\n")

# For M3: use proportion indicators (corr, other, distc, disto, npl)
# as the "behavioral indicators" analogous to RT/PC difference scores

correlation_transfer <- df_correlations_all %>%
  filter(
    SampleSize == "N = 200",
    nTrials == "100",
    measure == "composite",
    indicator %in% c("accuracy", "intrusion_mem", "intrusion_dist", "intrusion_total", "npl_rate"),
    correlated_par %in% c("c", "a", "f", "c-a", "c-f", "a-f")
  ) %>%
  mutate(
    gen_param_cor = case_when(
      correlated_par == "c" ~ emp_corr.c,
      correlated_par == "a" ~ emp_corr.a,
      correlated_par == "f" ~ emp_corr.f,
      correlated_par == "c-a" ~ (emp_corr.c + emp_corr.a) / 2,
      correlated_par == "c-f" ~ (emp_corr.c + emp_corr.f) / 2,
      correlated_par == "a-f" ~ (emp_corr.a + emp_corr.f) / 2,
      TRUE ~ NA_real_
    ),
    param_label = case_when(
      correlated_par == "c" ~ "Context Binding (c)",
      correlated_par == "a" ~ "Item Activation (a)",
      correlated_par == "f" ~ "Filtering (f)",
      correlated_par == "c-a" ~ "c + a",
      correlated_par == "c-f" ~ "c + f",
      correlated_par == "a-f" ~ "a + f",
      TRUE ~ correlated_par
    ),
    param_label = factor(param_label, levels = c(
      "Context Binding (c)", "Item Activation (a)", "Filtering (f)",
      "c + a", "c + f", "a + f"
    )),
    indicator_label = case_when(
      indicator == "accuracy" ~ "Accuracy",
      indicator == "intrusion_mem" ~ "Memoranda Intrusions",
      indicator == "intrusion_dist" ~ "Distractor Intrusions",
      indicator == "intrusion_total" ~ "Total Intrusions",
      indicator == "npl_rate" ~ "Guessing Rate"
    )
  ) %>%
  filter(!is.na(gen_param_cor))

fig3 <- ggplot(correlation_transfer,
               aes(x = gen_param_cor, y = correlation_corrected,
                   color = indicator_label)) +
  facet_wrap(~ param_label, nrow = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "gray30", linewidth = 0.8) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
  scale_color_viridis_d(
    name = "Behavioral Indicator",
    option = "D",
    end = 0.9
  ) +
  labs(
    x = "Generating Parameter Correlation",
    y = "Observed Indicator Correlation"
  ) +
  coord_cartesian(xlim = c(0, 1), ylim = c(-0.5, 1)) +
  theme_manuscript +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10)
  )

ggsave(here("figures", "manuscript", "M3_Correlation_Transfer.png"),
       fig3, width = 13, height = 8, dpi = 600)
ggsave(here("figures", "manuscript", "M3_Correlation_Transfer.pdf"),
       fig3, width = 13, height = 8)
ggsave(here("figures", "manuscript", "M3_Correlation_Transfer.tiff"),
       fig3, width = 13, height = 8, dpi = 600, compression = "lzw")

cat("  Figure 3 saved successfully!\n")

# =============================================================================
# Summary Statistics for Text
# =============================================================================
cat("\n=============================================================================\n")
cat("M3 SUMMARY STATISTICS FOR MANUSCRIPT TEXT\n")
cat("=============================================================================\n\n")

# Differential validity statistics
cat("M3 DIFFERENTIAL INDICATOR VALIDITY (N=100, 100 trials):\n")
differential_stats <- df_recovery_all %>%
  filter(
    genPar %in% c("c", "a", "f"),
    SampleSize == "N = 100",
    nTrials == "100"
  ) %>%
  group_by(genPar, measure, indicator) %>%
  summarize(
    median_rec = median(rec, na.rm = TRUE),
    mean_rec = mean(rec, na.rm = TRUE),
    iqr = IQR(rec, na.rm = TRUE),
    prop_positive = mean(rec > 0),
    .groups = "drop"
  ) %>%
  mutate(
    param_label = case_when(
      genPar == "c" ~ "Context Binding",
      genPar == "a" ~ "Item Activation",
      genPar == "f" ~ "Filtering"
    )
  )
print(differential_stats)

# Reliability-recovery relationship
cat("\n\nM3 RELIABILITY-RECOVERY RELATIONSHIP:\n")
if (exists("reliability_recovery_data")) {
  rel_rec_stats <- reliability_recovery_data %>%
    filter(nTrials == "100") %>%
    group_by(genPar, indicator_label) %>%
    summarize(
      median_reliability = median(reliability, na.rm = TRUE),
      median_recovery = median(rec, na.rm = TRUE),
      cor_rel_rec = cor(reliability, rec, use = "pairwise.complete.obs"),
      .groups = "drop"
    )
  print(rel_rec_stats)
}

# Correlation transfer statistics
cat("\n\nM3 CORRELATION TRANSFER (N=200, 100 trials):\n")
if (exists("correlation_transfer")) {
  transfer_stats <- correlation_transfer %>%
    group_by(param_label, indicator_label) %>%
    summarize(
      median_param_cor = median(gen_param_cor, na.rm = TRUE),
      median_indicator_cor = median(correlation_corrected, na.rm = TRUE),
      transfer_ratio = median(correlation_corrected / gen_param_cor, na.rm = TRUE),
      .groups = "drop"
    )
  print(transfer_stats)
}

cat("\n=============================================================================\n")
cat("M3 figures saved to: figures/manuscript/\n")
cat("Figure Summary:\n")
cat("  Fig M3-1: Equifinality (parameter recovery heatmap)\n")
cat("  Fig M3-2: Reliability Paradox (reliability vs recovery)\n")
cat("  Fig M3-3: Correlation Transfer (parameter vs indicator correlations)\n")
cat("  Note: No Intermediate Modeling figure (no EZ-DM equivalent for M3)\n")
cat("=============================================================================\n")
