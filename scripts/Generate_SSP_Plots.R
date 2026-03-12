# =============================================================================
# Generate SSP Model Comparison Plots
# =============================================================================
# This script generates plots for the SSP model to demonstrate:
# - Model-relativity of validity (comparing DMC vs SSP)
# - Similar arguments as DMC but with different architectural assumptions
# =============================================================================

# Setup ------------------------------------------------------------------------
rm(list = ls())
graphics.off()

pacman::p_load(here, tidytable, data.table, ggplot2,
               patchwork, scales, viridis)

# Define clean theme for all plots
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
# LOAD SSP RECOVERY DATA
# =============================================================================

cat("Loading SSP recovery data from package...\n")
data(ssp_recovery_behavior, package = "ComputationalValidity")
data(ssp_recovery_ezDM, package = "ComputationalValidity")
data(ssp_recovery_parRecovery, package = "ComputationalValidity")
data(ssp_recovery_reliability, package = "ComputationalValidity")

df_behavior_rec_ssp <- data.table(ssp_recovery_behavior)
df_ezDM_rec_ssp <- data.table(ssp_recovery_ezDM)
df_recovery_ssp <- data.table(ssp_recovery_parRecovery)
df_reliability_rec_ssp <- data.table(ssp_recovery_reliability)

# Compute average effects
avg_effects_behavior_rec_ssp <- df_behavior_rec_ssp %>%
  summarize(mean = mean(value),
            sd = sd(value),
            .by = c(nRep, SampleSize, nTrials, measure, indicator))

avg_effects_ezDM_rec_ssp <- df_ezDM_rec_ssp %>%
  summarize(mean = mean(value),
            sd = sd(value),
            .by = c(nRep, SampleSize, nTrials, measure, indicator))

avg_effects_all_rec_ssp <- rbind(avg_effects_behavior_rec_ssp, avg_effects_ezDM_rec_ssp)

df_recovery_ssp <- df_recovery_ssp %>%
  left_join(avg_effects_all_rec_ssp)

# =============================================================================
# LOAD SSP CORRELATION DATA
# =============================================================================

cat("Loading SSP correlation data from package...\n")
data(ssp_correlation_recCorrs, package = "ComputationalValidity")
df_correlations_ssp <- data.table(ssp_correlation_recCorrs)

# =============================================================================
# FIGURE S1: SSP Equifinality & Model-Guided Indicator Selection
# =============================================================================
cat("Generating SSP Figure S1: Equifinality & Model-Guided Indicator Selection...\n")

# PRIMARY FINDING: Demonstrates equifinality through selective recovery patterns
# Shows ALL 4 SSP parameters × 4 indicators to reveal:
# 1. Each indicator only recovers SOME parameters (incomplete coverage)
# 2. Different indicators needed for different constructs (selective sensitivity)
# 3. Practical guidance: which indicators to use for which parameters
# Parallel to DMC Figure 1

# Calculate recovery quality for ALL SSP parameters and ALL indicators
# Include 95% confidence intervals
recovery_comprehensive_ssp <- df_recovery_ssp %>%
  filter(
    genPar %in% c("b", "p", "sd_0", "non_dec"),  # ALL 4 SSP parameters
    measure %in% c("RT", "PC"),
    indicator %in% c("difference", "mean"),
    SampleSize == "N = 100",
    nTrials == "100"
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
        genPar == "p" ~ "Perceptual Input (p)",
        genPar == "sd_0" ~ "Initial Noise (σ₀)",
        genPar == "b" ~ "Boundary (b)",
        genPar == "non_dec" ~ "Non-Decision Time (Ter)"
      ),
      levels = c("Perceptual Input (p)", "Initial Noise (σ₀)", 
                 "Boundary (b)", "Non-Decision Time (Ter)")
    ),
    indicator_measure = case_when(
      indicator == "difference" & measure == "RT" ~ "RT Difference",
      indicator == "difference" & measure == "PC" ~ "PC Difference",
      indicator == "mean" & measure == "RT" ~ "RT Mean",
      indicator == "mean" & measure == "PC" ~ "PC Mean"
    ),
    indicator_measure = factor(indicator_measure,
                               levels = c("RT Difference", "PC Difference",
                                         "RT Mean", "PC Mean")),
    validity_strength = case_when(
      abs(median_rec) > 0.5 ~ "Strong",
      abs(median_rec) > 0.3 ~ "Moderate",
      abs(median_rec) > 0.1 ~ "Weak",
      TRUE ~ "None"
    ),
    validity_strength = factor(validity_strength,
                               levels = c("None", "Weak", "Moderate", "Strong"))
  )

# Create comprehensive heatmap showing selective recovery (equifinality)
figS1 <- ggplot(recovery_comprehensive_ssp,
               aes(x = param_label, y = indicator_measure, fill = abs(median_rec))) +
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
    x = "Target Construct (SSP Parameter)",
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

ggsave(here("figures", "manuscript", "SSP_Equifinality.png"),
       figS1, width = 11, height = 7, dpi = 600)
ggsave(here("figures", "manuscript", "SSP_Equifinality.pdf"),
       figS1, width = 11, height = 7)
ggsave(here("figures", "manuscript", "SSP_Equifinality.tiff"),
       figS1, width = 11, height = 7, dpi = 600, compression = "lzw")

cat("  Figure S1 saved successfully!\n")

# =============================================================================
# FIGURE S2: SSP Reliability Paradox
# =============================================================================
cat("Generating SSP Figure S2: Reliability Paradox...\n")

# SECOND FINDING: Reliability-validity dissociation
# High reliability does not guarantee parameter recovery
# Parallel to DMC Figure 2

reliability_recovery_data_ssp <- df_recovery_ssp %>%
  filter(
    genPar %in% c("b", "p", "sd_0"),
    measure == "RT",
    indicator %in% c("difference", "mean"),
    SampleSize == "N = 100"
  ) %>%
  mutate(
    # Convert indicator names to match reliability dataset
    indicator_short = ifelse(indicator == "difference", "diff", indicator)
  ) %>%
  left_join(
    df_reliability_rec_ssp %>%
      filter(measure == "RT", indicator %in% c("diff", "mean")) %>%
      mutate(
        SampleSize = factor(SampleSize, levels = c(25, 50, 100)),
        nTrials = factor(nTrials, levels = c(25, 50, 100)),
        SampleSize = paste0("N = ", SampleSize)
      ) %>%
      select(nRep, SampleSize, nTrials, indicator, reliability) %>%
      rename(indicator_short = indicator),
    by = c("nRep", "SampleSize", "nTrials", "indicator_short")
  ) %>%
  filter(!is.na(reliability)) %>%
  mutate(
    indicator_label = case_when(
      indicator == "difference" ~ "Difference Score\n(Incomp - Comp)",
      indicator == "mean" ~ "Mean Score\n(Average)",
      TRUE ~ indicator
    )
  )

figS2 <- ggplot(reliability_recovery_data_ssp,
               aes(x = reliability, y = rec, color = as.factor(nTrials))) +
  facet_grid(genPar ~ indicator_label,
             labeller = labeller(
               genPar = c(
                 "b" ~ "Boundary (b)",
                 "p" ~ "Perceptual Input (p)",
                 "sd_0" ~ "Initial Noise (sd_0)"
               )
             ),
             scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30") +
  stat_function(fun = sqrt, linetype = "dashed", color = "gray50", linewidth = 0.6) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_viridis_d(
    name = "Trials per Condition",
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

ggsave(here("figures", "manuscript", "SSP_Reliability_Paradox.png"),
       figS2, width = 12, height = 8, dpi = 600)
ggsave(here("figures", "manuscript", "SSP_Reliability_Paradox.pdf"),
       figS2, width = 12, height = 8)
ggsave(here("figures", "manuscript", "SSP_Reliability_Paradox.tiff"),
       figS2, width = 12, height = 8, dpi = 600, compression = "lzw")

cat("  Figure S2 saved successfully!\n")

# =============================================================================
# FIGURE S3: SSP Correlation Transfer
# =============================================================================
cat("Generating SSP Figure S3: Correlation Transfer...\n")

correlation_transfer_ssp <- df_correlations_ssp %>%
  filter(
    SampleSize == "N = 200",
    nTrials == "100",
    indicator %in% c("difference", "mean"),
    measure %in% c("RT", "PC"),
    correlated_par %in% c("p", "sd_0")
  ) %>%
  mutate(
    gen_param_cor = case_when(
      correlated_par == "p" ~ emp_corr.p,
      correlated_par == "sd_0" ~ emp_corr.sd_0,
      TRUE ~ NA_real_
    ),
    param_label = case_when(
      correlated_par == "p" ~ "Perceptual Input (p)",
      correlated_par == "sd_0" ~ "Initial Noise (sd_0)",
      TRUE ~ correlated_par
    ),
    measure_label = ifelse(measure == "RT", "Response Time", "Proportion Correct"),
    score_type = factor(
      ifelse(indicator == "difference", "Difference Scores", "Mean Scores"),
      levels = c("Difference Scores", "Mean Scores")
    ),
    indicator_label = case_when(
      indicator == "difference" & measure == "RT" ~ "RT Difference",
      indicator == "difference" & measure == "PC" ~ "Accuracy Difference",
      indicator == "mean" & measure == "RT" ~ "RT Mean",
      indicator == "mean" & measure == "PC" ~ "Accuracy Mean",
      TRUE ~ paste(measure, indicator)
    )
  ) %>%
  filter(!is.na(gen_param_cor))

figS3 <- ggplot(correlation_transfer_ssp,
               aes(x = gen_param_cor, y = correlation, color = measure_label)) +
  facet_grid(score_type ~ param_label) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "gray30", linewidth = 0.8) +
  geom_point(alpha = 0.1, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
  scale_color_manual(
    name = "Behavioral Indicator",
    values = c("Response Time" = "#D55E00", "Proportion Correct" = "#0072B2")
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

ggsave(here("figures", "manuscript", "SSP_Correlation_Transfer.png"),
       figS3, width = 8, height = 6, dpi = 600)
ggsave(here("figures", "manuscript", "SSP_Correlation_Transfer.pdf"),
       figS3, width = 8, height = 6)
ggsave(here("figures", "manuscript", "SSP_Correlation_Transfer.tiff"),
       figS3, width = 8, height = 6, dpi = 600, compression = "lzw")

cat("  Figure S3 saved successfully!\n")

# =============================================================================
# FIGURE S4: SSP Intermediate Modeling
# =============================================================================
# FIGURE S4: SSP Intermediate Modeling
# =============================================================================
cat("Generating SSP Figure S4: Intermediate Modeling...\n")

recovery_comparison_ssp <- df_recovery_ssp %>%
  filter(
    genPar %in% c("b", "non_dec", "p", "sd_0"),
    measure %in% c("RT", "PC", "drift", "boundary", "non_dec"),
    indicator %in% c("mean", "difference"),
    SampleSize == "N = 100",
    nTrials == "100"
  ) %>%
  mutate(
    param_label = case_when(
      genPar == "b" ~ "Boundary (b)",
      genPar == "non_dec" ~ "Non-Decision Time (t0)",
      genPar == "p" ~ "Perceptual Input (p)",
      genPar == "sd_0" ~ "Initial Noise (sd_0)",
      TRUE ~ genPar
    ),
    measure_type = case_when(
      measure %in% c("RT", "PC") ~ "Behavioral",
      measure %in% c("drift", "boundary", "non_dec") ~ "ezDM",
      TRUE ~ "Other"
    ),
    measure_label = case_when(
      measure == "RT" ~ "RT",
      measure == "PC" ~ "PC",
      measure == "drift" ~ "Drift",
      measure == "boundary" ~ "Boundary",
      measure == "non_dec" ~ "Non-Decision",
      TRUE ~ measure
    )
  ) %>%
  filter(measure_type %in% c("Behavioral", "ezDM"))

recovery_summary_comp_ssp <- recovery_comparison_ssp %>%
  group_by(param_label, measure_label, measure_type, indicator) %>%
  summarize(
    median_rec = median(rec, na.rm = TRUE),
    mean_rec = mean(rec, na.rm = TRUE),
    q25 = quantile(rec, 0.25, na.rm = TRUE),
    q75 = quantile(rec, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

figS4 <- ggplot(recovery_summary_comp_ssp,
               aes(x = measure_label, y = median_rec, fill = measure_type)) +
  facet_grid(indicator ~ param_label, scales = "free_x") +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray60", linewidth = 0.8) +
  geom_hline(yintercept = c(-0.7, -0.5, -0.3, 0.3, 0.5, 0.7), linetype = "dashed",
             color = "gray40", alpha = 0.4) +
  geom_col(position = position_dodge(width = 0.9), color = "black",
           linewidth = 0.3, width = 0.8) +
  geom_errorbar(
    aes(ymin = q25, ymax = q75),
    position = position_dodge(width = 0.9),
    width = 0.3,
    linewidth = 0.5
  ) +
  scale_fill_manual(
    name = "Indicator Type",
    values = c("Behavioral" = "#E69F00", "ezDM" = "#56B4E9")
  ) +
  labs(
    x = "",
    y = "Parameter Recovery (Correlation)"
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_manuscript +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "gray90", color = "black")
  )

ggsave(here("figures", "manuscript", "SSP_Intermediate_Modeling.png"),
       figS4, width = 14, height = 8, dpi = 600)
ggsave(here("figures", "manuscript", "SSP_Intermediate_Modeling.pdf"),
       figS4, width = 14, height = 8)
ggsave(here("figures", "manuscript", "SSP_Intermediate_Modeling.tiff"),
       figS4, width = 14, height = 8, dpi = 600, compression = "lzw")

cat("  Figure S4 saved successfully!\n")

# =============================================================================
# Summary Statistics for SSP
# =============================================================================
cat("\n=============================================================================\n")
cat("SSP SUMMARY STATISTICS\n")
cat("=============================================================================\n\n")

# Differential validity statistics
cat("SSP DIFFERENTIAL INDICATOR VALIDITY (N=100, 100 trials/condition):\n")
differential_stats_ssp <- df_recovery_ssp %>%
  filter(
    genPar %in% c("b", "p", "sd_0"),
    measure %in% c("RT", "PC"),
    indicator %in% c("difference", "mean"),
    SampleSize == "N = 100",
    nTrials == "100"
  ) %>%
  group_by(genPar, measure, indicator) %>%
  summarize(
    median_rec = median(rec, na.rm = TRUE),
    mean_rec = mean(rec, na.rm = TRUE),
    iqr = IQR(rec, na.rm = TRUE),
    .groups = "drop"
  )
print(differential_stats_ssp)

# Reliability paradox statistics
cat("\n\nSSP RELIABILITY PARADOX (N=100, 100 trials/condition):\n")
reliability_stats_ssp <- reliability_recovery_data_ssp %>%
  filter(nTrials == "100") %>%
  group_by(genPar, indicator_label) %>%
  summarize(
    median_reliability = median(reliability, na.rm = TRUE),
    median_recovery = median(rec, na.rm = TRUE),
    cor_rel_rec = cor(reliability, rec, use = "pairwise.complete.obs"),
    .groups = "drop"
  )
print(reliability_stats_ssp)

# Correlation transfer statistics
cat("\n\nSSP CORRELATION TRANSFER (N=200, 100 trials/condition):\n")
transfer_stats_ssp <- correlation_transfer_ssp %>%
  group_by(param_label, measure_label) %>%
  summarize(
    median_param_cor = median(gen_param_cor, na.rm = TRUE),
    median_indicator_cor = median(correlation, na.rm = TRUE),
    transfer_efficiency = median(correlation / gen_param_cor, na.rm = TRUE),
    .groups = "drop"
  )
print(transfer_stats_ssp)

# Intermediate modeling statistics
cat("\n\nSSP INTERMEDIATE MODELING (N=100, 100 trials/condition):\n")
cat("Comparison of behavioral vs ezDM indicators:\n")
modeling_comparison_ssp <- recovery_summary_comp_ssp %>%
  group_by(param_label, measure_type) %>%
  summarize(
    mean_recovery = mean(median_rec, na.rm = TRUE),
    max_recovery = max(median_rec, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = measure_type,
    values_from = c(mean_recovery, max_recovery)
  )
print(modeling_comparison_ssp)

cat("\n=============================================================================\n")
cat("SSP figures saved to: figures/manuscript/\n")
cat("=============================================================================\n")
