# =============================================================================
# Generate Manuscript-Ready Plots for Validity Paper
# =============================================================================
# This script generates accessible, informative plots that communicate the
# key arguments about validity in computational psychometrics.
#
# Key arguments:
# 1. Parameters are the locus of individual differences, not indicators
# 2. Parameter confounding exists (equifinality)
# 3. Reliability doesn't guarantee parameter recovery
# 4. Common variance requires explicit theory
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
# PART 1: PARAMETER RECOVERY SIMULATION
# =============================================================================

# Load package datasets
cat("Loading DMC recovery data from package...\n")
data(dmc_recovery_behavior, package = "ComputationalValidity")
data(dmc_recovery_ezDM, package = "ComputationalValidity")
data(dmc_recovery_parRecovery, package = "ComputationalValidity")
data(dmc_recovery_reliability, package = "ComputationalValidity")

# Rename for consistency with script
df_behavior_rec <- data.table(dmc_recovery_behavior)
df_ezDM_rec <- data.table(dmc_recovery_ezDM)
df_recovery_all <- data.table(dmc_recovery_parRecovery)
df_reliability_rec <- data.table(dmc_recovery_reliability)

# Compute average effects
avg_effects_behavior_rec <- df_behavior_rec %>%
  summarize(mean = mean(value),
            sd = sd(value),
            .by = c(nRep, SampleSize, nTrials, measure, indicator))

avg_effects_ezDM_rec <- df_ezDM_rec %>%
  summarize(mean = mean(value),
            sd = sd(value),
            .by = c(nRep, SampleSize, nTrials, measure, indicator))

avg_effects_all_rec <- rbind(avg_effects_behavior_rec, avg_effects_ezDM_rec)

df_reliability_rec <- df_reliability_rec %>%
  left_join(avg_effects_all_rec)

df_recovery_all <- df_recovery_all %>%
  left_join(avg_effects_all_rec)

# =============================================================================
# FIGURE 1: Equifinality & Model-Guided Indicator Selection
# =============================================================================
cat("Generating Figure 1: Equifinality & Model-Guided Indicator Selection...\n")

# PRIMARY FINDING: Demonstrates equifinality through selective recovery patterns
# Shows ALL 5 parameters × 4 indicators to reveal:
# 1. Each indicator only recovers SOME parameters (incomplete coverage)
# 2. Different indicators needed for different constructs (selective sensitivity)
# 3. Practical guidance: which indicators to use for which parameters

# Calculate recovery quality for ALL parameters and ALL indicators
# Include 95% confidence intervals using bootstrap percentile method
recovery_comprehensive <- df_recovery_all %>%
  filter(
    genPar %in% c("A", "tau", "muc", "b", "non_dec"),  # ALL 5 parameters
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
        genPar == "muc" ~ "Controlled Drift (μc)",
        genPar == "tau" ~ "Temporal Dynamics (τ)",
        genPar == "A" ~ "Amplitude (A)",
        genPar == "b" ~ "Boundary (b)",
        genPar == "non_dec" ~ "Non-Decision Time (Ter)"
      ),
      levels = c("Controlled Drift (μc)", "Temporal Dynamics (τ)", 
                 "Amplitude (A)", "Boundary (b)", "Non-Decision Time (Ter)")
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

# Ensure directories exist
dir.create(here("figures", "manuscript"), recursive = TRUE, showWarnings = FALSE)

# Create comprehensive heatmap showing selective recovery (equifinality)
fig1 <- ggplot(recovery_comprehensive,
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
    x = "Target Construct (DMC Parameter)",
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

ggsave(here("figures", "manuscript", "DMC_Equifinality.png"),
       fig1, width = 12, height = 7, dpi = 600)
ggsave(here("figures", "manuscript", "DMC_Equifinality.pdf"),
       fig1, width = 12, height = 7)
ggsave(here("figures", "manuscript", "DMC_Equifinality.tiff"),
       fig1, width = 12, height = 7, dpi = 600, compression = "lzw")

cat("  Figure 1 saved successfully!\n")

# =============================================================================
# FIGURE 2: The Reliability Paradox
# =============================================================================
cat("Generating Figure 2: The Reliability Paradox...\n")

# THIRD FINDING: High INDICATOR reliability doesn't guarantee parameter recovery
# Shows that the reliability-validity link is not necessary

# Get reliability of RT indicators (both difference and mean scores)
reliability_recovery_data <- df_recovery_all %>%
  filter(
    genPar %in% c("A", "tau", "muc"),  # Focus on key parameters
    measure == "RT",  # Parameter recovery from RT
    indicator %in% c("difference", "mean"),  # Compare difference and mean scores
    SampleSize == "N = 100"
  ) %>%
  left_join(
    df_reliability_rec %>%
      filter(measure == "RT", indicator %in% c("difference", "mean")) %>%
      select(nRep, SampleSize, nTrials, indicator, reliability),
    by = c("nRep", "SampleSize", "nTrials", "indicator")
  ) %>%
  mutate(
    indicator_label = case_when(
      indicator == "difference" ~ "Difference Score\n(Incomp - Comp)",
      indicator == "mean" ~ "Mean Score\n(Average)",
      TRUE ~ indicator
    )
  )

fig2 <- ggplot(reliability_recovery_data,
               aes(x = reliability, y = rec, color = as.factor(nTrials))) +
  facet_grid(genPar ~ indicator_label,
             labeller = labeller(
               genPar = c(
                 "A" = "Amplitude (A)",
                 "tau" = "Temporal Dynamics (τ)",
                 "muc" = "Controlled drift (μc)"
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

ggsave(here("figures", "manuscript", "DMC_Reliability_Paradox.png"),
       fig2, width = 8, height = 12, dpi = 600)
ggsave(here("figures", "manuscript", "DMC_Reliability_Paradox.pdf"),
       fig2, width = 8, height = 12)
ggsave(here("figures", "manuscript", "DMC_Reliability_Paradox.tiff"),
       fig2, width = 8, height = 12, dpi = 600, compression = "lzw")

cat("  Figure 2 saved successfully!\n")

# =============================================================================
# PART 2: CORRELATION SIMULATION - Nuanced Perspective
# =============================================================================

# Load package datasets
cat("Loading DMC correlation data from package...\n")
data(dmc_correlation_recCorrs, package = "ComputationalValidity")

# Rename for consistency
df_correlations_all <- data.table(dmc_correlation_recCorrs)

# =============================================================================
# FIGURE 3: Correlation Transfer - The Limits of Validity
# =============================================================================
cat("Generating Figure 3: Correlation Transfer...\n")

# This figure provides nuance: valid indicators don't always track parameter correlations
# This is important for understanding the LIMITS of indicator validity

# Prepare correlation transfer data
correlation_transfer <- df_correlations_all %>%
  filter(
    SampleSize == "N = 200",
    nTrials == "100",
    indicator %in% c("difference", "mean"),
    measure %in% c("RT", "PC"),
    correlated_par %in% c("muc","A","tau","muc-A","muc-tau","A-tau")
  ) %>%
  mutate(
    # Get the relevant conflict parameter correlation
    # For single parameters: use that parameter's correlation
    # For pairs: use average of the two conflict parameters' correlations
    gen_param_cor = case_when(
      correlated_par == "muc" ~ emp_corr.muc,
      correlated_par == "A" ~ emp_corr.A,
      correlated_par == "tau" ~ emp_corr.tau,
      correlated_par == "muc-A" ~ emp_corr.A,
      correlated_par == "muc-tau" ~ emp_corr.tau,
      correlated_par == "A-tau" ~ (emp_corr.A + emp_corr.tau) / 2,
      TRUE ~ NA_real_
    ),
    param_label = case_when(
      correlated_par == "muc" ~ "Controlled Drift (μc)",
      correlated_par == "A" ~ "Amplitude (A)",
      correlated_par == "tau" ~ "Temporal Dynamics (τ)",
      correlated_par == "muc-A" ~ "μc + A",
      correlated_par == "muc-tau" ~ "μc + τ",
      correlated_par == "A-tau" ~ "A + τ",
      TRUE ~ correlated_par
    ),
    # Create row grouping for faceting
    param_type = case_when(
      correlated_par %in% c("A", "muc", "tau") ~ "Single Parameter",
      TRUE ~ "Parameter Pairs"
    ),
    # Order for display
    param_label = factor(param_label, levels = c(
      "Amplitude (A)", "Controlled Drift (μc)", "Temporal Dynamics (τ)",
      "μc + A", "μc + τ", "A + τ"
    )),
    measure_label = ifelse(measure == "RT", "Response Time", "Proportion Correct"),
    indicator_label = case_when(
      indicator == "difference" & measure == "RT" ~ "RT Difference",
      indicator == "difference" & measure == "PC" ~ "Accuracy Difference",
      indicator == "mean" & measure == "RT" ~ "RT Mean",
      indicator == "mean" & measure == "PC" ~ "Accuracy Mean",
      TRUE ~ paste(measure, indicator)
    )
  ) %>%
  filter(!is.na(gen_param_cor))

fig3 <- ggplot(correlation_transfer,
               aes(x = gen_param_cor, y = correlation_corrected, color = indicator_label)) +
  facet_wrap(~ param_label, nrow = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "gray30", linewidth = 0.8) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
  scale_color_manual(
    name = "Behavioral Indicator",
    values = c("RT Difference" = "#E69F00", "Accuracy Difference" = "#56B4E9",
               "RT Mean" = "#D55E00", "Accuracy Mean" = "#0072B2")
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

ggsave(here("figures", "manuscript", "DMC_Correlation_Transfer.png"),
       fig3, width = 13, height = 5, dpi = 600)
ggsave(here("figures", "manuscript", "DMC_Correlation_Transfer.pdf"),
       fig3, width = 13, height = 5)
ggsave(here("figures", "manuscript", "DMC_Correlation_Transfer.tiff"),
       fig3, width = 13, height = 5, dpi = 600, compression = "lzw")

cat("  Figure 3 saved successfully!\n")

# =============================================================================
# FIGURE 4: Intermediate Modeling - ezDM vs Behavioral Indicators
# =============================================================================
cat("Generating Figure 4: Intermediate Modeling...\n")

# This figure shows that intermediate models (ezDM) don't always outperform
# behavioral indicators. ezDM is better for some parameters but not all.

# Prepare data comparing behavioral (RT/PC difference) vs ezDM parameters
recovery_comparison <- df_recovery_all %>%
  filter(
    genPar %in% c("A", "tau", "b", "muc", "non_dec"),
    measure %in% c("RT", "PC", "drift", "boundary", "non_dec"),
    indicator %in% c("mean","difference"),
    SampleSize == "N = 100",
    nTrials == "100"
  ) %>%
  mutate(
    param_label = case_when(
      genPar == "A" ~ "Amplitude (A)",
      genPar == "tau" ~ "Temporal\nDynamics (τ)",
      genPar == "muc" ~ "Controlled\nDrift (μc)",
      genPar == "b" ~ "Boundary (b)",
      genPar == "non_dec" ~ "Non-Decision\nTime (t₀)",
      TRUE ~ genPar
    ),
    measure_type = case_when(
      measure %in% c("RT", "PC") ~ "Behavioral",
      measure %in% c("drift", "boundary", "non_dec") ~ "ezDM",
      TRUE ~ "Other"
    ),
    measure_label = case_when(
      measure == "RT" ~ "RT",
      measure == "PC" ~ "PC ",
      measure == "drift" ~ "Drift",
      measure == "boundary" ~ "Boundary",
      measure == "non_dec" ~ "Non-Decision",
      TRUE ~ measure
    )
  ) %>%
  filter(measure_type %in% c("Behavioral", "ezDM"))

# Calculate summary statistics
recovery_summary_comp <- recovery_comparison %>%
  group_by(param_label, measure_label, measure_type, indicator) %>%
  summarize(
    median_rec = median(rec, na.rm = TRUE),
    mean_rec = mean(rec, na.rm = TRUE),
    q25 = quantile(rec, 0.25, na.rm = TRUE),
    q75 = quantile(rec, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

# Create grouped comparison plot
fig4 <- ggplot(recovery_summary_comp,
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

ggsave(here("figures", "manuscript", "DMC_Intermediate_Modeling.png"),
       fig4, width = 14, height = 8, dpi = 600)
ggsave(here("figures", "manuscript", "DMC_Intermediate_Modeling.pdf"),
       fig4, width = 14, height = 8)
ggsave(here("figures", "manuscript", "DMC_Intermediate_Modeling.tiff"),
       fig4, width = 14, height = 8, dpi = 600, compression = "lzw")

cat("  Figure 4 saved successfully!\n")

# =============================================================================
# Summary Statistics for Text
# =============================================================================
cat("\n=============================================================================\n")
cat("SUMMARY STATISTICS FOR MANUSCRIPT TEXT\n")
cat("=============================================================================\n\n")

# Differential validity statistics
cat("DIFFERENTIAL INDICATOR VALIDITY (N=100, 100 trials/condition):\n")
differential_stats <- df_recovery_all %>%
  filter(
    genPar %in% c("A", "tau", "muc"),
    measure %in% c("RT", "PC"),
    indicator == "difference",
    SampleSize == "N = 100",
    nTrials == "100"
  ) %>%
  group_by(genPar, measure) %>%
  summarize(
    median_rec = median(rec, na.rm = TRUE),
    mean_rec = mean(rec, na.rm = TRUE),
    iqr = IQR(rec, na.rm = TRUE),
    prop_positive = mean(rec > 0),
    .groups = "drop"
  ) %>%
  mutate(
    param_label = case_when(
      genPar == "A" ~ "Amplitude",
      genPar == "tau" ~ "Temporal Dynamics",
      genPar == "muc" ~ "Controlled Drift"
    ),
    indicator = ifelse(measure == "RT", "Response Time", "Accuracy")
  ) %>%
  select(param_label, indicator, median_rec, mean_rec, iqr, prop_positive)

print(differential_stats)

# Correlation transfer statistics
cat("\n\nCORRELATION TRANSFER (N=200, 100 trials/condition):\n")
if (exists("correlation_transfer")) {
  transfer_stats <- correlation_transfer %>%
    group_by(param_label, measure_label) %>%
    summarize(
      median_param_cor = median(gen_param_cor, na.rm = TRUE),
      median_indicator_cor = median(correlation, na.rm = TRUE),
      transfer_ratio = median(correlation / gen_param_cor, na.rm = TRUE),
      .groups = "drop"
    )
  print(transfer_stats)
}

# Reliability-recovery relationship
cat("\n\nRELIABILITY-RECOVERY RELATIONSHIP:\n")
if (exists("reliability_recovery_data")) {
  rel_rec_stats <- reliability_recovery_data %>%
    filter(nTrials == "100") %>%
    group_by(genPar) %>%
    summarize(
      cor_rel_rec = cor(reliability, rec, use = "pairwise.complete.obs"),
      .groups = "drop"
    ) %>%
    mutate(
      param_label = case_when(
        genPar == "A" ~ "Amplitude (A)",
      genPar == "tau" ~ "Temporal Dynamics (τ)",
        genPar == "muc" ~ "Controlled Drift",
        genPar == "b" ~ "Boundary",
        genPar == "non_dec" ~ "Non-Decision Time"
      )
    )
  print(rel_rec_stats)
}

cat("\n=============================================================================\n")
cat("All manuscript figures saved to: figures/manuscript/\n")
cat("Figure Summary:\n")
cat("  Fig1: Parameter Confounding (equifinality problem)\n")
cat("  Fig2: Reliability Paradox (high reliability ≠ validity)\n")
cat("  Fig3: Differential Indicator Validity (RT vs PC recover different parameters)\n")
cat("  Fig4: Correlation Transfer (limits of indicator validity)\n")
cat("  Fig5: Model-Guided Selection (decision matrix for indicator choice)\n")
cat("=============================================================================\n")
