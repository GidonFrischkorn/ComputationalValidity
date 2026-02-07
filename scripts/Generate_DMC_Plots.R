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
# FIGURE 1: Parameter Confounding Matrix (Equifinality)
# =============================================================================
cat("Generating Figure 1: Parameter Confounding Matrix (Difference & Mean Indicators)...\n")

# Create confounding matrices for both difference and mean indicators
# These are the most relevant behavioral indicators

create_confound_matrix <- function(data, indicator_type, indicator_label) {
  recovery_wide <- data %>%
    filter(
      genPar %in% c("A", "tau", "b", "muc", "non_dec"),
      measure %in% c("RT", "PC"),
      indicator == indicator_type,
      SampleSize == "N = 100",
      nTrials == "100"
    ) %>%
    select(nRep, genPar, measure, rec) %>%
    pivot_wider(
      names_from = c(genPar, measure),
      values_from = rec,
      names_sep = "_"
    )

  # Calculate correlation matrix
  param_cors <- cor(recovery_wide %>% select(-nRep), use = "pairwise.complete.obs")

  # Create heatmap data
  confound_data <- as.data.frame(as.table(param_cors))
  names(confound_data) <- c("Param1", "Param2", "Correlation")

  # Clean up parameter names for display
  confound_data <- confound_data %>%
    mutate(
      Param1 = gsub("_", " (", Param1),
      Param1 = paste0(Param1, ")"),
      Param2 = gsub("_", " (", Param2),
      Param2 = paste0(Param2, ")"),
      Indicator = indicator_label
    )

  return(confound_data)
}

# Generate data for both indicators
confound_diff <- create_confound_matrix(df_recovery_all, "difference", "Difference Scores\n(Incomp - Comp)")
confound_mean <- create_confound_matrix(df_recovery_all, "mean", "Mean Scores\n(Average RT/PC)")

# Combine data using rbind
confound_data_all <- rbind(confound_diff, confound_mean)

# Verify data structure
cat(paste0("  Combined data rows: ", nrow(confound_data_all), "\n"))
cat(paste0("  Unique Indicators: ", length(unique(confound_data_all$Indicator)), "\n"))
cat(paste0("  Column class: ", class(confound_data_all$Indicator), "\n"))
cat(paste0("  Column names: ", paste(names(confound_data_all), collapse=", "), "\n"))

# Ensure directories exist
dir.create(here("figures", "manuscript"), recursive = TRUE, showWarnings = FALSE)

# Create faceted plot
fig1 <- ggplot(confound_data_all, aes(x = Param1, y = Param2, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", Correlation)),
            size = 2.5, color = "black") +
  scale_fill_gradient2(
    low = "#2166AC", mid = "white", high = "#B2182B",
    midpoint = 0, limits = c(-1, 1),
    name = "Correlation"
  ) +
  facet_wrap(~ Indicator, ncol = 2) +
  labs(
    x = "",
    y = ""
  ) +
  theme_manuscript +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    legend.position = "right",
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(size = 11, face = "bold"),
    strip.background = element_rect(fill = "gray90", color = "black")
  ) +
  coord_equal()

cat("  Plot object created, attempting to save...\n")

ggsave(here("figures", "manuscript", "DMC_Parameter_Confounding.png"),
       fig1, width = 14, height = 7, dpi = 600)
ggsave(here("figures", "manuscript", "DMC_Parameter_Confounding.pdf"),
       fig1, width = 14, height = 7)
ggsave(here("figures", "manuscript", "DMC_Parameter_Confounding.tiff"),
       fig1, width = 14, height = 7, dpi = 600, compression = "lzw")

cat("  Figure 1 saved successfully!\n")

# =============================================================================
# FIGURE 2: The Reliability Paradox
# =============================================================================
cat("Generating Figure 2: The Reliability Paradox...\n")

# For Figure 2, we want to show that high INDICATOR reliability doesn't guarantee
# parameter recovery. Compare difference vs mean scores for RT indicators.

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

# =============================================================================
# FIGURE 3: Differential Indicator Validity
# =============================================================================
cat("Generating Figure 3: Differential Indicator Validity...\n")

# KEY FIGURE: Shows that different indicators recover different parameters
# This demonstrates the central thesis - model-based validation reveals
# which indicators are valid for which theoretical constructs

# Prepare data showing RT vs PC for key parameters, comparing difference vs mean scores
recovery_differential <- df_recovery_all %>%
  filter(
    genPar %in% c("A", "tau", "muc"),  # Focus on key conflict parameters
    measure %in% c("RT", "PC"),
    indicator %in% c("difference", "mean"),  # Compare difference and mean scores
    SampleSize == "N = 100",
    nTrials == "100"  # Focus on one condition for clarity
  ) %>%
  mutate(
    param_label = case_when(
      genPar == "A" ~ "Amplitude (A)\nAutomatic activation strength",
      genPar == "tau" ~ "Temporal Dynamics (τ)\nAutomatic activation build-up",
      genPar == "muc" ~ "Controlled Drift (μc)\nControlled processing rate",
      TRUE ~ genPar
    ),
    indicator_label = case_when(
      indicator == "difference" ~ "Difference Score\n(Incomp - Comp)",
      indicator == "mean" ~ "Mean Score\n(Average)",
      TRUE ~ indicator
    )
  )

fig3 <- ggplot(recovery_differential,
               aes(x = param_label, y = rec, fill = measure)) +
  facet_grid(indicator_label ~ ., scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray60", linewidth = 0.8) +
  geom_hline(yintercept = c(-0.7, -0.5, -0.3, 0.3, 0.5, 0.7), linetype = "dashed",
             color = "gray40", alpha = 0.4) +
  geom_violin(position = position_dodge(width = 0.9), alpha = 0.6,
              draw_quantiles = c(0.25, 0.5, 0.75)) +
  geom_boxplot(position = position_dodge(width = 0.9), width = 0.2,
               alpha = 0.8, outlier.alpha = 0.3, show.legend = FALSE) +
  scale_fill_manual(
    name = "Behavioral Indicator",
    values = c("RT" = "#E69F00", "PC" = "#56B4E9"),
    labels = c("RT" = "Response Time", "PC" = "Proportion Correct")
  ) +
  labs(
    x = "",
    y = "Parameter Recovery (Correlation)"
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_manuscript +
  theme(
    axis.text.x = element_text(size = 10, hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    strip.text.y = element_text(size = 11, face = "bold"),
    strip.background = element_rect(fill = "gray90", color = "black")
  )

ggsave(here("figures", "manuscript", "DMC_Differential_Validity.png"),
       fig3, width = 10, height = 10, dpi = 600)
ggsave(here("figures", "manuscript", "DMC_Differential_Validity.pdf"),
       fig3, width = 10, height = 10)
ggsave(here("figures", "manuscript", "DMC_Differential_Validity.tiff"),
       fig3, width = 10, height = 10, dpi = 600, compression = "lzw")

# =============================================================================
# PART 2: CORRELATION SIMULATION - Nuanced Perspective
# =============================================================================

# Load package datasets
cat("Loading DMC correlation data from package...\n")
data(dmc_correlation_recCorrs, package = "ComputationalValidity")

# Rename for consistency
df_correlations_all <- data.table(dmc_correlation_recCorrs)

# =============================================================================
# FIGURE 4: Correlation Transfer - The Limits of Validity
# =============================================================================
cat("Generating Figure 4: Correlation Transfer...\n")

# This figure provides nuance: valid indicators don't always track parameter correlations
# This is important for understanding the LIMITS of indicator validity

# Prepare correlation transfer data
correlation_transfer <- df_correlations_all %>%
  filter(
    SampleSize == "N = 200",
    nTrials == "100",
    indicator == "difference",
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
    measure_label = ifelse(measure == "RT", "Response Time", "Proportion Correct")
  ) %>%
  filter(!is.na(gen_param_cor))

fig4 <- ggplot(correlation_transfer,
               aes(x = gen_param_cor, y = correlation_corrected, color = measure_label)) +
  facet_wrap(~ param_label, nrow = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed",
              color = "gray30", linewidth = 0.8) +
  geom_point(alpha = 0.4, size = 2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
  scale_color_manual(
    name = "Behavioral Indicator",
    values = c("Response Time" = "#E69F00", "Proportion Correct" = "#56B4E9")
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
       fig4, width = 13, height = 5, dpi = 600)
ggsave(here("figures", "manuscript", "DMC_Correlation_Transfer.pdf"),
       fig4, width = 13, height = 5)
ggsave(here("figures", "manuscript", "DMC_Correlation_Transfer.tiff"),
       fig4, width = 13, height = 5, dpi = 600, compression = "lzw")

# =============================================================================
# FIGURE 5: Model-Guided Indicator Selection
# =============================================================================
cat("Generating Figure 5: Model-Guided Indicator Selection...\n")

# This figure shows HOW to use model-based validation to select indicators
# Shows the same data as Figure 3 but organized to emphasize decision-making
# Include both difference and mean scores to show differential validity

# Calculate recovery quality categories
recovery_for_selection <- df_recovery_all %>%
  filter(
    genPar %in% c("A", "tau", "muc"),
    measure %in% c("RT", "PC"),
    indicator %in% c("difference", "mean"),  # Include both scoring methods
    SampleSize == "N = 100",
    nTrials == "100"
  ) %>%
  group_by(genPar, measure, indicator) %>%
  summarize(
    median_rec = median(rec, na.rm = TRUE),
    q25 = quantile(rec, 0.25, na.rm = TRUE),
    q75 = quantile(rec, 0.75, na.rm = TRUE),
    prop_positive = mean(rec > 0, na.rm = TRUE),
    prop_strong = mean(rec > 0.5, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    param_label = case_when(
      genPar == "A" ~ "Amplitude",
      genPar == "tau" ~ "Temporal Dynamics",
      genPar == "muc" ~ "Controlled Drift"
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

# Create decision matrix heatmap
fig5 <- ggplot(recovery_for_selection,
               aes(x = param_label, y = indicator_measure, fill = abs(median_rec))) +
  geom_tile(color = "white", linewidth = 1) +
  geom_text(aes(label = sprintf("%.2f\n(%s)", median_rec, validity_strength)),
            size = 3.5, fontface = "bold") +
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
    axis.text.x = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  coord_equal()

ggsave(here("figures", "manuscript", "DMC_Model_Guided_Selection.png"),
       fig5, width = 10, height = 7, dpi = 600)
ggsave(here("figures", "manuscript", "DMC_Model_Guided_Selection.pdf"),
       fig5, width = 10, height = 7)
ggsave(here("figures", "manuscript", "DMC_Model_Guided_Selection.tiff"),
       fig5, width = 10, height = 7, dpi = 600, compression = "lzw")

# =============================================================================
# FIGURE 6: Intermediate Modeling - ezDM vs Behavioral Indicators
# =============================================================================
cat("Generating Figure 6: Intermediate Modeling...\n")

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
fig6 <- ggplot(recovery_summary_comp,
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
       fig6, width = 14, height = 8, dpi = 600)
ggsave(here("figures", "manuscript", "DMC_Intermediate_Modeling.pdf"),
       fig6, width = 14, height = 8)
ggsave(here("figures", "manuscript", "DMC_Intermediate_Modeling.tiff"),
       fig6, width = 14, height = 8, dpi = 600, compression = "lzw")

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
