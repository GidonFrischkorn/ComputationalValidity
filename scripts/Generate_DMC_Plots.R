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

# Shared plot elements
reliability_base <- list(
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray30"),
  stat_function(fun = sqrt, linetype = "dashed", color = "gray50", linewidth = 0.6),
  geom_point(alpha = 0.3, size = 1.5),
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2),
  scale_color_viridis_d(name = "Trials per Condition", option = "D", end = 0.8),
  coord_cartesian(ylim = c(-1, 1)),
  theme_manuscript,
  theme(
    strip.text = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill = "gray90", color = "black")
  )
)

genPar_labeller <- labeller(
  genPar = c(
    "A" = "Amplitude (A)",
    "tau" = "Temporal Dynamics (τ)",
    "muc" = "Controlled drift (μc)"
  )
)

# Panel (a): Difference scores
fig2a <- ggplot(reliability_recovery_data %>% filter(indicator == "difference"),
                aes(x = reliability, y = rec, color = as.factor(nTrials))) +
  facet_wrap(~ genPar, nrow = 1, labeller = genPar_labeller) +
  reliability_base +
  labs(x = "Reliability of Difference Scores",
       y = "Parameter Recovery (Correlation)")

# Panel (b): Mean scores
fig2b <- ggplot(reliability_recovery_data %>% filter(indicator == "mean"),
                aes(x = reliability, y = rec, color = as.factor(nTrials))) +
  facet_wrap(~ genPar, nrow = 1, labeller = genPar_labeller) +
  reliability_base +
  labs(x = "Reliability of Mean Scores",
       y = "Parameter Recovery (Correlation)")

# Combine panels
fig2 <- fig2a / fig2b +
  plot_annotation(tag_levels = "A") +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(here("figures", "manuscript", "DMC_Reliability_Paradox.png"),
       fig2, width = 12, height = 8, dpi = 600)
ggsave(here("figures", "manuscript", "DMC_Reliability_Paradox.pdf"),
       fig2, width = 12, height = 8)
ggsave(here("figures", "manuscript", "DMC_Reliability_Paradox.tiff"),
       fig2, width = 12, height = 8, dpi = 600, compression = "lzw")

cat("  Figure 2 saved successfully!\n")

# =============================================================================
# PART 2: CORRELATION SIMULATION - Nuanced Perspective
# =============================================================================

data(dmc_correlation_recCorrs, package = "ComputationalValidity")

# Rename for consistency
df_correlations_all <- data.table(dmc_correlation_recCorrs)

# =============================================================================
# FIGURE 3: Correlation Transfer - The Limits of Validity
# =============================================================================

# Prepare correlation transfer data
correlation_transfer <- df_correlations_all %>%
  filter(
    SampleSize == "N = 200",
    nTrials == "100",
    indicator %in% c("difference", "mean"),
    measure %in% c("RT", "PC"),
    correlated_par %in% c("muc", "A", "tau")
  ) %>%
  mutate(
    gen_param_cor = case_when(
      correlated_par == "muc" ~ emp_corr.muc,
      correlated_par == "A" ~ emp_corr.A,
      correlated_par == "tau" ~ emp_corr.tau,
      TRUE ~ NA_real_
    ),
    param_label = factor(case_when(
      correlated_par == "muc" ~ "Controlled Drift (μc)",
      correlated_par == "A" ~ "Amplitude (A)",
      correlated_par == "tau" ~ "Temporal Dynamics (τ)",
      TRUE ~ correlated_par
    ), levels = c(
      "Amplitude (A)", "Controlled Drift (μc)", "Temporal Dynamics (τ)"
    )),
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

fig3 <- ggplot(correlation_transfer,
               aes(x = gen_param_cor, y = correlation_corrected, color = measure_label)) +
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

ggsave(here("figures", "manuscript", "DMC_Correlation_Transfer.png"),
       fig3, width = 10, height = 6, dpi = 600)
ggsave(here("figures", "manuscript", "DMC_Correlation_Transfer.pdf"),
       fig3, width = 10, height = 6)
ggsave(here("figures", "manuscript", "DMC_Correlation_Transfer.tiff"),
       fig3, width = 10, height = 6, dpi = 600, compression = "lzw")

cat("  Figure 3 saved successfully!\n")

# =============================================================================
# FIGURE 4: Intermediate Modeling - ezDM vs Behavioral Indicators
# =============================================================================

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
