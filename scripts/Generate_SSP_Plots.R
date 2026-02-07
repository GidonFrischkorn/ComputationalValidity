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
# FIGURE S1: SSP Parameter Confounding Matrix
# =============================================================================
cat("Generating SSP Figure S1: Parameter Confounding Matrix...\n")

create_confound_matrix <- function(data, indicator_type, indicator_label) {
  recovery_wide <- data %>%
    filter(
      genPar %in% c("b", "non_dec", "p", "sd_0"),
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

  param_cors <- cor(recovery_wide %>% select(-nRep), use = "pairwise.complete.obs")

  confound_data <- as.data.frame(as.table(param_cors))
  names(confound_data) <- c("Param1", "Param2", "Correlation")

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

confound_diff_ssp <- create_confound_matrix(df_recovery_ssp, "difference", "Difference Scores\n(Incomp - Comp)")
confound_mean_ssp <- create_confound_matrix(df_recovery_ssp, "mean", "Mean Scores\n(Average RT/PC)")

confound_data_all_ssp <- rbind(confound_diff_ssp, confound_mean_ssp)

figS1 <- ggplot(confound_data_all_ssp, aes(x = Param1, y = Param2, fill = Correlation)) +
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

ggsave(here("figures", "manuscript", "SSP_Parameter_Confounding.png"),
       figS1, width = 14, height = 7, dpi = 600)
ggsave(here("figures", "manuscript", "SSP_Parameter_Confounding.pdf"),
       figS1, width = 14, height = 7)
ggsave(here("figures", "manuscript", "SSP_Parameter_Confounding.tiff"),
       figS1, width = 14, height = 7, dpi = 600, compression = "lzw")

# =============================================================================
# FIGURE S2: SSP Reliability Paradox
# =============================================================================
cat("Generating SSP Figure S2: Reliability Paradox...\n")

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
  filter(!is.na(reliability)) %>%  # Remove rows without reliability data
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
       figS2, width = 10, height = 10, dpi = 600)
ggsave(here("figures", "manuscript", "SSP_Reliability_Paradox.pdf"),
       figS2, width = 10, height = 10)
ggsave(here("figures", "manuscript", "SSP_Reliability_Paradox.tiff"),
       figS2, width = 10, height = 10, dpi = 600, compression = "lzw")

# =============================================================================
# FIGURE S3: SSP Differential Indicator Validity
# =============================================================================
cat("Generating SSP Figure S3: Differential Indicator Validity...\n")

recovery_differential_ssp <- df_recovery_ssp %>%
  filter(
    genPar %in% c("b", "p", "sd_0"),
    measure %in% c("RT", "PC"),
    indicator %in% c("difference", "mean"),
    SampleSize == "N = 100",
    nTrials == "100"
  ) %>%
  mutate(
    param_label = case_when(
      genPar == "b" ~ "Boundary (b)\nResponse caution",
      genPar == "p" ~ "Perceptual Input (p)\nStimulus strength",
      genPar == "sd_0" ~ "Initial Noise (sd_0)\nStarting point variability",
      TRUE ~ genPar
    ),
    indicator_label = case_when(
      indicator == "difference" ~ "Difference Score\n(Incomp - Comp)",
      indicator == "mean" ~ "Mean Score\n(Average)",
      TRUE ~ indicator
    )
  )

figS3 <- ggplot(recovery_differential_ssp,
               aes(x = param_label, y = rec, fill = measure)) +
  facet_grid(indicator_label ~ ., scales = "free_y") +
  geom_hline(yintercept = 0, linetype = "solid", color = "gray60", linewidth = 0.8) +
  geom_hline(yintercept = c(-0.7, -0.5, -0.3, 0.3, 0.5, 0.7), linetype = "dashed",
             color = "gray40", alpha = 0.4) +
  geom_violin(position = position_dodge(width = 0.9), alpha = 0.6) +
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

ggsave(here("figures", "manuscript", "SSP_Differential_Validity.png"),
       figS3, width = 10, height = 10, dpi = 600)
ggsave(here("figures", "manuscript", "SSP_Differential_Validity.pdf"),
       figS3, width = 10, height = 10)
ggsave(here("figures", "manuscript", "SSP_Differential_Validity.tiff"),
       figS3, width = 10, height = 10, dpi = 600, compression = "lzw")

# =============================================================================
# FIGURE S4: SSP Correlation Transfer
# =============================================================================
cat("Generating SSP Figure S4: Correlation Transfer...\n")

correlation_transfer_ssp <- df_correlations_ssp %>%
  filter(
    SampleSize == "N = 200",
    nTrials == "100",
    indicator == "difference",
    measure %in% c("RT", "PC"),
    correlated_par %in% c("p", "sd_0", "p-sd_0")
  ) %>%
  mutate(
    gen_param_cor = case_when(
      correlated_par == "p" ~ emp_corr.p,
      correlated_par == "sd_0" ~ emp_corr.sd_0,
      correlated_par == "p-sd_0" ~ (emp_corr.p + emp_corr.sd_0) / 2,
      TRUE ~ NA_real_
    ),
    param_label = case_when(
      correlated_par == "p" ~ "Perceptual Input (p)",
      correlated_par == "sd_0" ~ "Initial Noise (sd_0)",
      correlated_par == "p-sd_0" ~ "Multiple Parameters\n(p, sd_0)",
      TRUE ~ correlated_par
    ),
    measure_label = ifelse(measure == "RT", "Response Time", "Proportion Correct")
  ) %>%
  filter(!is.na(gen_param_cor))

figS4 <- ggplot(correlation_transfer_ssp,
               aes(x = gen_param_cor, y = correlation, color = measure_label)) +
  facet_wrap(~ param_label, nrow = 1) +
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

ggsave(here("figures", "manuscript", "SSP_Correlation_Transfer.png"),
       figS4, width = 13, height = 5, dpi = 600)
ggsave(here("figures", "manuscript", "SSP_Correlation_Transfer.pdf"),
       figS4, width = 13, height = 5)
ggsave(here("figures", "manuscript", "SSP_Correlation_Transfer.tiff"),
       figS4, width = 13, height = 5, dpi = 600, compression = "lzw")

# =============================================================================
# FIGURE S5: SSP Model-Guided Indicator Selection
# =============================================================================
cat("Generating SSP Figure S5: Model-Guided Indicator Selection...\n")

recovery_for_selection_ssp <- df_recovery_ssp %>%
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
    q25 = quantile(rec, 0.25, na.rm = TRUE),
    q75 = quantile(rec, 0.75, na.rm = TRUE),
    prop_positive = mean(rec > 0, na.rm = TRUE),
    prop_strong = mean(rec > 0.5, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    param_label = case_when(
      genPar == "b" ~ "Boundary",
      genPar == "p" ~ "Perceptual Input",
      genPar == "sd_0" ~ "Initial Noise"
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

figS5 <- ggplot(recovery_for_selection_ssp,
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
    x = "Target Construct (SSP Parameter)",
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

ggsave(here("figures", "manuscript", "SSP_Model_Guided_Selection.png"),
       figS5, width = 10, height = 7, dpi = 600)
ggsave(here("figures", "manuscript", "SSP_Model_Guided_Selection.pdf"),
       figS5, width = 10, height = 7)
ggsave(here("figures", "manuscript", "SSP_Model_Guided_Selection.tiff"),
       figS5, width = 10, height = 7, dpi = 600, compression = "lzw")

# =============================================================================
# FIGURE S6: SSP Intermediate Modeling
# =============================================================================
cat("Generating SSP Figure S6: Intermediate Modeling...\n")

recovery_comparison_ssp <- df_recovery_ssp %>%
  filter(
    genPar %in% c("b", "non_dec", "p", "sd_0"),
    measure %in% c("RT", "PC", "drift", "boundary", "non_dec"),
    indicator == "difference",
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
      measure == "RT" ~ "RT Difference",
      measure == "PC" ~ "PC Difference",
      measure == "drift" ~ "ezDM Drift",
      measure == "boundary" ~ "ezDM Boundary",
      measure == "non_dec" ~ "ezDM Non-Decision",
      TRUE ~ measure
    )
  ) %>%
  filter(measure_type %in% c("Behavioral", "ezDM"))

recovery_summary_comp_ssp <- recovery_comparison_ssp %>%
  group_by(param_label, measure_label, measure_type) %>%
  summarize(
    median_rec = median(rec, na.rm = TRUE),
    mean_rec = mean(rec, na.rm = TRUE),
    q25 = quantile(rec, 0.25, na.rm = TRUE),
    q75 = quantile(rec, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

figS6 <- ggplot(recovery_summary_comp_ssp,
               aes(x = measure_label, y = median_rec, fill = measure_type)) +
  facet_wrap(~ param_label, nrow = 1, scales = "free_x") +
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
       figS6, width = 14, height = 5, dpi = 600)
ggsave(here("figures", "manuscript", "SSP_Intermediate_Modeling.pdf"),
       figS6, width = 14, height = 5)
ggsave(here("figures", "manuscript", "SSP_Intermediate_Modeling.tiff"),
       figS6, width = 14, height = 5, dpi = 600, compression = "lzw")

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

# Model-guided selection statistics
cat("\n\nSSP MODEL-GUIDED INDICATOR SELECTION (N=100, 100 trials/condition):\n")
cat("Best indicators for each parameter:\n")
best_indicators_ssp <- recovery_for_selection_ssp %>%
  mutate(abs_rec = abs(median_rec)) %>%
  group_by(param_label) %>%
  slice_max(abs_rec, n = 1) %>%
  select(param_label, indicator_measure, median_rec, validity_strength)
print(best_indicators_ssp)

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
