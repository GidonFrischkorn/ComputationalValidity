# =============================================================================
# Model Comparison: DMC vs SSP
# =============================================================================
# This script generates a direct comparison between DMC and SSP models
# to demonstrate the model-relativity of validity claims
# =============================================================================

# Setup ------------------------------------------------------------------------
rm(list = ls())
graphics.off()

pacman::p_load(here, tidytable, data.table, ggplot2, 
               patchwork, scales, viridis)

# Define clean theme
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
# LOAD DMC DATA
# =============================================================================
cat("Loading DMC data from package...\n")

# Load recovery and correlation data
data(dmc_recovery_parRecovery, package = "ComputationalValidity")
data(dmc_correlation_recCorrs, package = "ComputationalValidity")

# Add Model column and rename
df_recovery_dmc <- data.table(dmc_recovery_parRecovery)
df_recovery_dmc$Model <- "DMC"

df_correlations_dmc <- data.table(dmc_correlation_recCorrs)
df_correlations_dmc$Model <- "DMC"

# =============================================================================
# LOAD SSP DATA
# =============================================================================
cat("Loading SSP data from package...\n")

# Load recovery and correlation data
data(ssp_recovery_parRecovery, package = "ComputationalValidity")
data(ssp_correlation_recCorrs, package = "ComputationalValidity")

# Add Model column and rename
df_recovery_ssp <- data.table(ssp_recovery_parRecovery)
df_recovery_ssp$Model <- "SSP"

df_correlations_ssp <- data.table(ssp_correlation_recCorrs)
df_correlations_ssp$Model <- "SSP"

# =============================================================================
# FIGURE 7: Model-Relativity of Validity
# =============================================================================
cat("Generating Figure 7: Model-Relativity...\n")

# Combine recovery data for common parameters
recovery_comparison <- rbind(
  df_recovery_dmc %>%
    filter(
      genPar %in% c("b", "non_dec"),
      measure == "RT",
      indicator == "difference",
      SampleSize == "N = 100",
      nTrials == "100"
    ),
  df_recovery_ssp %>%
    filter(
      genPar %in% c("b", "non_dec"),
      measure == "RT",
      indicator == "difference",
      SampleSize == "N = 100",
      nTrials == "100"
    )
) %>%
  mutate(
    param_label = case_when(
      genPar == "b" ~ "Boundary (b)",
      genPar == "non_dec" ~ "Non-Decision Time (Ter)",
      TRUE ~ genPar
    )
  )

# Panel A: Recovery comparison for shared parameters
panel_a <- ggplot(recovery_comparison, 
                  aes(x = Model, y = rec, fill = Model)) +
  facet_wrap(~ param_label, nrow = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_boxplot(outlier.alpha = 0.3, show.legend = FALSE) +
  scale_fill_manual(
    values = c("DMC" = "#E41A1C", "SSP" = "#377EB8")
  ) +
  labs(
    title = "A. Shared Parameters Show Model-Specific Recovery",
    x = "",
    y = "Recovery\n(Correlation)"
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_manuscript

# Panel B: Model-specific parameters
model_specific_recovery <- rbind(
  df_recovery_dmc %>%
    filter(
      genPar %in% c("A", "muc", "tau"),
      measure == "RT",
      indicator == "difference",
      SampleSize == "N = 100",
      nTrials == "100"
    ) %>%
    mutate(param_type = "DMC-specific"),
  df_recovery_ssp %>%
    filter(
      genPar %in% c("p", "sd_0"),
      measure == "RT",
      indicator == "difference",
      SampleSize == "N = 100",
      nTrials == "100"
    ) %>%
    mutate(param_type = "SSP-specific")
)

panel_b <- ggplot(model_specific_recovery, 
                  aes(x = genPar, y = rec, fill = Model)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_manual(
    values = c("DMC" = "#E41A1C", "SSP" = "#377EB8"),
    name = "Model Architecture"
  ) +
  labs(
    title = "B. Model-Specific Parameters",
    x = "Parameter",
    y = "Recovery\n(Correlation)"
  ) +
  coord_cartesian(ylim = c(-1, 1)) +
  theme_manuscript +
  theme(legend.position = "bottom")

# Get indicator correlations for multiple parameters condition
# (Parameter correlations are already in the emp_corr columns)
indicator_cors_comparison <- rbind(
  df_correlations_dmc %>%
    filter(
      SampleSize == "N = 100", 
      nTrials == "100",
      correlated_par == "muc-A-tau",
      indicator == "difference",
      measure == "RT"
    ) %>%
    mutate(
      gen_param_cor = (emp_corr.muc + emp_corr.A + emp_corr.tau) / 3
    ),
  df_correlations_ssp %>%
    filter(
      SampleSize == "N = 100", 
      nTrials == "100",
      correlated_par == "p-b-sd_0",
      indicator == "difference",
      measure == "RT"
    ) %>%
    mutate(
      gen_param_cor = (emp_corr.p + emp_corr.b + emp_corr.sd_0) / 3
    )
) %>%
  pivot_longer(
    cols = c(gen_param_cor, obs_correlation),
    names_to = "correlation_type",
    values_to = "correlation"
  ) %>%
  mutate(
    type_label = ifelse(correlation_type == "gen_param_cor",
                        "Parameter",
                        "Indicator")
  )

# Panel C: Parameters vs Indicators across models
panel_c <- ggplot(indicator_cors_comparison, 
                  aes(x = type_label, y = correlation, fill = Model)) +
  geom_boxplot(position = position_dodge(width = 0.8),
               outlier.alpha = 0.3) +
  scale_fill_manual(
    values = c("DMC" = "#E41A1C", "SSP" = "#377EB8"),
    name = "Model Architecture"
  ) +
  labs(
    title = "C. Both Models Show Same Pattern",
    subtitle = "Parameters > Indicators",
    x = "Correlation Level",
    y = "Cross-Task\nCorrelation"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_manuscript +
  theme(legend.position = "bottom")

# Combine all panels
fig7 <- (panel_a / (panel_b + panel_c)) +
  plot_layout(heights = c(1, 1)) +
  plot_annotation(
    title = "Model-Relativity of Validity",
    subtitle = "Different process theories lead to different parameter interpretations",
    theme = theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0),
      plot.subtitle = element_text(size = 12, hjust = 0)
    )
  )

ggsave(here("figures", "manuscript", "Fig7_Model_Relativity.png"),
       fig7, width = 14, height = 10, dpi = 300)
ggsave(here("figures", "manuscript", "Fig7_Model_Relativity.pdf"),
       fig7, width = 14, height = 10)

# =============================================================================
# SUPPLEMENTARY FIGURE: Direct Behavioral Comparison
# =============================================================================
cat("Generating Supplementary Figure: Behavioral Similarity Despite Different Models...\n")

# This would require extracting behavioral data from both models
# and showing that similar behavioral patterns can arise from different
# parameter configurations across models (equifinality across architectures)

# For now, create a conceptual figure showing the distributions
# (Could be expanded with actual matching of behavioral patterns)

cat("\n=============================================================================\n")
cat("MODEL COMPARISON SUMMARY\n")
cat("=============================================================================\n\n")

# Recovery comparison for shared parameters
cat("SHARED PARAMETER RECOVERY COMPARISON (b, non_dec):\n")
shared_recovery_stats <- recovery_comparison %>%
  group_by(Model, param_label) %>%
  summarize(
    median_rec = median(rec, na.rm = TRUE),
    iqr = IQR(rec, na.rm = TRUE),
    .groups = "drop"
  )
print(shared_recovery_stats)

# Parameter vs indicator correlation
cat("\n\nPARAMETER VS INDICATOR CORRELATIONS (Multiple Parameters, RT):\n")
corr_summary <- indicator_cors_comparison %>%
  group_by(Model, type_label) %>%
  summarize(
    median_cor = median(correlation, na.rm = TRUE),
    iqr = IQR(correlation, na.rm = TRUE),
    .groups = "drop"
  )
print(corr_summary)

cat("\n=============================================================================\n")
cat("Model comparison figure saved to: figures/manuscript/\n")
cat("=============================================================================\n")
