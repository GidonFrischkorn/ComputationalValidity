# start fresh
rm(list = ls())   # clean up work space
graphics.off()  # switch off graphics device

# use relative paths to load & save data
pacman::p_load(here, tidytable, data.table, ggplot2)

## Load Package Data -----------------------------------------------------------
data(m3_recovery_parRecovery, package = "ComputationalValidity")
data(m3_recovery_behavior, package = "ComputationalValidity")
data(m3_recovery_reliability, package = "ComputationalValidity")

df_recovery_all <- data.table(m3_recovery_parRecovery)
df_behavior_all <- data.table(m3_recovery_behavior)
df_reliability_all <- data.table(m3_recovery_reliability)

clean_plot <- theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.key = element_rect(fill = "white"),
        text = element_text(size = 15),
        line = element_line(linewidth = 1),
        axis.ticks = element_line(linewidth = 1))

## =============================================================================
## 1) Behavioral Indicators
## =============================================================================

# Average response proportions across participants within each replication
avg_effects_behavior <- df_behavior_all %>%
  summarize(mean = mean(value),
            sd = sd(value),
            .by = c(nRep, SampleSize, nTrials, measure, indicator))

# Plot 1: Response proportions by sample size and trials (raw categories)
ggplot(data = avg_effects_behavior %>%
         filter(measure == "proportion"),
       aes(x = indicator, y = mean, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ .) +
  geom_boxplot() +
  labs(x = "Response Category", y = "Mean Proportion",
       fill = "Number of\nTrials") +
  clean_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 2: Composite indicators by sample size and trials
ggplot(data = avg_effects_behavior %>%
         filter(measure == "composite"),
       aes(x = indicator, y = mean, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ .) +
  geom_boxplot() +
  labs(x = "Composite Indicator", y = "Mean Value",
       fill = "Number of\nTrials") +
  clean_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 3: SD of indicators (individual differences)
ggplot(data = avg_effects_behavior %>%
         filter(measure == "proportion"),
       aes(x = indicator, y = sd, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ .) +
  geom_boxplot() +
  labs(x = "Response Category", y = "SD of Proportion",
       fill = "Number of\nTrials") +
  clean_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## =============================================================================
## 2) Reliability
## =============================================================================

# Join average effects to reliability for later plots
df_reliability_all <- df_reliability_all %>%
  left_join(avg_effects_behavior)

# Plot 4: Reliability by indicator and trial count
ggplot(data = df_reliability_all,
       aes(x = as.factor(nTrials), y = reliability, fill = as.factor(SampleSize))) +
  facet_grid(indicator ~ measure) +
  geom_boxplot() +
  labs(x = "Number of Trials", y = "Estimated Reliability",
       fill = "Sample Size") +
  clean_plot

# Plot 5: Reliability by indicator (proportions only, cleaner view)
ggplot(data = df_reliability_all %>% filter(measure == "proportion"),
       aes(x = as.factor(nTrials), y = reliability, fill = as.factor(SampleSize))) +
  facet_wrap(~ indicator, nrow = 1) +
  geom_boxplot() +
  labs(x = "Number of Trials", y = "Estimated Reliability",
       fill = "Sample Size") +
  clean_plot

# Plot 6: Reliability by indicator (composites only)
ggplot(data = df_reliability_all %>% filter(measure == "composite"),
       aes(x = as.factor(nTrials), y = reliability, fill = as.factor(SampleSize))) +
  facet_wrap(~ indicator, nrow = 1) +
  geom_boxplot() +
  labs(x = "Number of Trials", y = "Estimated Reliability",
       fill = "Sample Size") +
  clean_plot

# Plot 7: Reliability vs. SD of indicator (individual differences drive reliability)
ggplot(data = df_reliability_all %>% filter(measure == "proportion"),
       aes(x = sd, y = reliability, color = as.factor(SampleSize))) +
  facet_grid(nTrials ~ indicator, scales = "free_x") +
  ylim(-1, 1) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(x = "SD of Proportion", y = "Estimated Reliability",
       color = "Sample Size") +
  clean_plot

## =============================================================================
## 3) Parameter Recovery — Proportions
## =============================================================================

# Join average effects to recovery
df_recovery_all <- df_recovery_all %>%
  left_join(avg_effects_behavior)

# Plot 8: Recovery of M3 parameters from raw proportions
ggplot(data = df_recovery_all %>%
         filter(measure == "proportion"),
       aes(x = genPar, y = rec, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ indicator) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "Generating M3 Parameter", y = "Recovery",
       fill = "Number of\nTrials") +
  clean_plot

# Plot 9: Recovery of M3 parameters from composite indicators
ggplot(data = df_recovery_all %>%
         filter(measure == "composite"),
       aes(x = genPar, y = rec, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ indicator) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "Generating M3 Parameter", y = "Recovery",
       fill = "Number of\nTrials") +
  clean_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## =============================================================================
## 4) Parameter Recovery — Corrected for Reliability
## =============================================================================

# Plot 10: Reliability-corrected recovery (proportions, largest sample)
ggplot(data = df_recovery_all %>%
         filter(measure == "proportion",
                SampleSize == "N = 100"),
       aes(x = genPar, y = rec_corrected, fill = as.factor(nTrials))) +
  facet_grid(. ~ indicator) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "Generating M3 Parameter",
       y = "Recovery (Corrected for Reliability)",
       fill = "Number of\nTrials") +
  clean_plot

# Plot 11: Reliability-corrected recovery (composites, largest sample)
ggplot(data = df_recovery_all %>%
         filter(measure == "composite",
                SampleSize == "N = 100"),
       aes(x = genPar, y = rec_corrected, fill = as.factor(nTrials))) +
  facet_grid(. ~ indicator) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "Generating M3 Parameter",
       y = "Recovery (Corrected for Reliability)",
       fill = "Number of\nTrials") +
  clean_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## =============================================================================
## 5) Recovery x Mean Effect Size Interaction
## =============================================================================

# Plot 12: Does average accuracy moderate recovery? (accuracy indicator)
ggplot(data = df_recovery_all %>%
         filter(measure == "proportion",
                indicator == "corr"),
       aes(x = mean, y = rec, color = as.factor(SampleSize))) +
  facet_grid(nTrials ~ genPar) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "Mean P(Correct) in the Sample", y = "Recovery",
       color = "Sample Size") +
  clean_plot

# Plot 13: Does SD of accuracy moderate recovery?
ggplot(data = df_recovery_all %>%
         filter(measure == "proportion",
                indicator == "corr"),
       aes(x = sd, y = rec, color = as.factor(SampleSize))) +
  facet_grid(nTrials ~ genPar, scales = "free_x") +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(x = "SD of P(Correct) in the Sample", y = "Recovery",
       color = "Sample Size") +
  clean_plot
