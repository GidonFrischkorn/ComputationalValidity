# start fresh
rm(list = ls())   # clean up work space
graphics.off()  # switch off graphics device

# use relative paths to load & save data
pacman::p_load(here, tidytable, data.table, ggplot2)

## Load Package Data -----------------------------------------------------------
data(m3_correlation_recCorrs, package = "ComputationalValidity")
data(m3_correlation_behavior, package = "ComputationalValidity")
data(m3_correlation_reliability, package = "ComputationalValidity")

df_correlation_all <- data.table(m3_correlation_recCorrs)
df_behavior_all <- data.table(m3_correlation_behavior)
df_reliability_all <- data.table(m3_correlation_reliability)

clean_plot <- theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.key = element_rect(fill = "white"),
        text = element_text(size = 15),
        line = element_line(linewidth = 1),
        axis.ticks = element_line(linewidth = 1))

## =============================================================================
## 1) Behavioral Indicators (Correlation Simulation)
## =============================================================================

avg_effects_behavior <- df_behavior_all %>%
  summarize(mean = mean(value),
            sd = sd(value),
            .by = c(nRep, SampleSize, nTrials, measure, indicator))

# Plot 1: Response proportions across replications
ggplot(data = avg_effects_behavior %>%
         filter(measure == "proportion"),
       aes(x = indicator, y = mean, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ .) +
  geom_boxplot() +
  labs(x = "Response Category", y = "Mean Proportion",
       fill = "Number of\nTrials") +
  clean_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## =============================================================================
## 2) Reliability (Correlation Simulation)
## =============================================================================

# Plot 2: Reliability by indicator and trial count
ggplot(data = df_reliability_all,
       aes(x = as.factor(nTrials), y = reliability, fill = as.factor(SampleSize))) +
  facet_grid(indicator ~ measure) +
  geom_boxplot() +
  labs(x = "Number of Trials", y = "Estimated Reliability",
       fill = "Sample Size") +
  clean_plot

# Plot 3: Reliability for proportions (cleaner view)
ggplot(data = df_reliability_all %>% filter(measure == "proportion"),
       aes(x = as.factor(nTrials), y = reliability)) +
  facet_wrap(~ indicator, nrow = 1) +
  geom_boxplot() +
  labs(x = "Number of Trials", y = "Estimated Reliability") +
  clean_plot

## =============================================================================
## 3) Correlation Transfer — Single Parameter Conditions
## =============================================================================

# Plot 4: Observed vs. empirical correlations for parameter c
ggplot(data = df_correlation_all %>%
         filter(correlated_par == "c",
                measure == "proportion"),
       aes(y = correlation, x = emp_corr.c, color = as.factor(nTrials))) +
  facet_wrap(~ indicator, nrow = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") +
  coord_cartesian(ylim = c(-0.5, 1)) +
  labs(x = "Empirical Parameter Correlation (c)",
       y = "Indicator Cross-Task Correlation",
       color = "Number of\nTrials",
       title = "Correlated Parameter: c (Context Binding)") +
  clean_plot

# Plot 5: Observed vs. empirical correlations for parameter a
ggplot(data = df_correlation_all %>%
         filter(correlated_par == "a",
                measure == "proportion"),
       aes(y = correlation, x = emp_corr.a, color = as.factor(nTrials))) +
  facet_wrap(~ indicator, nrow = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") +
  coord_cartesian(ylim = c(-0.5, 1)) +
  labs(x = "Empirical Parameter Correlation (a)",
       y = "Indicator Cross-Task Correlation",
       color = "Number of\nTrials",
       title = "Correlated Parameter: a (Item Activation)") +
  clean_plot

# Plot 6: Observed vs. empirical correlations for parameter f
ggplot(data = df_correlation_all %>%
         filter(correlated_par == "f",
                measure == "proportion"),
       aes(y = correlation, x = emp_corr.f, color = as.factor(nTrials))) +
  facet_wrap(~ indicator, nrow = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") +
  coord_cartesian(ylim = c(-0.5, 1)) +
  labs(x = "Empirical Parameter Correlation (f)",
       y = "Indicator Cross-Task Correlation",
       color = "Number of\nTrials",
       title = "Correlated Parameter: f (Filtering)") +
  clean_plot

## =============================================================================
## 4) Correlation Transfer — Composite Indicators
## =============================================================================

# Plot 7: Composites for parameter c
ggplot(data = df_correlation_all %>%
         filter(correlated_par == "c",
                measure == "composite"),
       aes(y = correlation, x = emp_corr.c, color = as.factor(nTrials))) +
  facet_wrap(~ indicator, nrow = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") +
  coord_cartesian(ylim = c(-0.5, 1)) +
  labs(x = "Empirical Parameter Correlation (c)",
       y = "Indicator Cross-Task Correlation",
       color = "Number of\nTrials",
       title = "Composites — Correlated Parameter: c") +
  clean_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 8: Composites for parameter a
ggplot(data = df_correlation_all %>%
         filter(correlated_par == "a",
                measure == "composite"),
       aes(y = correlation, x = emp_corr.a, color = as.factor(nTrials))) +
  facet_wrap(~ indicator, nrow = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") +
  coord_cartesian(ylim = c(-0.5, 1)) +
  labs(x = "Empirical Parameter Correlation (a)",
       y = "Indicator Cross-Task Correlation",
       color = "Number of\nTrials",
       title = "Composites — Correlated Parameter: a") +
  clean_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Plot 9: Composites for parameter f
ggplot(data = df_correlation_all %>%
         filter(correlated_par == "f",
                measure == "composite"),
       aes(y = correlation, x = emp_corr.f, color = as.factor(nTrials))) +
  facet_wrap(~ indicator, nrow = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") +
  coord_cartesian(ylim = c(-0.5, 1)) +
  labs(x = "Empirical Parameter Correlation (f)",
       y = "Indicator Cross-Task Correlation",
       color = "Number of\nTrials",
       title = "Composites — Correlated Parameter: f") +
  clean_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## =============================================================================
## 5) Correlation Transfer — Multi-Parameter Conditions
## =============================================================================

# Plot 10: When c and a are jointly correlated — proportions
ggplot(data = df_correlation_all %>%
         filter(correlated_par == "c-a",
                measure == "proportion"),
       aes(y = correlation, x = emp_corr.c, color = as.factor(nTrials))) +
  facet_wrap(~ indicator, nrow = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") +
  coord_cartesian(ylim = c(-0.5, 1)) +
  labs(x = "Empirical Parameter Correlation (c)",
       y = "Indicator Cross-Task Correlation",
       color = "Number of\nTrials",
       title = "Jointly Correlated: c + a") +
  clean_plot

# Plot 11: When all three parameters are jointly correlated
ggplot(data = df_correlation_all %>%
         filter(correlated_par == "c-a-f",
                measure == "proportion"),
       aes(y = correlation, x = emp_corr.c, color = as.factor(nTrials))) +
  facet_wrap(~ indicator, nrow = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") +
  coord_cartesian(ylim = c(-0.5, 1)) +
  labs(x = "Empirical Parameter Correlation (c)",
       y = "Indicator Cross-Task Correlation",
       color = "Number of\nTrials",
       title = "All Parameters Correlated: c + a + f") +
  clean_plot

## =============================================================================
## 6) Reliability-Corrected Correlations
## =============================================================================

# Plot 12: Corrected vs. uncorrected cross-task correlations (single params)
ggplot(data = df_correlation_all %>%
         filter(correlated_par %in% c("c", "a", "f"),
                measure == "proportion"),
       aes(y = correlation_corrected, x = emp_corr.c, color = as.factor(nTrials))) +
  facet_grid(correlated_par ~ indicator) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", formula = "y ~ x") +
  coord_cartesian(ylim = c(-0.5, 1.5)) +
  labs(x = "Empirical Parameter Correlation",
       y = "Corrected Cross-Task Correlation",
       color = "Number of\nTrials",
       title = "Reliability-Corrected Correlation Transfer") +
  clean_plot

## =============================================================================
## 7) Summary: Correlation Attenuation Overview
## =============================================================================

# Plot 13: Overall correlation attenuation — all single-parameter conditions
# Shows how much indicator correlations underestimate parameter correlations
corr_summary <- df_correlation_all %>%
  filter(correlated_par %in% c("c", "a", "f"),
         measure == "proportion") %>%
  mutate(
    emp_corr = case_when(
      correlated_par == "c" ~ emp_corr.c,
      correlated_par == "a" ~ emp_corr.a,
      correlated_par == "f" ~ emp_corr.f
    ),
    attenuation = correlation / emp_corr
  )

ggplot(data = corr_summary,
       aes(x = indicator, y = attenuation, fill = correlated_par)) +
  facet_wrap(~ nTrials, nrow = 1) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 0.5) +
  geom_boxplot() +
  coord_cartesian(ylim = c(-0.5, 1.5)) +
  labs(x = "Response Category",
       y = "Correlation Ratio (Indicator / Parameter)",
       fill = "Correlated\nParameter",
       title = "Correlation Transfer Efficiency") +
  clean_plot +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
