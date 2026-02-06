# start fresh
rm(list = ls())   # clean up work space
graphics.off()  # switch off graphics device

# use relative paths to load & save data
pacman::p_load(here,tidytable,data.table, ggplot2)

## Load Package Data -----------------------------------------------------------
# Load SSP recovery datasets from package
data(ssp_recovery_parRecovery, package = "ComputationalValidity")
data(ssp_recovery_behavior, package = "ComputationalValidity")
data(ssp_recovery_ezDM, package = "ComputationalValidity")
data(ssp_recovery_reliability, package = "ComputationalValidity")

# Convert to data.table for analysis
df_recovery_all <- data.table(ssp_recovery_parRecovery)
df_behavior_all <- data.table(ssp_recovery_behavior)
df_ezDM_all <- data.table(ssp_recovery_ezDM)
df_reliability_all <- data.table(ssp_recovery_reliability)


#
clean_plot <- theme_bw() +
theme(panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.key = element_rect(fill = 'white'),
      text = element_text(size = 15),
      line = element_line(linewidth = 1),
      axis.ticks = element_line(linewidth = 1))

avg_effects_behavior <- df_behavior_all %>%
  summarize(mean = mean(value),
            .by = c(nRep, SampleSize, nTrials, measure, indicator))

ggplot(data = avg_effects_behavior %>% filter (indicator %in% c("comp", "incomp")),
       aes(x = indicator, y = mean, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ measure) +
  geom_boxplot() +
  labs(x = "Condition", y = "Performance", fill = "Trials in each\nCondition") +
  clean_plot

ggplot(data = df_behavior_all %>% filter(indicator == "diff"),
       aes(x = value, fill = as.factor(nTrials))) +
  facet_grid(SampleSize~ measure) +
  geom_density(alpha = 0.3)

avg_effects_ezDM <- df_ezDM_all %>%
  summarize(mean = mean(value),
            .by = c(nRep, SampleSize, nTrials, measure, indicator))

ggplot(data = avg_effects_ezDM %>% filter (indicator %in% c("comp", "incomp")),
       aes(x = indicator, y = mean, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ measure) +
  geom_boxplot() +
  labs(x = "Condition", y = "Performance", fill = "Trials in each\nCondition") +
  clean_plot

ggplot(data = df_reliability_all,
       aes(x = as.factor(nTrials), y = reliability, fill = as.factor(nTrials))) +
  facet_grid(indicator ~ measure) +
  geom_boxplot() +
  ylim(-1,1) +
  labs(x = "Number of Trials in each Condition", y = "Estimated Reliability",
       fill = "Trials in each\nCondition") +
  clean_plot

ggplot(data = df_recovery_all %>%
         filter(genPar %in% c("b","non_dec","p","sd_0"),
                measure %in% c("RT","PC")),
       aes(x = genPar, y = rec, fill = as.factor(nTrials))) +
  facet_grid(measure ~ indicator) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  labs(x = "Generating DMC Parameter", y = "Recovery",
       fill = "Trials in each\nCondition") +
  clean_plot

ggplot(data = df_recovery_all %>%
         filter(genPar %in% c("b","non_dec","p","sd_0"),
                measure %in% c("v","a","t0")),
       aes(x = genPar, y = rec, fill = as.factor(nTrials))) +
  facet_grid(measure ~ indicator) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  labs(x = "Generating DMC Parameter", y = "Recovery",
       fill = "Trials in each\nCondition") +
  clean_plot

ggplot(data = df_recovery_all %>%
         filter(genPar %in% c("b","non_dec","p","sd_0"),
                measure %in% c("RT","PC")),
       aes(x = genPar, y = rec_corrected, fill = as.factor(nTrials))) +
  facet_grid(measure ~ indicator) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  ylim(-1,1) +
  labs(x = "Generating DMC Parameter", y = "Recovery (Corrected for Reliability)",
       fill = "Trials in each\nCondition") +
  clean_plot

ggplot(data = df_recovery_all %>%
         filter(genPar %in% c("b","non_dec","p","sd_0"),
                measure %in% c("v","a","t0")),
       aes(x = genPar, y = rec_corrected, fill = as.factor(nTrials))) +
  facet_grid(measure ~ indicator) +
  ylim(-1,1) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  labs(x = "Generating DMC Parameter", y = "Recovery (Corrected for Reliability)",
       fill = "Trials in each\nCondition") +
  clean_plot
