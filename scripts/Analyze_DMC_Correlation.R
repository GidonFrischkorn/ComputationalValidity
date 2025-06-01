# start fresh
rm(list = ls())   # clean up work space
graphics.off()  # switch off graphics device

# use relative paths to load & save data
pacman::p_load(SimDesign,tidytable,data.table, ggplot2)
nReplications <- 10

## Collect Results -------------------------------------------------------------
load(here("output","res_DMC_correlation.rds"))
allResults <- SimResults(res)

# loop through simulation conditions to collect all results
for (c in 1:length(allResults)) {
  # get the results from one condition
  results_cond <- allResults[[c]]

  # separate condition info from results object
  condition <- results_cond$condition
  results <- results_cond$results

  # collect the recoveries in one data frame
  df_recovery <- do.call(rbind,
                         lapply(1:nReplications,
                                function(ind, res) res[[ind]]$recovery %>%
                                  mutate(nRep = ind),
                                res = results))

  # add condition information
  df_recovery$SampleSize <- condition$sample_size
  df_recovery$nTrials <- condition$nTrials

  # collect the recoveries in one data frame
  df_behavior <- do.call(rbind,
                         lapply(1:nReplications,
                                function(ind, res) res[[ind]]$behavior %>%
                                  mutate(nRep = ind),
                                res = results))

  # add condition information
  df_behavior$SampleSize <- condition$sample_size
  df_behavior$nTrials <- condition$nTrials

  # collect the recoveries in one data frame
  df_ezDM <- do.call(rbind,
                     lapply(1:nReplications,
                            function(ind, res) res[[ind]]$ezDM %>%
                              mutate(nRep = ind),
                            res = results))

  # add condition information
  df_ezDM$SampleSize <- condition$sample_size
  df_ezDM$nTrials <- condition$nTrials

  # collect descriptive statistics in one data frame
  df_reliability <- do.call(rbind,
                            lapply(1:nReplications,
                                   function(ind,res) res[[ind]]$reliability %>%
                                     mutate(nRep = ind),
                                   res = results))

  # add condition information
  df_reliability$SampleSize <- condition$sample_size
  df_reliability$nTrials <- condition$nTrials

  # write condition recoveries and descriptive statistics into the overall results
  # data frame
  if (c == 1) {
    df_behavior_all <- data.table(df_behavior)
    df_ezDM_all <- data.table(df_ezDM)
    df_recovery_all <- data.table(df_recovery)
    df_reliability_all <- data.table(df_reliability)
  } else {
    df_behavior_all <- rbind(df_behavior_all,data.table(df_behavior))
    df_ezDM_all <- rbind(df_ezDM_all,data.table(df_ezDM))
    df_recovery_all <- rbind(df_recovery_all,data.table(df_recovery))
    df_reliability_all <- rbind(df_reliability_all,data.table(df_reliability))
  }

  # clean up after each iteration
  rm(results_cond,condition,results,
     df_behavior,df_ezDM,df_recovery,df_reliability)
}


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
            sd = sd(value),
            .by = c(nRep, SampleSize, nTrials, measure, indicator))

ggplot(data = avg_effects_behavior %>% filter (indicator %in% c("comp", "incomp")),
       aes(x = indicator, y = mean, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ measure) +
  geom_boxplot() +
  labs(x = "Condition", y = "Performance", fill = "Trials in each\nCondition") +
  clean_plot

ggplot(data = avg_effects_behavior %>% filter (indicator %in% c("diff")),
       aes(x = indicator, y = sd, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ measure) +
  geom_boxplot() +
  labs(x = "Condition", y = "Performance", fill = "Trials in each\nCondition") +
  clean_plot

ggplot(data = df_behavior_dmc %>% filter(indicator == "diff"),
       aes(x = value, fill = as.factor(nTrials))) +
  facet_grid(SampleSize~ measure) +
  geom_density(alpha = 0.3)

avg_effects_ezDM <- df_ezDM_all %>%
  summarize(mean = mean(value),
            sd = sd(value),
            .by = c(nRep, SampleSize, nTrials, measure, indicator))

ggplot(data = avg_effects_ezDM %>% filter (indicator %in% c("comp", "incomp")),
       aes(x = indicator, y = mean, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ measure) +
  geom_boxplot() +
  labs(x = "Condition", y = "Performance", fill = "Trials in each\nCondition") +
  clean_plot

ggplot(data = avg_effects_ezDM %>% filter(indicator %in% c("diff")),
       aes(x = indicator, y = sd, fill = as.factor(nTrials))) +
  facet_grid(SampleSize ~ measure) +
  geom_boxplot() +
  labs(x = "Condition", y = "Performance", fill = "Trials in each\nCondition") +
  clean_plot

avg_effects_all <- rbind(avg_effects_behavior,avg_effects_ezDM)

df_reliability_all <- df_reliability_all %>%
  left_join(avg_effects_all)

df_recovery_all <- df_recovery_all %>%
  left_join(avg_effects_all)

ggplot(data = df_reliability_all,
       aes(x = as.factor(nTrials), y = reliability, fill = as.factor(SampleSize))) +
  facet_grid(indicator ~ measure) +
  geom_boxplot() +
  labs(x = "Number of Trials in each Condition", y = "Estimated Reliability",
       fill = "Sample Size") +
  clean_plot

ggplot(data = df_reliability_all %>% filter(indicator == "diff"),
       aes(x = mean, y = reliability, color = as.factor(SampleSize))) +
  facet_grid(nTrials ~ measure, scales = "free") +
  ylim(-1,1) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(x = "Average Conflict Effect", y = "Estimated Reliability",
       fill = "Sample Size") +
  clean_plot

ggplot(data = df_reliability_all %>% filter(indicator == "diff"),
       aes(x = sd, y = reliability, color = as.factor(SampleSize))) +
  facet_grid(nTrials ~ measure, scales = "free") +
  ylim(-1,1) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  labs(x = "Standard Deviation of Conflict Effect", y = "Estimated Reliability",
       fill = "Sample Size") +
  clean_plot



ggplot(data = df_recovery_all %>%
         filter(genPar %in% c("A","b","muc","non_dec","tau"),
                measure %in% c("RT","PC")),
       aes(x = genPar, y = rec, fill = as.factor(nTrials))) +
  facet_grid(measure ~ indicator) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  labs(x = "Generating DMC Parameter", y = "Recovery",
       fill = "Sample Size") +
  coord_cartesian(ylim = c(-1,1)) +
  clean_plot

ggplot(data = df_recovery_all %>%
         filter(genPar %in% c("A","b","muc","non_dec","tau"),
                measure %in% c("RT","PC"),
                indicator %in% c("diff")),
       aes(x = mean, y = rec, color = as.factor(SampleSize))) +
  facet_grid(measure ~ genPar) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  labs(x = "Mean Conflict Effect in the Sample", y = "Recovery",
       fill = "Sample Size") +
  coord_cartesian(ylim = c(-1,1)) +
  clean_plot

ggplot(data = df_recovery_all %>%
         filter(genPar %in% c("A","b","muc","non_dec","tau"),
                measure %in% c("RT","PC"),
                indicator %in% c("diff")),
       aes(x = sd, y = rec, color = as.factor(SampleSize))) +
  facet_grid(measure ~ genPar, scale = "free") +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_point(alpha = 0.1) +
  geom_smooth(method = "lm") +
  labs(x = "SD of Conflict Effect in the Sample", y = "Recovery",
       fill = "Sample Size") +
  coord_cartesian(ylim = c(-1,1)) +
  clean_plot

ggplot(data = df_recovery_all %>%
         filter(genPar %in% c("A","b","muc","non_dec","tau"),
                measure %in% c("v","a","t0")),
       aes(x = genPar, y = rec, fill = as.factor(SampleSize))) +
  facet_grid(measure ~ indicator) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  coord_cartesian(ylim = c(-1,1)) +
  labs(x = "Generating DMC Parameter", y = "Recovery",
       fill = "Sample Size") +
  clean_plot

ggplot(data = df_recovery_all %>%
         filter(genPar %in% c("A","b","muc","non_dec","tau"),
                measure %in% c("RT","PC"),
                SampleSize == 100),
       aes(x = genPar, y = rec_corrected, fill = as.factor(nTrials))) +
  facet_grid(measure ~ indicator) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  ylim(-1,1) +
  coord_cartesian(ylim = c(-1,1)) +
  labs(x = "Generating DMC Parameter", y = "Recovery (Corrected for Reliability)",
       fill = "Sample Size") +
  clean_plot

ggplot(data = df_recovery_all %>%
         filter(genPar %in% c("A","b","muc","non_dec","tau"),
                measure %in% c("v","a","t0"),
                SampleSize == 100),
       aes(x = genPar, y = rec_corrected, fill = as.factor(nTrials))) +
  facet_grid(measure ~ indicator) +
  geom_hline(yintercept = 0, color = "darkred", linewidth = 1) +
  geom_hline(yintercept = 0.3, color = "darkred", linetype = "dotted") +
  geom_hline(yintercept = -0.3, color = "darkred", linetype = "dotted") +
  geom_boxplot() +
  ylim(-1,1) +
  coord_cartesian(ylim = c(-1,1)) +
  labs(x = "Generating DMC Parameter", y = "Recovery (Corrected for Reliability)",
       fill = "Sample Size") +
  clean_plot
