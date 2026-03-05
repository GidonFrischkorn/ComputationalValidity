# start fresh
rm(list = ls())
graphics.off()

library(here)
library(SimDesign)
library(tidytable)
library(data.table)
library(usethis)

load(here("output", "res_M3_correlation.rds"))
nReplications <- unique(res$REPLICATIONS)
allResults <- SimResults(res, prefix = "M3_Correlation_Cond", wd = here("output"))

# 1) collect all results ---------------
for (c in 1:length(allResults)) {
  results_cond <- allResults[[c]]
  condition <- results_cond$condition
  results <- results_cond$results

  # collect correlations
  df_correlations <- do.call(rbind,
                             lapply(1:nReplications,
                                    function(ind, res) res[[ind]]$correlations %>%
                                      mutate(nRep = ind),
                                    res = results))
  df_correlations$SampleSize <- as.character(condition$sample_size)
  df_correlations$nTrials <- condition$nTrials
  df_correlations$correlated_par <- condition$correlated_par

  # collect behavior
  df_behavior <- do.call(rbind,
                         lapply(1:nReplications,
                                function(ind, res) res[[ind]]$behavior %>%
                                  mutate(nRep = ind),
                                res = results))
  df_behavior$SampleSize <- as.character(condition$sample_size)
  df_behavior$nTrials <- condition$nTrials

  # collect reliability
  df_reliability <- do.call(rbind,
                            lapply(1:nReplications,
                                   function(ind, res) res[[ind]]$reliability %>%
                                     mutate(nRep = ind),
                                   res = results))
  df_reliability$SampleSize <- as.character(condition$sample_size)
  df_reliability$nTrials <- condition$nTrials

  if (c == 1) {
    m3_correlation_recCorrs <- data.table(df_correlations)
    m3_correlation_behavior <- data.table(df_behavior)
    m3_correlation_reliability <- data.table(df_reliability)
  } else {
    m3_correlation_recCorrs <- rbind(m3_correlation_recCorrs, data.table(df_correlations), fill = TRUE)
    m3_correlation_behavior <- rbind(m3_correlation_behavior, data.table(df_behavior), fill = TRUE)
    m3_correlation_reliability <- rbind(m3_correlation_reliability, data.table(df_reliability), fill = TRUE)
  }

  rm(results_cond, condition, results, df_correlations, df_behavior, df_reliability)
}
rm(allResults, res)

# 2) Code Factors ---------------

# Behavioral Results
m3_correlation_behavior$measure <- factor(m3_correlation_behavior$measure,
                                           levels = c("proportion", "composite"))

m3_correlation_behavior$indicator <- factor(m3_correlation_behavior$indicator,
                                             levels = c("corr", "other", "distc", "disto", "npl",
                                                        "accuracy", "intrusion_mem", "intrusion_dist",
                                                        "intrusion_total", "npl_rate"))

m3_correlation_behavior$SampleSize <- factor(m3_correlation_behavior$SampleSize,
                                              levels = c("200"))
levels(m3_correlation_behavior$SampleSize) <- c("N = 200")

m3_correlation_behavior$nTrials <- factor(m3_correlation_behavior$nTrials,
                                           levels = c("50", "100", "200"))

# Reliability
m3_correlation_reliability$measure <- factor(m3_correlation_reliability$measure,
                                              levels = c("proportion", "composite"))

m3_correlation_reliability$indicator <- factor(m3_correlation_reliability$indicator,
                                                levels = c("corr", "other", "distc", "disto", "npl",
                                                           "accuracy", "intrusion_mem", "intrusion_dist",
                                                           "intrusion_total", "npl_rate"))

m3_correlation_reliability$SampleSize <- factor(m3_correlation_reliability$SampleSize,
                                                 levels = c("200"))
levels(m3_correlation_reliability$SampleSize) <- c("N = 200")

m3_correlation_reliability$nTrials <- factor(m3_correlation_reliability$nTrials,
                                              levels = c("50", "100", "200"))

# Correlation Results
m3_correlation_recCorrs$measure <- factor(m3_correlation_recCorrs$measure,
                                           levels = c("proportion", "composite"))

m3_correlation_recCorrs$indicator <- factor(m3_correlation_recCorrs$indicator,
                                             levels = c("corr", "other", "distc", "disto", "npl",
                                                        "accuracy", "intrusion_mem", "intrusion_dist",
                                                        "intrusion_total", "npl_rate"))

m3_correlation_recCorrs$SampleSize <- factor(m3_correlation_recCorrs$SampleSize,
                                              levels = c("200"))
levels(m3_correlation_recCorrs$SampleSize) <- c("N = 200")

m3_correlation_recCorrs$nTrials <- factor(m3_correlation_recCorrs$nTrials,
                                           levels = c("50", "100", "200"))

m3_correlation_recCorrs$correlated_par <- factor(m3_correlation_recCorrs$correlated_par,
                                                  levels = c("c", "a", "f", "c-a", "c-f", "a-f", "c-a-f"))

# 3) Save to Package ---------------
usethis::use_data(m3_correlation_behavior, overwrite = TRUE, compress = "xz")
usethis::use_data(m3_correlation_reliability, overwrite = TRUE, compress = "xz")
usethis::use_data(m3_correlation_recCorrs, overwrite = TRUE, compress = "xz")
