# start fresh
rm(list = ls())
graphics.off()

library(here)
library(SimDesign)
library(tidytable)
library(data.table)
library(usethis)

load(here("output", "res_M3_recovery.rds"))
nReplications <- unique(res$REPLICATIONS)
allResults <- SimResults(res, prefix = "M3_Recovery_Cond", wd = here("output"))

# 1) collect all results ---------------
for (c in 1:length(allResults)) {
  results_cond <- allResults[[c]]
  condition <- results_cond$condition
  results <- results_cond$results

  # collect recovery
  df_recovery <- do.call(rbind,
                         lapply(1:nReplications,
                                function(ind, res) res[[ind]]$recovery %>%
                                  mutate(nRep = ind),
                                res = results))
  df_recovery$SampleSize <- condition$sample_size
  df_recovery$nTrials <- condition$nTrials

  # collect behavior
  df_behavior <- do.call(rbind,
                         lapply(1:nReplications,
                                function(ind, res) res[[ind]]$behavior %>%
                                  mutate(nRep = ind),
                                res = results))
  df_behavior$SampleSize <- condition$sample_size
  df_behavior$nTrials <- condition$nTrials

  # collect reliability
  df_reliability <- do.call(rbind,
                            lapply(1:nReplications,
                                   function(ind, res) res[[ind]]$reliability %>%
                                     mutate(nRep = ind),
                                   res = results))
  df_reliability$SampleSize <- condition$sample_size
  df_reliability$nTrials <- condition$nTrials

  if (c == 1) {
    m3_recovery_behavior <- data.table(df_behavior)
    m3_recovery_parRecovery <- data.table(df_recovery)
    m3_recovery_reliability <- data.table(df_reliability)
  } else {
    m3_recovery_behavior <- rbind(m3_recovery_behavior, data.table(df_behavior))
    m3_recovery_parRecovery <- rbind(m3_recovery_parRecovery, data.table(df_recovery))
    m3_recovery_reliability <- rbind(m3_recovery_reliability, data.table(df_reliability))
  }

  rm(results_cond, condition, results, df_behavior, df_recovery, df_reliability)
}
rm(allResults, res)

# 2) Code Factors ---------------

# Behavioral Results
m3_recovery_behavior$measure <- factor(m3_recovery_behavior$measure,
                                        levels = c("proportion", "composite"))

m3_recovery_behavior$indicator <- factor(m3_recovery_behavior$indicator,
                                          levels = c("corr", "other", "distc", "disto", "npl",
                                                     "accuracy", "intrusion_mem", "intrusion_dist",
                                                     "intrusion_total", "npl_rate"))

m3_recovery_behavior$SampleSize <- factor(m3_recovery_behavior$SampleSize,
                                           levels = c("25", "50", "100"))
levels(m3_recovery_behavior$SampleSize) <- c("N = 25", "N = 50", "N = 100")

m3_recovery_behavior$nTrials <- factor(m3_recovery_behavior$nTrials,
                                        levels = c("50", "100", "200"))

# Reliability
m3_recovery_reliability$measure <- factor(m3_recovery_reliability$measure,
                                           levels = c("proportion", "composite"))

m3_recovery_reliability$indicator <- factor(m3_recovery_reliability$indicator,
                                             levels = c("corr", "other", "distc", "disto", "npl",
                                                        "accuracy", "intrusion_mem", "intrusion_dist",
                                                        "intrusion_total", "npl_rate"))

m3_recovery_reliability$SampleSize <- factor(m3_recovery_reliability$SampleSize,
                                              levels = c("25", "50", "100"))
levels(m3_recovery_reliability$SampleSize) <- c("N = 25", "N = 50", "N = 100")

m3_recovery_reliability$nTrials <- factor(m3_recovery_reliability$nTrials,
                                           levels = c("50", "100", "200"))

# Parameter Recovery
m3_recovery_parRecovery$genPar <- factor(m3_recovery_parRecovery$genPar,
                                          levels = c("c", "a", "f"))

m3_recovery_parRecovery$measure <- factor(m3_recovery_parRecovery$measure,
                                           levels = c("proportion", "composite"))

m3_recovery_parRecovery$indicator <- factor(m3_recovery_parRecovery$indicator,
                                             levels = c("corr", "other", "distc", "disto", "npl",
                                                        "accuracy", "intrusion_mem", "intrusion_dist",
                                                        "intrusion_total", "npl_rate"))

m3_recovery_parRecovery$SampleSize <- factor(m3_recovery_parRecovery$SampleSize,
                                              levels = c("25", "50", "100"))
levels(m3_recovery_parRecovery$SampleSize) <- c("N = 25", "N = 50", "N = 100")

m3_recovery_parRecovery$nTrials <- factor(m3_recovery_parRecovery$nTrials,
                                           levels = c("50", "100", "200"))

# 3) Save to Package ---------------
usethis::use_data(m3_recovery_behavior, overwrite = TRUE, compress = "xz")
usethis::use_data(m3_recovery_reliability, overwrite = TRUE, compress = "xz")
usethis::use_data(m3_recovery_parRecovery, overwrite = TRUE, compress = "xz")
