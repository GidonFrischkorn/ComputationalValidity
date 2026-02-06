# start fresh
rm(list = ls())   # clean up work space
graphics.off()  # switch off graphics device

library(here)
library(SimDesign)
library(tidytable)
library(data.table)
library(usethis)

load(here("output","res_DMC_recovery.rds"))
nReplications <- unique(res$REPLICATIONS)
allResults <- SimResults(res, prefix = "DMC_Recovery_Cond", wd = here("output"))

# 1) collect all results ---------------
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
    dmc_recovery_behavior <- data.table(df_behavior)
    dmc_recovery_ezDM <- data.table(df_ezDM)
    dmc_recovery_parRecovery <- data.table(df_recovery)
    dmc_recovery_reliability <- data.table(df_reliability)
  } else {
    dmc_recovery_behavior <- rbind(dmc_recovery_behavior,data.table(df_behavior))
    dmc_recovery_ezDM <- rbind(dmc_recovery_ezDM,data.table(df_ezDM))
    dmc_recovery_parRecovery <- rbind(dmc_recovery_parRecovery,data.table(df_recovery))
    dmc_recovery_reliability <- rbind(dmc_recovery_reliability,data.table(df_reliability))
  }

  # clean up after each iteration
  rm(results_cond,condition,results,
     df_behavior,df_ezDM,df_recovery,df_reliability)
}
rm(allResults,res)

# 2) Code Factors ---------------

# Behavioral Results
dmc_recovery_behavior$measure <- factor(dmc_recovery_behavior$measure,
                                        levels = c("RT","PC"))
dmc_recovery_behavior$indicator <- factor(dmc_recovery_behavior$indicator,
                                        levels = c("mean","comp","incomp","diff"))
levels(dmc_recovery_behavior$indicator) <- c("mean","congruent","incongruent","difference")

dmc_recovery_behavior$SampleSize <- factor(dmc_recovery_behavior$SampleSize,
                                          levels = c("25","50","100"))
levels(dmc_recovery_behavior$SampleSize) <- c("N = 25","N = 50","N = 100")

dmc_recovery_behavior$nTrials <- factor(dmc_recovery_behavior$nTrials,
                                           levels = c("50","100","200"))

# ezDM Results
dmc_recovery_ezDM$measure <- factor(dmc_recovery_ezDM$measure,
                                        levels = c("v","a","t0"))
levels(dmc_recovery_ezDM$measure) <- c("drift","boundary","non_dec")

dmc_recovery_ezDM$indicator <- factor(dmc_recovery_ezDM$indicator,
                                          levels = c("mean","comp","incomp","diff"))
levels(dmc_recovery_ezDM$indicator) <- c("mean","congruent","incongruent","difference")

dmc_recovery_ezDM$SampleSize <- factor(dmc_recovery_ezDM$SampleSize,
                                           levels = c("25","50","100"))
levels(dmc_recovery_ezDM$SampleSize) <- c("N = 25","N = 50","N = 100")

dmc_recovery_ezDM$nTrials <- factor(dmc_recovery_ezDM$nTrials,
                                        levels = c("50","100","200"))

# Reliability of performance indicators
dmc_recovery_reliability$measure <- factor(dmc_recovery_reliability$measure,
                                    levels = c("PC","RT","v","a","t0"))
levels(dmc_recovery_reliability$measure) <- c("PC","RT","drift","boundary","non_dec")

dmc_recovery_reliability$indicator <- factor(dmc_recovery_reliability$indicator,
                                      levels = c("mean","comp","incomp","diff"))
levels(dmc_recovery_reliability$indicator) <- c("mean","congruent","incongruent","difference")

dmc_recovery_reliability$SampleSize <- factor(dmc_recovery_reliability$SampleSize,
                                       levels = c("25","50","100"))
levels(dmc_recovery_reliability$SampleSize) <- c("N = 25","N = 50","N = 100")

dmc_recovery_reliability$nTrials <- factor(dmc_recovery_reliability$nTrials,
                                    levels = c("50","100","200"))

# Recovery of DMC parameters
dmc_recovery_parRecovery$genPar <- factor(dmc_recovery_parRecovery$genPar,
                                          levels = c("A","tau","muc","b","non_dec","sd_non_dec","alpha"))

dmc_recovery_parRecovery$measure <- factor(dmc_recovery_parRecovery$measure,
                                    levels = c("RT","PC","v","a","t0"))
levels(dmc_recovery_parRecovery$measure) <- c("RT","PC","drift","boundary","non_dec")

dmc_recovery_parRecovery$indicator <- factor(dmc_recovery_parRecovery$indicator,
                                      levels = c("mean","comp","incomp","diff"))
levels(dmc_recovery_parRecovery$indicator) <- c("mean","congruent","incongruent","difference")

dmc_recovery_parRecovery$SampleSize <- factor(dmc_recovery_parRecovery$SampleSize,
                                       levels = c("25","50","100"))
levels(dmc_recovery_parRecovery$SampleSize) <- c("N = 25","N = 50","N = 100")

dmc_recovery_parRecovery$nTrials <- factor(dmc_recovery_parRecovery$nTrials,
                                    levels = c("50","100","200"))

# 3) Save to Package ---------------
usethis::use_data(dmc_recovery_behavior, overwrite = TRUE, compress = "xz")
usethis::use_data(dmc_recovery_ezDM, overwrite = TRUE, compress = "xz")
usethis::use_data(dmc_recovery_reliability, overwrite = TRUE, compress = "xz")
usethis::use_data(dmc_recovery_parRecovery, overwrite = TRUE, compress = "xz")
