# start fresh
rm(list = ls())   # clean up work space
graphics.off()  # switch off graphics device

library(here)
library(SimDesign)
library(tidytable)
library(data.table)
library(usethis)
library(ggplot2)

load(here("output","res_SSP_correlation.rds"))
nReplications <- unique(res$REPLICATIONS)
allResults <- SimDesign::SimResults(res, prefix = "SSP_Correlation_Cond", wd = here("output"))

# 1) collect all results ---------------
for (c in 1:length(allResults)) {
  # get the results from one condition
  results_cond <- allResults[[c]]

  # separate condition info from results object
  condition <- results_cond$condition
  results <- results_cond$results

  # collect the correlations in one data frame
  df_correlations <- do.call(rbind,
                         lapply(1:nReplications,
                                function(ind, res) res[[ind]]$correlations %>%
                                  mutate(nRep = ind),
                                res = results))

  # add condition information
  df_correlations$SampleSize <- as.character(condition$sample_size)
  df_correlations$nTrials <- condition$nTrials
  df_correlations$correlated_par <- condition$correlated_par

  # collect the behavioral results in one data frame
  df_behavior <- do.call(rbind,
                         lapply(1:nReplications,
                                function(ind, res) res[[ind]]$behavior %>%
                                  mutate(nRep = ind),
                                res = results))

  # add condition information
  df_behavior$SampleSize <- as.character(condition$sample_size)
  df_behavior$nTrials <- condition$nTrials
  df_behavior$correlated_par <- condition$correlated_par

  # collect the ezDM results in one data frame
  df_ezDM <- do.call(rbind,
                     lapply(1:nReplications,
                            function(ind, res) res[[ind]]$ezDM %>%
                              mutate(nRep = ind),
                            res = results))

  # add condition information
  df_ezDM$SampleSize <- as.character(condition$sample_size)
  df_ezDM$nTrials <- condition$nTrials
  df_ezDM$correlated_par <- condition$correlated_par

  # collect reliability estimates in one data frame
  df_reliability <- do.call(rbind,
                            lapply(1:nReplications,
                                   function(ind,res) res[[ind]]$reliability %>%
                                     mutate(nRep = ind),
                                   res = results))

  # add condition information
  df_reliability$SampleSize <- as.character(condition$sample_size)
  df_reliability$nTrials <- condition$nTrials
  df_reliability$correlated_par <- condition$correlated_par

  # write condition results into the overall results data frame
  if (c == 1) {
    ssp_correlation_recCorrs <- data.table(df_correlations)
    ssp_correlation_behavior <- data.table(df_behavior)
    ssp_correlation_ezDM <- data.table(df_ezDM)
    ssp_correlation_reliability <- data.table(df_reliability)
  } else {
    ssp_correlation_recCorrs <- rbind(ssp_correlation_recCorrs, data.table(df_correlations), fill = TRUE)
    ssp_correlation_behavior <- rbind(ssp_correlation_behavior, data.table(df_behavior), fill = TRUE)
    ssp_correlation_ezDM <- rbind(ssp_correlation_ezDM, data.table(df_ezDM), fill = TRUE)
    ssp_correlation_reliability <- rbind(ssp_correlation_reliability, data.table(df_reliability), fill = TRUE)
  }

  # clean up after each iteration
  rm(results_cond, condition, results,
     df_correlations, df_behavior, df_ezDM, df_reliability)
}
rm(allResults, res)


# 2) Code Factors ---------------

# Behavioral Results
ssp_correlation_behavior$measure <- factor(ssp_correlation_behavior$measure,
                                        levels = c("RT","PC"))
ssp_correlation_behavior$indicator <- factor(ssp_correlation_behavior$indicator,
                                        levels = c("mean","comp","incomp","diff"))
levels(ssp_correlation_behavior$indicator) <- c("mean","congruent","incongruent","difference")

ssp_correlation_behavior$SampleSize <- factor(ssp_correlation_behavior$SampleSize,
                                          levels = c("200"))
levels(ssp_correlation_behavior$SampleSize) <- c("N = 200")

ssp_correlation_behavior$nTrials <- factor(ssp_correlation_behavior$nTrials,
                                           levels = c("50","100","200"))

ssp_correlation_behavior$correlated_par <- factor(ssp_correlation_behavior$correlated_par,
                                                   levels = c("p","sd_0","p-sd_0"))

# ezDM Results
ssp_correlation_ezDM$measure <- factor(ssp_correlation_ezDM$measure,
                                        levels = c("v","a","t0"))
levels(ssp_correlation_ezDM$measure) <- c("drift","boundary","non_dec")

ssp_correlation_ezDM$indicator <- factor(ssp_correlation_ezDM$indicator,
                                          levels = c("mean","comp","incomp","diff"))
levels(ssp_correlation_ezDM$indicator) <- c("mean","congruent","incongruent","difference")

ssp_correlation_ezDM$SampleSize <- factor(ssp_correlation_ezDM$SampleSize,
                                           levels = c("200"))
levels(ssp_correlation_ezDM$SampleSize) <- c("N = 200")

ssp_correlation_ezDM$nTrials <- factor(ssp_correlation_ezDM$nTrials,
                                        levels = c("50","100","200"))

ssp_correlation_ezDM$correlated_par <- factor(ssp_correlation_ezDM$correlated_par,
                                               levels = c("p","sd_0","p-sd_0"))

# Reliability Results
ssp_correlation_reliability$measure <- factor(ssp_correlation_reliability$measure,
                                    levels = c("RT","PC","v","a","t0"))
levels(ssp_correlation_reliability$measure) <- c("RT","PC","drift","boundary","non_dec")

ssp_correlation_reliability$indicator <- factor(ssp_correlation_reliability$indicator,
                                      levels = c("mean","comp","incomp","diff"))
levels(ssp_correlation_reliability$indicator) <- c("mean","congruent","incongruent","difference")

ssp_correlation_reliability$SampleSize <- factor(ssp_correlation_reliability$SampleSize,
                                       levels = c("200"))
levels(ssp_correlation_reliability$SampleSize) <- c("N = 200")

ssp_correlation_reliability$nTrials <- factor(ssp_correlation_reliability$nTrials,
                                    levels = c("50","100","200"))

ssp_correlation_reliability$correlated_par <- factor(ssp_correlation_reliability$correlated_par,
                                                      levels = c("p","sd_0","p-sd_0"))

# Correlation Results
ssp_correlation_recCorrs$measure <- factor(ssp_correlation_recCorrs$measure,
                                            levels = c("RT","PC","v","a","t0"))
levels(ssp_correlation_recCorrs$measure) <- c("RT","PC","drift","boundary","non_dec")

ssp_correlation_recCorrs$indicator <- factor(ssp_correlation_recCorrs$indicator,
                                              levels = c("mean","comp","incomp","diff"))
levels(ssp_correlation_recCorrs$indicator) <- c("mean","congruent","incongruent","difference")

ssp_correlation_recCorrs$SampleSize <- factor(ssp_correlation_recCorrs$SampleSize,
                                               levels = c("200"))
levels(ssp_correlation_recCorrs$SampleSize) <- c("N = 200")

ssp_correlation_recCorrs$nTrials <- factor(ssp_correlation_recCorrs$nTrials,
                                            levels = c("50","100","200"))

ssp_correlation_recCorrs$correlated_par <- factor(ssp_correlation_recCorrs$correlated_par,
                                                   levels = c("p","sd_0","p-sd_0"))

# 3) Save to Package ---------------
usethis::use_data(ssp_correlation_behavior, overwrite = TRUE, compress = "xz")
usethis::use_data(ssp_correlation_ezDM, overwrite = TRUE, compress = "xz")
usethis::use_data(ssp_correlation_reliability, overwrite = TRUE, compress = "xz")
usethis::use_data(ssp_correlation_recCorrs, overwrite = TRUE, compress = "xz")
