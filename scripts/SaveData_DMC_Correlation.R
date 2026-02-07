# start fresh
rm(list = ls())   # clean up work space
graphics.off()  # switch off graphics device

library(here)
library(SimDesign)
library(tidytable)
library(data.table)
library(usethis)
library(ggplot2)

load(here("output","res_DMC_correlation.rds"))
nReplications <- unique(res$REPLICATIONS)
allResults <- SimResults(res, prefix = "DMC_Correlation_Cond", wd = here("output"))

# 1) collect all results ---------------
for (c in 1:length(allResults)) {
  # get the results from one condition
  results_cond <- allResults[[c]]

  # separate condition info from results object
  condition <- results_cond$condition
  results <- results_cond$results

  # collect the recoveries in one data frame
  df_correlations <- do.call(rbind,
                         lapply(1:nReplications,
                                function(ind, res) res[[ind]]$correlations %>%
                                  mutate(nRep = ind),
                                res = results))

  # add condition information
  df_correlations$SampleSize <- as.character(condition$sample_size)
  df_correlations$nTrials <- condition$nTrials
  df_correlations$correlated_par <- condition$correlated_par

  # # collect the recoveries in one data frame
  # df_gen_correlations <- do.call(rbind,
  #                            lapply(1:nReplications,
  #                                   function(ind, res) res[[ind]]$genCorr %>%
  #                                     as.data.frame() %>%
  #                                     mutate(nRep = ind),
  #                                   res = results))
  # colnames(df_gen_correlations) <- c(stringr::str_split(condition$correlated_par, pattern = "-")[[1]],"nRep")
  #
  # # add condition information
  # df_gen_correlations$SampleSize <- condition$sample_size
  # df_gen_correlations$nTrials <- condition$nTrials

  # collect the recoveries in one data frame
  df_behavior <- do.call(rbind,
                         lapply(1:nReplications,
                                function(ind, res) res[[ind]]$behavior %>%
                                  mutate(nRep = ind),
                                res = results))

  # add condition information
  df_behavior$SampleSize <- as.character(condition$sample_size)
  df_behavior$nTrials <- condition$nTrials

  # collect the recoveries in one data frame
  df_ezDM <- do.call(rbind,
                     lapply(1:nReplications,
                            function(ind, res) res[[ind]]$ezDM %>%
                              mutate(nRep = ind),
                            res = results))

  # add condition information
  df_ezDM$SampleSize <- as.character(condition$sample_size)
  df_ezDM$nTrials <- condition$nTrials

  # collect descriptive statistics in one data frame
  df_reliability <- do.call(rbind,
                            lapply(1:nReplications,
                                   function(ind,res) res[[ind]]$reliability %>%
                                     mutate(nRep = ind),
                                   res = results))

  # add condition information
  df_reliability$SampleSize <- as.character(condition$sample_size)
  df_reliability$nTrials <- condition$nTrials

  # write condition recoveries and descriptive statistics into the overall results
  # data frame
  if (c == 1) {
    dmc_correlation_recCorrs <- data.table(df_correlations)
    dmc_correlation_behavior <- data.table(df_behavior)
    dmc_correlation_ezDM <- data.table(df_ezDM)
    dmc_correlation_reliability <- data.table(df_reliability)
    # dmc_correlation_genCorrs <- data.table(df_gen_correlations)
  } else {
    dmc_correlation_recCorrs <- rbind(dmc_correlation_recCorrs,data.table(df_correlations), fill = TRUE)
    dmc_correlation_behavior <- rbind(dmc_correlation_behavior,data.table(df_behavior), fill = TRUE)
    dmc_correlation_ezDM <- rbind(dmc_correlation_ezDM,data.table(df_ezDM), fill = TRUE)
    dmc_correlation_reliability <- rbind(dmc_correlation_reliability,data.table(df_reliability), fill = TRUE)
    # dmc_correlation_genCorrs <- rbind(dmc_correlation_genCorrs,data.table(df_gen_correlations))
  }

  # clean up after each iteration
  rm(results_cond,condition,results,
     df_correlations,df_behavior,df_ezDM,df_gen_correlations,df_reliability)
}
rm(allResults,res)


ggplot(data = dmc_correlation_recCorrs,
       aes(y = correlation, x = emp_corr.muc, color = as.factor(nTrials))) +
  facet_grid(measure ~ indicator) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x")

ggplot(data = dmc_correlation_recCorrs,
       aes(y = correlation, x = emp_corr.A, color = as.factor(nTrials))) +
  facet_grid(measure ~ indicator) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x")

ggplot(data = dmc_correlation_recCorrs,
       aes(y = correlation, x = emp_corr.tau, color = as.factor(nTrials))) +
  facet_grid(measure ~ indicator) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point() +
  geom_smooth(method = "lm", formula = "y ~ x")

# 2) Code Factors ---------------

# Behavioral Results
dmc_correlation_behavior$measure <- factor(dmc_correlation_behavior$measure,
                                        levels = c("RT","PC"))
dmc_correlation_behavior$indicator <- factor(dmc_correlation_behavior$indicator,
                                        levels = c("mean","comp","incomp","diff"))
levels(dmc_correlation_behavior$indicator) <- c("mean","congruent","incongruent","difference")

dmc_correlation_behavior$SampleSize <- factor(dmc_correlation_behavior$SampleSize,
                                          levels = c("200"))
levels(dmc_correlation_behavior$SampleSize) <- c("N = 200")

dmc_correlation_behavior$nTrials <- factor(dmc_correlation_behavior$nTrials,
                                           levels = c("50","100","200"))

dmc_correlation_behavior$correlated_par <- factor(dmc_correlation_behavior$correlated_par,
                                                   levels = c("muc","A","tau","muc-A","muc-tau","A-tau","muc-A-tau"))

# ezDM Results
dmc_correlation_ezDM$measure <- factor(dmc_correlation_ezDM$measure,
                                        levels = c("v","a","t0"))
levels(dmc_correlation_ezDM$measure) <- c("drift","boundary","non_dec")

dmc_correlation_ezDM$indicator <- factor(dmc_correlation_ezDM$indicator,
                                          levels = c("mean","comp","incomp","diff"))
levels(dmc_correlation_ezDM$indicator) <- c("mean","congruent","incongruent","difference")

dmc_correlation_ezDM$SampleSize <- factor(dmc_correlation_ezDM$SampleSize,
                                           levels = c("200"))
levels(dmc_correlation_ezDM$SampleSize) <- c("N = 200")

dmc_correlation_ezDM$nTrials <- factor(dmc_correlation_ezDM$nTrials,
                                        levels = c("50","100","200"))

dmc_correlation_ezDM$correlated_par <- factor(dmc_correlation_ezDM$correlated_par,
                                               levels = c("muc","A","tau","muc-A","muc-tau","A-tau","muc-A-tau"))

# Reliability Results
dmc_correlation_reliability$measure <- factor(dmc_correlation_reliability$measure,
                                    levels = c("RT","PC","v","a","t0"))
levels(dmc_correlation_reliability$measure) <- c("RT","PC","drift","boundary","non_dec")

dmc_correlation_reliability$indicator <- factor(dmc_correlation_reliability$indicator,
                                      levels = c("mean","comp","incomp","diff"))
levels(dmc_correlation_reliability$indicator) <- c("mean","congruent","incongruent","difference")

dmc_correlation_reliability$SampleSize <- factor(dmc_correlation_reliability$SampleSize,
                                       levels = c("200"))
levels(dmc_correlation_reliability$SampleSize) <- c("N = 200")

dmc_correlation_reliability$nTrials <- factor(dmc_correlation_reliability$nTrials,
                                    levels = c("50","100","200"))

dmc_correlation_reliability$correlated_par <- factor(dmc_correlation_reliability$correlated_par,
                                                      levels = c("muc","A","tau","muc-A","muc-tau","A-tau","muc-A-tau"))

# Correlation Results
dmc_correlation_recCorrs$measure <- factor(dmc_correlation_recCorrs$measure,
                                            levels = c("RT","PC","v","a","t0"))
levels(dmc_correlation_recCorrs$measure) <- c("RT","PC","drift","boundary","non_dec")

dmc_correlation_recCorrs$indicator <- factor(dmc_correlation_recCorrs$indicator,
                                              levels = c("mean","comp","incomp","diff"))
levels(dmc_correlation_recCorrs$indicator) <- c("mean","congruent","incongruent","difference")

dmc_correlation_recCorrs$SampleSize <- factor(dmc_correlation_recCorrs$SampleSize,
                                               levels = c("200"))
levels(dmc_correlation_recCorrs$SampleSize) <- c("N = 200")

dmc_correlation_recCorrs$nTrials <- factor(dmc_correlation_recCorrs$nTrials,
                                            levels = c("50","100","200"))

dmc_correlation_recCorrs$correlated_par <- factor(dmc_correlation_recCorrs$correlated_par,
                                                   levels = c("muc","A","tau","muc-A","muc-tau","A-tau","muc-A-tau"))

# 3) Save to Package ---------------
usethis::use_data(dmc_correlation_behavior, overwrite = TRUE, compress = "xz")
usethis::use_data(dmc_correlation_ezDM, overwrite = TRUE, compress = "xz")
usethis::use_data(dmc_correlation_reliability, overwrite = TRUE, compress = "xz")
usethis::use_data(dmc_correlation_recCorrs, overwrite = TRUE, compress = "xz")
