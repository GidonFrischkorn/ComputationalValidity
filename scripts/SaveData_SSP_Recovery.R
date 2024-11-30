load(here("output","res_SSP_recovery.rds"))
nReplications <- unique(res$REPLICATIONS)
allResults <- SimResults(res,prefix = "SSP_Recovery_Cond")

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
    ssp_recovery_behavior <- data.table(df_behavior)
    ssp_recovery_ezDM <- data.table(df_ezDM)
    ssp_recovery_parRecovery <- data.table(df_recovery)
    ssp_recovery_reliability <- data.table(df_reliability)
  } else {
    ssp_recovery_behavior <- rbind(ssp_recovery_behavior,data.table(df_behavior))
    ssp_recovery_ezDM <- rbind(ssp_recovery_ezDM,data.table(df_ezDM))
    ssp_recovery_parRecovery <- rbind(ssp_recovery_parRecovery,data.table(df_recovery))
    ssp_recovery_reliability <- rbind(ssp_recovery_reliability,data.table(df_reliability))
  }

  # clean up after each iteration
  rm(results_cond,condition,results,
     df_behavior,df_ezDM,df_recovery,df_reliability)
}
rm(allResults,res)

# 2) Code Factors ---------------

# Behavioral Results
ssp_recovery_behavior$measure <- factor(ssp_recovery_behavior$measure,
                                        levels = c("RT","PC"))
ssp_recovery_behavior$indicator <- factor(ssp_recovery_behavior$indicator,
                                        levels = c("mean","comp","incomp","diff"))
levels(ssp_recovery_behavior$indicator) <- c("mean","congruent","incongruent","difference")

ssp_recovery_behavior$SampleSize <- factor(ssp_recovery_behavior$SampleSize,
                                          levels = c("25","50","100"))
levels(ssp_recovery_behavior$SampleSize) <- c("N = 25","N = 50","N = 100")

ssp_recovery_behavior$nTrials <- factor(ssp_recovery_behavior$nTrials,
                                           levels = c("50","100","200"))

# ezDM Results
ssp_recovery_ezDM$measure <- factor(ssp_recovery_ezDM$measure,
                                        levels = c("v","a","t0"))
levels(ssp_recovery_ezDM$measure) <- c("drift","boundary","non_dec")

ssp_recovery_ezDM$indicator <- factor(ssp_recovery_ezDM$indicator,
                                          levels = c("mean","comp","incomp","diff"))
levels(ssp_recovery_ezDM$indicator) <- c("mean","congruent","incongruent","difference")

ssp_recovery_ezDM$SampleSize <- factor(ssp_recovery_ezDM$SampleSize,
                                           levels = c("25","50","100"))
levels(ssp_recovery_ezDM$SampleSize) <- c("N = 25","N = 50","N = 100")

ssp_recovery_ezDM$nTrials <- factor(ssp_recovery_ezDM$nTrials,
                                        levels = c("50","100","200"))


# Recovery of DMC parameters
ssp_recovery_parRecovery$genPar <- factor(ssp_recovery_parRecovery$genPar,
                                          levels = c("sd_0","p","b","non_dec","sd_non_dec"))


ssp_recovery_parRecovery$measure <- factor(ssp_recovery_parRecovery$measure,
                                    levels = c("RT","PC","v","a","t0"))
levels(ssp_recovery_parRecovery$measure) <- c("RT","PC","drift","boundary","non_dec")

ssp_recovery_parRecovery$indicator <- factor(ssp_recovery_parRecovery$indicator,
                                      levels = c("mean","comp","incomp","diff"))
levels(ssp_recovery_parRecovery$indicator) <- c("mean","congruent","incongruent","difference")

ssp_recovery_parRecovery$SampleSize <- factor(ssp_recovery_parRecovery$SampleSize,
                                       levels = c("25","50","100"))
levels(ssp_recovery_parRecovery$SampleSize) <- c("N = 25","N = 50","N = 100")

ssp_recovery_parRecovery$nTrials <- factor(ssp_recovery_parRecovery$nTrials,
                                    levels = c("50","100","200"))

# 3) Save to Package ---------------
use_data(ssp_recovery_behavior, overwrite = TRUE, compress = "xz")
use_data(ssp_recovery_ezDM, overwrite = TRUE, compress = "xz")
use_data(ssp_recovery_reliability, overwrite = TRUE, compress = "xz")
use_data(ssp_recovery_parRecovery, overwrite = TRUE, compress = "xz")
