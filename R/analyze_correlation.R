#' @title Analyse Data for
#'
#' @param n_sub A single numeric value specifying for how many subjects data should be simulated.
#' @param n_trials A single numeric value specifying how many trials per condition should be simulated.
#' @param par_limits A data.frame containing two variables: min & max with 7 values
#'   for each parameter of the DMC. You have to either include a third variable `par_name` labeling
#'   which row contains information for which parameters or name the rows of the data frame.
#'   Parameter names of the dmc parameters can be obtained via: `dRiftDM::dmc_dm()$free_prms`
#'
#' @export
analyze_correlation <- function(dat) {
  # Add AUC composite parameter to generating parameters for both tasks
  # AUC = 2 * A * tau (area under Gamma activation curve)
  if ("A" %in% colnames(dat$sub_parms) && "tau" %in% colnames(dat$sub_parms)) {
    dat$sub_parms$AUC <- 2 * dat$sub_parms$A * dat$sub_parms$tau
    
    # Also add AUC to empirical correlation if A or tau were correlated
    if (!is.null(dat$empirical_correlation)) {
      if ("A" %in% names(dat$empirical_correlation) || "tau" %in% names(dat$empirical_correlation)) {
        # Calculate AUC for both tasks from the parameters
        # Extract task indicators (assuming ID column exists and task column distinguishes tasks)
        task1_indices <- which(dat$sub_parms$task == 1)
        task2_indices <- which(dat$sub_parms$task == 2)
        
        if (length(task1_indices) > 0 && length(task2_indices) > 0) {
          auc_task1 <- dat$sub_parms$AUC[task1_indices]
          auc_task2 <- dat$sub_parms$AUC[task2_indices]
          dat$empirical_correlation <- c(dat$empirical_correlation, AUC = cor(auc_task1, auc_task2))
        }
      }
    }
  }
  
  # calculate descriptive performance indicators
  desc_performance <- get_descriptives(data = dat$sim_data)

  # ezDM parameters
  ezDM_performance <- get_ezDM(data = dat$sim_data)
  
  # Check for failed estimates
  n_failed <- sum(is.na(ezDM_performance$value))
  if (n_failed > 0) {
    warning(sprintf("EZ-DM estimation failed for %d out of %d observations (%.1f%%)",
                    n_failed, nrow(ezDM_performance), 100 * n_failed / nrow(ezDM_performance)))
  }

  # combine descriptive and ezDM performance indicators
  all_performance <- rbind(desc_performance, ezDM_performance)

  # reliability estimates
  reliability <- get_reliability(data = dat$sim_data)

  # recovery of DMC parameters
  correlations <- get_correlations(all_performance, reliabilites = reliability)
  correlations <- cbind(correlations, data.frame(as.list(dat$empirical_correlation)))

  return(list(correlations = correlations,
              behavior = desc_performance,
              ezDM = ezDM_performance,
              reliability = reliability,
              genPars = dat$sub_parms,
              genCorr = dat$empirical_correlation,
              n_failed_estimates = n_failed))

}
