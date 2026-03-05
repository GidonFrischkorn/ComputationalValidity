# ==============================================================================
# DMC RECOVERY SIMULATION DATASETS
# ==============================================================================

#' Behavioral Results from the DMC Recovery Simulation
#'
#' Contains descriptive behavioral statistics (RT and accuracy) from a parameter
#' recovery simulation using the Diffusion Model for Conflict (DMC). Data were
#' generated with known DMC parameters, then behavioral indicators were computed
#' to assess how well parameters can be recovered from behavioral measures.
#'
#' @format ## `dmc_recovery_behavior`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Type of behavioral measure: "RT" (response time) or "PC" (proportion correct)}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{value}{The observed value of the behavioral measure}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_DMC_Recovery.R` from simulation results
"dmc_recovery_behavior"

#' EZ-Diffusion Model Results from the DMC Recovery Simulation
#'
#' Contains EZ-diffusion model parameter estimates (drift rate, boundary, non-decision time)
#' computed from simulated DMC data. EZ-DM is a simplified diffusion model that provides
#' quick parameter estimates from aggregate RT and accuracy data.
#'
#' @format ## `dmc_recovery_ezDM`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{EZ-DM parameter: "drift" (v), "boundary" (a), or "non_dec" (t0)}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{value}{The estimated EZ-DM parameter value}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_DMC_Recovery.R` from simulation results
"dmc_recovery_ezDM"


#' Parameter Recovery Results from the DMC Recovery Simulation
#'
#' Contains correlations between generating DMC parameters and recovered parameters
#' (from behavioral indicators and EZ-DM estimates). Shows how well each DMC parameter
#' can be recovered from different behavioral measures.
#'
#' @format ## `dmc_recovery_parRecovery`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{genPar}{Generating DMC parameter: "A" (amplitude), "tau" (temporal dynamics),
#'         "muc" (controlled drift), "b" (boundary), "non_dec" (non-decision time), etc.}
#'   \item{measure}{Indicator type: "RT", "PC", "drift", "boundary", or "non_dec"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{rec}{Recovery correlation (correlation between generating and recovered parameter)}
#'   \item{rec_corrected}{Recovery correlation corrected for indicator reliability}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_DMC_Recovery.R` from simulation results
"dmc_recovery_parRecovery"

#' Reliability Results from the DMC Recovery Simulation
#'
#' Contains split-half reliability estimates for behavioral indicators and EZ-DM parameters.
#' Reliability is computed using odd-even trial splits within each replication.
#'
#' @format ## `dmc_recovery_reliability`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Measure type: "RT", "PC", "drift", "boundary", or "non_dec"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{reliability}{Split-half reliability estimate (Spearman-Brown corrected)}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_DMC_Recovery.R` from simulation results
"dmc_recovery_reliability"

# ==============================================================================
# SSP RECOVERY SIMULATION DATASETS
# ==============================================================================

#' Behavioral Results from the SSP Recovery Simulation
#'
#' Contains descriptive behavioral statistics from a parameter recovery simulation
#' using the Shrinking Spotlight (SSP) model. Parallel to DMC recovery but with
#' SSP architecture (perceptual input, spatial shrinking, etc.).
#'
#' @format ## `ssp_recovery_behavior`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Type of behavioral measure: "RT" or "PC"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{value}{The observed value of the behavioral measure}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_SSP_Recovery.R` from simulation results
"ssp_recovery_behavior"

#' EZ-Diffusion Model Results from the SSP Recovery Simulation
#'
#' Contains EZ-DM parameter estimates computed from SSP-generated data.
#' Shows how well simplified diffusion model captures SSP architecture.
#'
#' @format ## `ssp_recovery_ezDM`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{EZ-DM parameter: "drift", "boundary", or "non_dec"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{value}{The estimated EZ-DM parameter value}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_SSP_Recovery.R` from simulation results
"ssp_recovery_ezDM"

#' Parameter Recovery Results from the SSP Recovery Simulation
#'
#' Contains correlations between generating SSP parameters and recovered parameters.
#' SSP parameters include: perceptual input (p), boundary (b), initial noise (sd_0),
#' and non-decision time (non_dec).
#'
#' @format ## `ssp_recovery_parRecovery`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{genPar}{Generating SSP parameter: "p", "b", "sd_0", "non_dec", etc.}
#'   \item{measure}{Indicator type: "RT", "PC", "drift", "boundary", or "non_dec"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{rec}{Recovery correlation}
#'   \item{rec_corrected}{Recovery correlation corrected for indicator reliability}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_SSP_Recovery.R` from simulation results
"ssp_recovery_parRecovery"

#' Reliability Results from the SSP Recovery Simulation
#'
#' Contains split-half reliability estimates for behavioral indicators and EZ-DM
#' parameters computed from SSP-generated data.
#'
#' @format ## `ssp_recovery_reliability`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Measure type: "RT", "PC", "drift", "boundary", or "non_dec"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{reliability}{Split-half reliability estimate}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_SSP_Recovery.R` from simulation results
"ssp_recovery_reliability"

# ==============================================================================
# DMC CORRELATION SIMULATION DATASETS
# ==============================================================================

#' Behavioral Results from the DMC Correlation Simulation
#'
#' Contains behavioral statistics from a cross-task correlation simulation using DMC.
#' Two tasks were generated with correlated DMC parameters to assess whether
#' parameter-level correlations propagate to behavioral indicators.
#'
#' @format ## `dmc_correlation_behavior`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Type of behavioral measure: "RT" or "PC"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{value}{The observed value of the behavioral measure}
#'   \item{nRep}{Replication number (1-10)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#'   \item{correlated_par}{Which parameter(s) were correlated: "muc", "A", or "muc-A-tau"}
#' }
#' @source Generated via `scripts/SaveData_DMC_Correlation.R` from simulation results
"dmc_correlation_behavior"

#' EZ-Diffusion Model Results from the DMC Correlation Simulation
#'
#' Contains EZ-DM parameter estimates from two correlated DMC tasks.
#' Used to assess correlation transfer from parameters to EZ-DM estimates.
#'
#' @format ## `dmc_correlation_ezDM`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{EZ-DM parameter: "drift", "boundary", or "non_dec"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{value}{The estimated EZ-DM parameter value}
#'   \item{nRep}{Replication number (1-10)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#'   \item{correlated_par}{Which parameter(s) were correlated: "muc", "A", or "muc-A-tau"}
#' }
#' @source Generated via `scripts/SaveData_DMC_Correlation.R` from simulation results
"dmc_correlation_ezDM"

#' Reliability Results from the DMC Correlation Simulation
#'
#' Contains split-half reliability estimates for indicators from correlated DMC tasks.
#'
#' @format ## `dmc_correlation_reliability`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Measure type: "RT", "PC", "drift", "boundary", or "non_dec"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{reliability}{Split-half reliability estimate}
#'   \item{nRep}{Replication number (1-10)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#'   \item{correlated_par}{Which parameter(s) were correlated: "muc", "A", or "muc-A-tau"}
#' }
#' @source Generated via `scripts/SaveData_DMC_Correlation.R` from simulation results
"dmc_correlation_reliability"

#' Cross-Task Correlations from the DMC Correlation Simulation
#'
#' Contains cross-task correlations for behavioral indicators, EZ-DM parameters,
#' and generating DMC parameters. Key dataset for assessing whether parameters
#' show stronger cross-task correlations than behavioral indicators.
#'
#' @format ## `dmc_correlation_recCorrs`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Measure type: "RT", "PC", "drift", "boundary", or "non_dec"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{correlation}{Observed cross-task correlation for this measure}
#'   \item{obs_correlation}{Observed behavioral/EZ-DM correlation}
#'   \item{target_correlation}{Target parameter correlation (0.7)}
#'   \item{emp_corr.*}{Empirical correlations for various DMC parameters (muc, A, tau, etc.)}
#'   \item{nRep}{Replication number (1-10)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#'   \item{correlated_par}{Which parameter(s) were correlated: "muc", "A", or "muc-A-tau"}
#' }
#' @source Generated via `scripts/SaveData_DMC_Correlation.R` from simulation results
"dmc_correlation_recCorrs"

# ==============================================================================
# SSP CORRELATION SIMULATION DATASETS
# ==============================================================================

#' Behavioral Results from the SSP Correlation Simulation
#'
#' Contains behavioral statistics from a cross-task correlation simulation using SSP.
#' Parallel to DMC correlation but with SSP architecture.
#'
#' @format ## `ssp_correlation_behavior`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Type of behavioral measure: "RT" or "PC"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{value}{The observed value of the behavioral measure}
#'   \item{nRep}{Replication number (1-10)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#'   \item{correlated_par}{Which parameter(s) were correlated: "p", "b", or "p-b-sd_0"}
#' }
#' @source Generated via `scripts/SaveData_SSP_Correlation.R` from simulation results
"ssp_correlation_behavior"

#' EZ-Diffusion Model Results from the SSP Correlation Simulation
#'
#' Contains EZ-DM parameter estimates from two correlated SSP tasks.
#'
#' @format ## `ssp_correlation_ezDM`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{EZ-DM parameter: "drift", "boundary", or "non_dec"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{value}{The estimated EZ-DM parameter value}
#'   \item{nRep}{Replication number (1-10)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#'   \item{correlated_par}{Which parameter(s) were correlated: "p", "b", or "p-b-sd_0"}
#' }
#' @source Generated via `scripts/SaveData_SSP_Correlation.R` from simulation results
"ssp_correlation_ezDM"

#' Reliability Results from the SSP Correlation Simulation
#'
#' Contains split-half reliability estimates for indicators from correlated SSP tasks.
#'
#' @format ## `ssp_correlation_reliability`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Measure type: "RT", "PC", "drift", "boundary", or "non_dec"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{reliability}{Split-half reliability estimate}
#'   \item{nRep}{Replication number (1-10)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#'   \item{correlated_par}{Which parameter(s) were correlated: "p", "b", or "p-b-sd_0"}
#' }
#' @source Generated via `scripts/SaveData_SSP_Correlation.R` from simulation results
"ssp_correlation_reliability"

#' Cross-Task Correlations from the SSP Correlation Simulation
#'
#' Contains cross-task correlations for behavioral indicators, EZ-DM parameters,
#' and generating SSP parameters. Parallel to DMC correlation dataset.
#'
#' @format ## `ssp_correlation_recCorrs`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Measure type: "RT", "PC", "drift", "boundary", or "non_dec"}
#'   \item{indicator}{Experimental condition: "mean", "congruent", "incongruent", or "difference"}
#'   \item{correlation}{Observed cross-task correlation for this measure}
#'   \item{obs_correlation}{Observed behavioral/EZ-DM correlation}
#'   \item{target_correlation}{Target parameter correlation (0.7)}
#'   \item{emp_corr.*}{Empirical correlations for various SSP parameters (p, b, sd_0, etc.)}
#'   \item{nRep}{Replication number (1-10)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials per condition: "50", "100", or "200"}
#'   \item{correlated_par}{Which parameter(s) were correlated: "p", "b", or "p-b-sd_0"}
#' }
#' @source Generated via `scripts/SaveData_SSP_Correlation.R` from simulation results
"ssp_correlation_recCorrs"

# ==============================================================================
# M3 RECOVERY SIMULATION DATASETS
# ==============================================================================

#' Behavioral Results from the M3 Recovery Simulation
#'
#' Contains response proportion indicators from a parameter recovery simulation
#' using the M3 model for complex span tasks. Data were generated with known M3
#' parameters (c, a, f), then behavioral indicators were computed to assess how
#' well parameters can be recovered from categorical response proportions.
#'
#' @format ## `m3_recovery_behavior`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Type of measure: "proportion" (raw category proportions) or "composite" (derived indicators)}
#'   \item{indicator}{Response category or composite: "corr", "other", "distc", "disto", "npl",
#'         "accuracy", "intrusion_mem", "intrusion_dist", "intrusion_total", "npl_rate"}
#'   \item{value}{The observed value of the behavioral measure}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_M3_Recovery.R` from simulation results
"m3_recovery_behavior"

#' Parameter Recovery Results from the M3 Recovery Simulation
#'
#' Contains correlations between generating M3 parameters and recovered parameters
#' from categorical response indicators. Shows how well each M3 parameter (c, a, f)
#' can be recovered from different behavioral measures.
#'
#' @format ## `m3_recovery_parRecovery`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{genPar}{Generating M3 parameter: "c" (context binding), "a" (item activation), "f" (filtering)}
#'   \item{measure}{Indicator type: "proportion" or "composite"}
#'   \item{indicator}{Response category or composite measure}
#'   \item{rec}{Recovery correlation (correlation between generating and recovered parameter)}
#'   \item{rec_corrected}{Recovery correlation corrected for indicator reliability}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_M3_Recovery.R` from simulation results
"m3_recovery_parRecovery"

#' Reliability Results from the M3 Recovery Simulation
#'
#' Contains split-half reliability estimates for M3 behavioral indicators.
#' Reliability is computed using odd-even trial splits within each replication.
#'
#' @format ## `m3_recovery_reliability`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Measure type: "proportion" or "composite"}
#'   \item{indicator}{Response category or composite measure}
#'   \item{reliability}{Split-half reliability estimate (Spearman-Brown corrected)}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 25", "N = 50", or "N = 100"}
#'   \item{nTrials}{Number of trials: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_M3_Recovery.R` from simulation results
"m3_recovery_reliability"

# ==============================================================================
# M3 CORRELATION SIMULATION DATASETS
# ==============================================================================

#' Behavioral Results from the M3 Correlation Simulation
#'
#' Contains behavioral statistics from a cross-task correlation simulation using M3.
#' Two complex span tasks were generated with correlated M3 parameters to assess whether
#' parameter-level correlations propagate to behavioral indicators.
#'
#' @format ## `m3_correlation_behavior`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Type of measure: "proportion" or "composite"}
#'   \item{indicator}{Response category or composite measure}
#'   \item{value}{The observed value of the behavioral measure}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 200"}
#'   \item{nTrials}{Number of trials: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_M3_Correlation.R` from simulation results
"m3_correlation_behavior"

#' Reliability Results from the M3 Correlation Simulation
#'
#' Contains split-half reliability estimates for indicators from correlated M3 tasks.
#'
#' @format ## `m3_correlation_reliability`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Measure type: "proportion" or "composite"}
#'   \item{indicator}{Response category or composite measure}
#'   \item{reliability}{Split-half reliability estimate}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 200"}
#'   \item{nTrials}{Number of trials: "50", "100", or "200"}
#' }
#' @source Generated via `scripts/SaveData_M3_Correlation.R` from simulation results
"m3_correlation_reliability"

#' Cross-Task Correlations from the M3 Correlation Simulation
#'
#' Contains cross-task correlations for M3 behavioral indicators. Key dataset for
#' assessing whether M3 parameter-level correlations translate to indicator-level
#' correlations (correlation transfer failure in working memory tasks).
#'
#' @format ## `m3_correlation_recCorrs`
#' A data frame with multiple rows containing:
#' \describe{
#'   \item{measure}{Measure type: "proportion" or "composite"}
#'   \item{indicator}{Response category or composite measure}
#'   \item{correlation}{Observed cross-task correlation for this indicator}
#'   \item{correlation_corrected}{Cross-task correlation corrected for reliability}
#'   \item{true_corr.*}{Target parameter correlations}
#'   \item{emp_corr.*}{Empirical correlations for M3 parameters (c, a, f)}
#'   \item{nRep}{Replication number (1-250)}
#'   \item{SampleSize}{Sample size factor: "N = 200"}
#'   \item{nTrials}{Number of trials: "50", "100", or "200"}
#'   \item{correlated_par}{Which parameter(s) were correlated: "c", "a", "f", "c-a", "c-f", "a-f", "c-a-f"}
#' }
#' @source Generated via `scripts/SaveData_M3_Correlation.R` from simulation results
"m3_correlation_recCorrs"
