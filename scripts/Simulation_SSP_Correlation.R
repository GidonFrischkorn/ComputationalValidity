library(SimDesign)
library(ComputationalValidity)

# Set Up Design & Number of Replications per condition
nReplications <- 500

Design <- createDesign(
  sample_size = c(100),
  nTrials = c(50,100,200),
  correlation = "random",
  correlated_par = c("p","sd_0")
)

# set parameter limits
par_limits = data.frame(
  t(
    rbind(c(.4, 0.15, 0.001, 1, 0.5),
          c(.8, 0.50, 0.010, 4, 2))
  )
)
colnames(par_limits) <- c("min", "max")
rownames(par_limits) <- dRiftDM::ssp_dm()$free_prms

# Specify functions for generating & analyzing the data
Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition)
  if(!is.null(fixed_objects)) {
    par_limits = fixed_objects$par_limits
  } else {
    par_limits <- NULL
  }

  if (correlation == "random") {
    correlation_Z = runif(1, psych::fisherz(0), psych::fisherz(0.9))
    correlation = psych::fisherz2r(correlation_Z)
  }

  dat <- simulate_correlation_ssp(n_sub = sample_size, n_trials = nTrials,
                                  correlation = correlation, correlated_par = correlated_par,
                                  par_limits = par_limits)
  dat
}

dat <- Generate(condition = Design[1,], fixed_objects = list(par_limits = par_limits))

Analyse <- function(condition, dat, fixed_objects) {
  ret <- analyze_correlation(dat)
  ret
}

ret <- Analyse(condition = Design[1,], dat = dat, fixed_objects = list(par_limits = par_limits))

# the summary will be done separately to give us more flexibility
Summarise <- function(condition, results, fixed_objects) {
}

# run simulation if there have been no results saved
if (!file.exists(here::here("output","res_SSP_correlation.rds")) |
   !dir.exists(here::here("output","Simulation_SSP_Correlations"))) {
  res <- runSimulation(design = Design, replications = nReplications,
                       generate = Generate, analyse = Analyse, summarise = Summarise,
                       fixed_objects = list(par_limits = par_limits),
                       save_details = list(
                         safe = TRUE,
                         out_rootdir = here::here(),
                         save_results_dirname = "output/Simulation_SSP_Correlations",
                         save_results_filename = "SSP_Correlation_Cond"),
                       save_results = TRUE,
                       parallel = TRUE,
                       ncores = parallel::detectCores()/2,
                       packages = c("ComputationalValidity","data.table","tidytable"))

  save(res, file = here::here("output","res_SSP_correlation.rds"))
}
