library(SimDesign)
library(ComputationalValidity)

# Set Up Design & Number of Replications per condition
nReplications <- 250

Design <- createDesign(
  sample_size = c(200),
  nTrials = c(50,100,200),
  correlation = "random",
  correlated_par = c("muc","A","tau","muc-A","muc-tau","A-tau","muc-A-tau")
)

# set up model to simulate from - match DMC_Recovery configuration
dmc_model <- dRiftDM::dmc_dm(var_non_dec = FALSE, var_start = FALSE)

# set parameter limits - match DMC_Recovery exactly
par_limits = data.frame(
  t(
    rbind(c(1.5, .4, 0.15, 0.02, 0.015),
          c(4.0, .8, 0.50, 0.12, 0.400))
  )
)
colnames(par_limits) <- c("min", "max")
rownames(par_limits) <- c("muc", "b", "non_dec", "tau", "A")

cor_limits <- c(min = .25, max = .90)

# Specify functions for generating & analyzing the data
Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition)

  # set parameter limits
  if(!is.null(fixed_objects)) {
    par_limits = fixed_objects$par_limits
    cor_limits = fixed_objects$cor_limits
  } else {
    par_limits <- NULL
    cor_limits <- c(min = 0, max = 1)
  }

  # get names of correlated parameters
  pars_correlated <- strsplit(correlated_par, "-")[[1]]

  if (correlation == "randomZ") {
    correlation_Z = runif(length(pars_correlated), psych::fisherz(cor_limits["min"]), psych::fisherz(cor_limits["max"]))
    correlation = psych::fisherz2r(correlation_Z)
  } else if(correlation == "random") {
    correlation = runif(length(pars_correlated), cor_limits["min"], cor_limits["max"])
  } else {
    correlation = rep(correlation, length(pars_correlated))
  }
  names(correlation) <- pars_correlated

  dat <- simulate_correlation_dmc(n_sub = sample_size, n_trials = nTrials,
                                  correlation = correlation, correlated_par = correlated_par,
                                  par_limits = par_limits)
  dat
}

# dat <- Generate(condition = Design[7,], fixed_objects = list(par_limits = par_limits, cor_limits = cor_limits))

Analyse <- function(condition, dat, fixed_objects) {
  ret <- analyze_correlation(dat)
  ret
}

# ret <- Analyse(condition = Design[19,], dat = dat, fixed_objects = list(par_limits = par_limits, cor_limits = cor_limits))

# the summary will be done separately to give us more flexibility
Summarise <- function(condition, results, fixed_objects) {
}

# run simulation if there have been no results saved
if (!file.exists(here::here("output","res_DMC_correlation.rds")) |
   !dir.exists(here::here("output","Simulation_DMC_Correlations"))) {
  res <- runSimulation(design = Design, replications = nReplications,
                       generate = Generate, analyse = Analyse, summarise = Summarise,
                       fixed_objects = list(par_limits = par_limits, cor_limits = cor_limits),
                       save_details = list(
                         safe = TRUE,
                         out_rootdir = here::here("output"),
                         save_results_dirname = "Simulation_DMC_Correlations",
                         save_results_filename = "DMC_Correlation_Cond"),
                       save_results = TRUE,
                       parallel = TRUE,
                       ncores = 10,
                       packages = c("ComputationalValidity","data.table","tidytable"))

  save(res, file = here::here("output","res_DMC_correlation.rds"))
}
