library(SimDesign)
library(ComputationalValidity)
library(bmm)

# Set Up Design & Number of Replications per condition
nReplications <- 250

Design <- createDesign(
  sample_size = c(200),
  nTrials = c(50, 100, 200),
  correlation = "random",
  correlated_par = c("c", "a", "f", "c-a", "c-f", "a-f", "c-a-f")
)

# Set parameter limits for M3 (c, a, f)
par_limits <- data.frame(
  min = c(0.5, 0.5, 0.1),
  max = c(3.5, 3.0, 0.9),
  row.names = c("c", "a", "f")
)

cor_limits <- c(min = .25, max = .90)

# Specify functions for generating & analyzing the data
Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition)

  if (!is.null(fixed_objects)) {
    par_limits <- fixed_objects$par_limits
    cor_limits <- fixed_objects$cor_limits
  } else {
    par_limits <- NULL
    cor_limits <- c(min = 0, max = 1)
  }

  # get names of correlated parameters
  pars_correlated <- strsplit(correlated_par, "-")[[1]]

  if (correlation == "randomZ") {
    correlation_Z <- runif(length(pars_correlated),
                           psych::fisherz(cor_limits["min"]),
                           psych::fisherz(cor_limits["max"]))
    correlation <- psych::fisherz2r(correlation_Z)
  } else if (correlation == "random") {
    correlation <- runif(length(pars_correlated), cor_limits["min"], cor_limits["max"])
  } else {
    correlation <- rep(correlation, length(pars_correlated))
  }
  names(correlation) <- pars_correlated

  dat <- simulate_correlation_m3(n_sub = sample_size, n_trials = nTrials,
                                  correlation = correlation, correlated_par = correlated_par,
                                  par_limits = par_limits)
  dat
}

# dat <- Generate(condition = Design[1,], fixed_objects = list(par_limits = par_limits, cor_limits = cor_limits))

Analyse <- function(condition, dat, fixed_objects) {
  ret <- analyze_correlation_m3(dat)
  ret
}

# ret <- Analyse(condition = Design[1,], dat = dat, fixed_objects = list(par_limits = par_limits, cor_limits = cor_limits))

# the summary will be done separately to give us more flexibility
Summarise <- function(condition, results, fixed_objects) {
}

# run simulation if there have been no results saved
if (!file.exists(here::here("output", "res_M3_correlation.rds")) |
    !dir.exists(here::here("output", "Simulation_M3_Correlations"))) {
  res <- runSimulation(design = Design, replications = nReplications,
                       generate = Generate, analyse = Analyse, summarise = Summarise,
                       fixed_objects = list(par_limits = par_limits, cor_limits = cor_limits),
                       save_details = list(
                         safe = TRUE,
                         out_rootdir = here::here("output"),
                         save_results_dirname = "Simulation_M3_Correlations",
                         save_results_filename = "M3_Correlation_Cond"),
                       save_results = TRUE,
                       parallel = TRUE,
                       ncores = 10,
                       packages = c("ComputationalValidity", "bmm", "data.table", "tidytable"))

  save(res, file = here::here("output", "res_M3_correlation.rds"))
}
