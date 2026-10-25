# DMC cross-task correlation simulation, re-run with a narrower amplitude range
#
# Same design as scripts/Simulation_DMC_Correlation.R (the run behind the
# package datasets), with A drawn from [.10, .32] instead of [.015, .400]
# (co-author meeting 2026-10-20; local/R1_white2018_parameter_check.md), the
# corrected split-half reliability in R/get_reliability.R, and fixed seeds.
# Trial-level data are not stored here (N = 200 x 2 tasks x 21 conditions would
# run to several GB); genPars and reliabilities are stored as before.
# Results go to output/Simulation_DMC_Correlations_narrowA/ and
# output/res_DMC_correlation_narrowA.rds. The original results are untouched.
#
# Run with R 4.5.2 through scripts/run_DMC_narrowA.sh.

library(SimDesign)
library(dRiftDM)
pkg_path <- here::here()
pkgload::load_all(pkg_path, quiet = TRUE)

nReplications <- 250

Design <- createDesign(
  sample_size = c(200),
  nTrials = c(50, 100, 200),
  correlation = "random",
  correlated_par = c("muc", "A", "tau", "muc-A", "muc-tau", "A-tau", "muc-A-tau")
)

# amplitude range and run tag from the environment (see the recovery script)
a_min <- as.numeric(Sys.getenv("A_MIN", "0.10"))
a_max <- as.numeric(Sys.getenv("A_MAX", "0.32"))
run_tag <- Sys.getenv("RUN_TAG", "narrowA")
cat("Run tag:", run_tag, " A range:", a_min, "-", a_max, "\n")
par_limits <- data.frame(
  min = c(1.5, .4, 0.15, 0.02, a_min),
  max = c(4.0, .8, 0.50, 0.12, a_max),
  row.names = c("muc", "b", "non_dec", "tau", "A")
)

cor_limits <- c(min = .25, max = .90)

Generate <- function(condition, fixed_objects = NULL) {
  Attach(condition)
  par_limits <- fixed_objects$par_limits
  cor_limits <- fixed_objects$cor_limits

  pars_correlated <- strsplit(correlated_par, "-")[[1]]

  if (correlation == "randomZ") {
    correlation_Z <- runif(length(pars_correlated),
                           psych::fisherz(cor_limits["min"]), psych::fisherz(cor_limits["max"]))
    correlation <- psych::fisherz2r(correlation_Z)
  } else if (correlation == "random") {
    correlation <- runif(length(pars_correlated), cor_limits["min"], cor_limits["max"])
  } else {
    correlation <- rep(correlation, length(pars_correlated))
  }
  names(correlation) <- pars_correlated

  simulate_correlation_dmc(n_sub = sample_size, n_trials = nTrials,
                           correlation = correlation, correlated_par = correlated_par,
                           par_limits = par_limits)
}

Analyse <- function(condition, dat, fixed_objects) {
  analyze_correlation(dat)
}

Summarise <- function(condition, results, fixed_objects) {
}

ncores <- as.integer(Sys.getenv("SIM_NCORES", "8"))
master_libs <- .libPaths()
# workers must run R 4.5.2 too; the framework's Rscript would start the default R
cl <- parallel::makeCluster(ncores, rscript = file.path(pkg_path, "scripts", "Rscript45.sh"))
parallel::clusterExport(cl, c("pkg_path", "master_libs"))
invisible(parallel::clusterEvalQ(cl, {
  .libPaths(master_libs)
  library(dRiftDM); library(data.table); library(tidytable)
  pkgload::load_all(pkg_path, quiet = TRUE)
}))
print(parallel::clusterEvalQ(cl, c(R.version.string, .libPaths()[1]))[[1]])

out_dir <- here::here("output", paste0("Simulation_DMC_Correlations_", run_tag))
res_file <- here::here("output", paste0("res_DMC_correlation_", run_tag, ".rds"))
if (!file.exists(res_file) || !dir.exists(out_dir)) {
  res <- runSimulation(design = Design, replications = nReplications,
                       generate = Generate, analyse = Analyse, summarise = Summarise,
                       fixed_objects = list(par_limits = par_limits, cor_limits = cor_limits),
                       seed = 20261100 + seq_len(nrow(Design)),
                       save_details = list(
                         safe = TRUE,
                         out_rootdir = here::here("output"),
                         save_results_dirname = basename(out_dir),
                         save_results_filename = paste0("DMC_Correlation_", run_tag, "_Cond")),
                       save_results = TRUE,
                       cl = cl,
                       packages = c("data.table", "tidytable", "dRiftDM", "psych"))
  save(res, file = res_file)
  print(res[, c("sample_size", "nTrials", "correlated_par", "REPLICATIONS", "SIM_TIME")], n = 30)
}
parallel::stopCluster(cl)
