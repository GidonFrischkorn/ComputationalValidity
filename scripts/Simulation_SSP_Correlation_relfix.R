# SSP cross-task correlation simulation, re-run with the corrected split-half
# reliability. Same design and limits as scripts/Simulation_SSP_Correlation.R;
# fixed seeds; source package loaded with pkgload on master and workers.
# Trial-level data are not stored (size). Results go to
# output/Simulation_SSP_Correlations_<tag>/ and output/res_SSP_correlation_<tag>.rds.
# Run through scripts/run_relfix_all.sh.

library(SimDesign)
library(dRiftDM)
pkg_path <- here::here()
pkgload::load_all(pkg_path, quiet = TRUE)

run_tag <- Sys.getenv("RUN_TAG", "relfix")
cat("Run tag:", run_tag, "\n")

nReplications <- 250

Design <- createDesign(
  sample_size = c(200),
  nTrials = c(50, 100, 200),
  correlation = "random",
  correlated_par = c("p", "sd_0", "p-sd_0")
)

par_limits <- data.frame(
  min = c(.4, 0.15, 1, 0.5),
  max = c(.8, 0.50, 4, 2.0),
  row.names = c("b", "non_dec", "p", "sd_0")
)

cor_limits <- c(min = 0.25, max = 0.9)

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

  simulate_correlation_ssp(n_sub = sample_size, n_trials = nTrials,
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
cl <- parallel::makeCluster(ncores, rscript = file.path(pkg_path, "scripts", "Rscript45.sh"))
parallel::clusterExport(cl, c("pkg_path", "master_libs"))
invisible(parallel::clusterEvalQ(cl, {
  .libPaths(master_libs)
  library(dRiftDM); library(data.table); library(tidytable)
  pkgload::load_all(pkg_path, quiet = TRUE)
}))
print(parallel::clusterEvalQ(cl, c(R.version.string, .libPaths()[1]))[[1]])

out_dir <- here::here("output", paste0("Simulation_SSP_Correlations_", run_tag))
res_file <- here::here("output", paste0("res_SSP_correlation_", run_tag, ".rds"))
if (!file.exists(res_file) || !dir.exists(out_dir)) {
  res <- runSimulation(design = Design, replications = nReplications,
                       generate = Generate, analyse = Analyse, summarise = Summarise,
                       fixed_objects = list(par_limits = par_limits, cor_limits = cor_limits),
                       seed = 20262100 + seq_len(nrow(Design)),
                       save_details = list(
                         safe = TRUE,
                         out_rootdir = here::here("output"),
                         save_results_dirname = basename(out_dir),
                         save_results_filename = paste0("SSP_Correlation_", run_tag, "_Cond")),
                       save_results = TRUE,
                       cl = cl,
                       packages = c("data.table", "tidytable", "dRiftDM", "psych"))
  save(res, file = res_file)
  print(res[, c("sample_size", "nTrials", "correlated_par", "REPLICATIONS", "SIM_TIME")])
}
parallel::stopCluster(cl)
