# =============================================================================
# Setup Verification Script
# =============================================================================
# Run this after cloning the repository and running download_data.R to confirm
# your environment is correctly configured before running any analysis scripts.
#
# PREREQUISITE: Open ComputationalValidity.Rproj in RStudio first.
# =============================================================================

cat("=== ComputationalValidity Setup Check ===\n\n")

# 1. Check that here() anchors to the repository root -------------------------
library(here)
project_root <- here()
rproj_exists <- file.exists(file.path(project_root, "ComputationalValidity.Rproj"))

if (rproj_exists) {
  cat("[OK]   here() root:", project_root, "\n")
} else {
  cat("[FAIL] here() does not point to the ComputationalValidity repository root.\n")
  cat("       Current root:", project_root, "\n")
  cat("       FIX: Open ComputationalValidity.Rproj in RStudio and re-run this script.\n\n")
  stop("Setup check failed: wrong working directory.", call. = FALSE)
}

# 2. Check required packages ---------------------------------------------------
cat("\n--- Package checks ---\n")
required_pkgs <- c(
  "devtools", "here", "osfr",
  "data.table", "tidytable", "ggplot2", "patchwork",
  "pacman", "SimDesign", "dRiftDM", "bmm",
  "scales", "viridis", "stringr", "psychometric"
)
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]

if (length(missing_pkgs) == 0) {
  cat("[OK]   All required packages are installed.\n")
} else {
  cat("[WARN] Missing packages:", paste(missing_pkgs, collapse = ", "), "\n")
  cat("       Install with: install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "), "))\n")
}

# 3. Check package loads from source -------------------------------------------
cat("\n--- Package load check ---\n")
load_ok <- tryCatch({
  devtools::load_all(".", quiet = TRUE)
  TRUE
}, error = function(e) {
  cat("[FAIL] devtools::load_all('.') failed:\n       ", conditionMessage(e), "\n")
  FALSE
})
if (load_ok) cat("[OK]   Package loaded from source via devtools::load_all().\n")

# 4. Check manuscript data files -----------------------------------------------
cat("\n--- Data file checks ---\n")
manuscript_files <- c(
  "dmc_recovery_behavior.rda",    "dmc_recovery_ezDM.rda",
  "dmc_recovery_parRecovery.rda", "dmc_recovery_reliability.rda",
  "dmc_correlation_behavior.rda", "dmc_correlation_ezDM.rda",
  "dmc_correlation_recCorrs.rda", "dmc_correlation_reliability.rda",
  "ssp_recovery_behavior.rda",    "ssp_recovery_ezDM.rda",
  "ssp_recovery_parRecovery.rda", "ssp_recovery_reliability.rda",
  "ssp_correlation_behavior.rda", "ssp_correlation_ezDM.rda",
  "ssp_correlation_recCorrs.rda", "ssp_correlation_reliability.rda"
)
m3_files <- c(
  "m3_recovery_behavior.rda",    "m3_recovery_parRecovery.rda",
  "m3_recovery_reliability.rda", "m3_correlation_behavior.rda",
  "m3_correlation_recCorrs.rda", "m3_correlation_reliability.rda"
)

ms_present <- file.exists(file.path(here("data"), manuscript_files))
m3_present <- file.exists(file.path(here("data"), m3_files))

if (all(ms_present)) {
  cat("[OK]   All 16 manuscript data files are present in data/\n")
} else {
  missing_ms <- manuscript_files[!ms_present]
  cat("[FAIL] Missing manuscript data files (", sum(!ms_present), "of 16):\n")
  cat("       ", paste(missing_ms, collapse = "\n        "), "\n")
  cat("       FIX: Run scripts/download_data.R\n")
}

if (all(m3_present)) {
  cat("[OK]   All 6 M3 data files are present in data/\n")
} else {
  cat("[NOTE] M3 data files not found (", sum(!m3_present), "of 6 missing).\n")
  cat("       M3 analyses are not part of the manuscript — these files are not on OSF.\n")
  cat("       To generate: run Simulation_M3_*.R then SaveData_M3_*.R\n")
}

# 5. Check output files --------------------------------------------------------
cat("\n--- Output file checks ---\n")
output_files <- c(
  "res_DMC_recovery.rds", "res_DMC_correlation.rds",
  "res_SSP_recovery.rds", "res_SSP_correlation.rds"
)
out_present <- file.exists(file.path(here("output"), output_files))

if (all(out_present)) {
  cat("[OK]   All 4 aggregated output files are present in output/\n")
} else {
  cat("[NOTE] Missing output files (needed only to re-run SaveData_*.R scripts):\n")
  cat("       ", paste(output_files[!out_present], collapse = "\n        "), "\n")
  cat("       These are NOT needed to reproduce figures — only to regenerate data/ files.\n")
  cat("       FIX: Run scripts/download_data.R (downloads output/ if available on OSF),\n")
  cat("            or run Simulation_*.R scripts to regenerate from scratch.\n")
}

# 6. Check figure output directory ---------------------------------------------
cat("\n--- Directory checks ---\n")
if (dir.exists(here("figures", "manuscript"))) {
  cat("[OK]   figures/manuscript/ exists.\n")
} else {
  cat("[NOTE] figures/manuscript/ does not exist yet.\n")
  cat("       It will be created automatically when Generate_*.R scripts are run.\n")
}

# Summary ----------------------------------------------------------------------
cat("\n=== Summary ===\n")
cat("If all items show [OK] or [NOTE], your setup is ready.\n")
cat("[FAIL] items must be resolved before running analysis scripts.\n\n")
cat("Recommended script execution order for reproducing manuscript figures:\n")
cat("  1. scripts/download_data.R              (download data from OSF)\n")
cat("  2. scripts/Generate_DMC_Plots.R         (DMC manuscript figures)\n")
cat("  3. scripts/Generate_SSP_Plots.R         (SSP supplementary figures)\n")
cat("  4. scripts/Generate_Model_Comparison.R\n")
cat("  5. scripts/Generate_ModelValidation_Plots.R\n")
cat("  6. scripts/Generate_OverestimationPlots.R\n")
cat("  7. quarto render reports/ValidityComputationalModelling.qmd\n")
cat("\nFor M3 figures (not in manuscript): scripts/Generate_M3_Plots.R\n")
