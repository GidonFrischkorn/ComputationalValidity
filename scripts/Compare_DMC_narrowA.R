# Compare the narrow-amplitude DMC re-run (A in [.10, .32]) with the original
# run behind the package datasets (A in [.015, .400]).
#
# Old: data/dmc_recovery_parRecovery.rda, dmc_recovery_reliability.rda,
#      dmc_correlation_recCorrs.rda (aggregated by scripts/SaveData_DMC_*.R).
# New: output/Simulation_DMC_Recovery_narrowA/, output/Simulation_DMC_Correlations_narrowA/
#      (raw per-replication results), aggregated here with the same labels.
# Writes output/narrowA_comparison/*.csv and local/R1_narrowA_comparison.md.
# The package datasets are NOT modified.
#
# Caveat on reliability: the old run used the row-order split that inflated
# accuracy reliabilities; the new run uses the corrected random split. The
# reliability comparison therefore mixes the amplitude change with the fix.
# RT reliabilities are affected only by the amplitude change (the old RT
# split was computed on correct trials in arbitrary order).

suppressPackageStartupMessages({
  library(data.table)
})

root <- here::here()
# RUN_TAG selects which re-run to compare (narrowA = A in [.10, .32];
# A05-35 = A in [.05, .35]); outputs are named by the tag.
run_tag <- Sys.getenv("RUN_TAG", "narrowA")
rec_dir <- file.path(root, "output", paste0("Simulation_DMC_Recovery_", run_tag))
cor_dir <- file.path(root, "output", paste0("Simulation_DMC_Correlations_", run_tag))
out_dir <- file.path(root, "output", paste0(run_tag, "_comparison"))
report_file <- file.path(root, "local", paste0("R1_", run_tag, "_comparison.md"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# ---- helpers ---------------------------------------------------------------

load_rda <- function(name) {
  e <- new.env()
  load(file.path(root, "data", paste0(name, ".rda")), envir = e)
  as.data.table(get(name, envir = e))
}

relabel <- function(dt) {
  if ("indicator" %in% names(dt)) {
    dt[, indicator := factor(as.character(indicator),
                             levels = c("mean", "comp", "incomp", "diff",
                                        "congruent", "incongruent", "difference"))]
    dt[, indicator := fcase(indicator == "comp", "congruent",
                            indicator == "incomp", "incongruent",
                            indicator == "diff", "difference",
                            default = as.character(indicator))]
  }
  if ("measure" %in% names(dt)) {
    dt[, measure := fcase(measure == "v", "drift",
                          measure == "a", "boundary",
                          measure == "t0", "non_dec",
                          default = as.character(measure))]
  }
  dt
}

# SimDesign 2.25 writes one qs2 file per condition (no extension); older runs
# used readRDS, which is why the package datasets were built with SimResults().
read_run <- function(dir, element, cond_cols) {
  files <- list.files(dir, pattern = "Cond-[0-9]+$", full.names = TRUE)
  stopifnot(length(files) > 0)
  rbindlist(lapply(files, function(f) {
    x <- qs2::qd_read(f)
    cond <- as.data.table(x$condition)[, ..cond_cols]
    res <- rbindlist(lapply(seq_along(x$results), function(i) {
      r <- as.data.table(x$results[[i]][[element]])
      r[, nRep := i]
      r
    }), fill = TRUE)
    cbind(res, cond[rep(1, nrow(res))])
  }), fill = TRUE)
}

fmt <- function(x, d = 2) formatC(x, digits = d, format = "f")

md_table <- function(dt, digits = 2) {
  dt <- as.data.frame(dt)
  num <- vapply(dt, is.numeric, logical(1))
  dt[num] <- lapply(dt[num], fmt, d = digits)
  header <- paste0("| ", paste(names(dt), collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(dt)), collapse = "|"), "|")
  rows <- apply(dt, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(header, sep, rows), collapse = "\n")
}

classify <- function(r) fcase(abs(r) > .5, "strong", abs(r) > .3, "moderate",
                              abs(r) > .1, "weak", default = "none")

# ---- recovery --------------------------------------------------------------

old_rec <- load_rda("dmc_recovery_parRecovery")
old_rec[, `:=`(SampleSize = as.integer(sub("N = ", "", SampleSize)),
               nTrials = as.integer(as.character(nTrials)),
               genPar = as.character(genPar), measure = as.character(measure),
               indicator = as.character(indicator))]

new_rec <- read_run(rec_dir, "recovery", c("sample_size", "nTrials"))
setnames(new_rec, "sample_size", "SampleSize")
new_rec[, genPar := as.character(genPar)]
new_rec <- relabel(new_rec)

agg_rec <- function(dt, run) {
  # AUC = 2*A*tau. In the new run it is a true recovery correlation (analyze_data
  # adds AUC to the generating parameters); in the package data it is the mean
  # of the A and tau recoveries (SaveData_DMC_Recovery.R), so old AUC rows are
  # not the same quantity.
  dt[genPar %in% c("A", "b", "muc", "non_dec", "tau", "AUC"),
     .(run = run, rec = mean(rec, na.rm = TRUE), sd = sd(rec, na.rm = TRUE),
       rec_corrected = mean(rec_corrected, na.rm = TRUE), reps = .N),
     by = .(SampleSize, nTrials, measure, indicator, genPar)]
}
rec_all <- rbind(agg_rec(old_rec, "old"), agg_rec(new_rec, "new"))
fwrite(rec_all, file.path(out_dir, "recovery_old_vs_new_all_conditions.csv"))

rec_wide <- dcast(rec_all[SampleSize == 100 & nTrials == 100],
                  genPar + measure + indicator ~ run, value.var = c("rec", "sd"))
rec_wide[, `:=`(delta = rec_new - rec_old,
                class_old = classify(rec_old), class_new = classify(rec_new))]
rec_wide[, class_change := ifelse(class_old == class_new, "", paste(class_old, "->", class_new))]
setorder(rec_wide, genPar, measure, indicator)
fwrite(rec_wide, file.path(out_dir, "recovery_N100_T100.csv"))

# ---- reliability -----------------------------------------------------------

old_rel <- load_rda("dmc_recovery_reliability")
old_rel[, `:=`(SampleSize = as.integer(sub("N = ", "", SampleSize)),
               nTrials = as.integer(as.character(nTrials)),
               measure = as.character(measure), indicator = as.character(indicator))]
new_rel <- read_run(rec_dir, "reliability", c("sample_size", "nTrials"))
setnames(new_rel, "sample_size", "SampleSize")
new_rel <- relabel(new_rel)

agg_rel <- function(dt, run) {
  dt[, .(run = run, reliability = mean(reliability, na.rm = TRUE),
         sd = sd(reliability, na.rm = TRUE), reps = .N),
     by = .(SampleSize, nTrials, measure, indicator)]
}
rel_all <- rbind(agg_rel(old_rel, "old"), agg_rel(new_rel, "new"))
fwrite(rel_all, file.path(out_dir, "reliability_old_vs_new_all_conditions.csv"))
rel_wide <- dcast(rel_all[SampleSize == 100], measure + indicator + nTrials ~ run,
                  value.var = "reliability")
setorder(rel_wide, measure, indicator, nTrials)
fwrite(rel_wide, file.path(out_dir, "reliability_N100.csv"))

# reliability paradox table: reliability vs validity for A, N = 100, 100 trials
paradox <- merge(rel_wide[nTrials == 100, .(measure, indicator, rel_old = old, rel_new = new)],
                 rec_wide[genPar == "A", .(measure, indicator, valA_old = rec_old, valA_new = rec_new)],
                 by = c("measure", "indicator"))
setorder(paradox, measure, indicator)
fwrite(paradox, file.path(out_dir, "reliability_vs_validityA_N100_T100.csv"))

# ---- correlation transfer --------------------------------------------------

old_cor <- load_rda("dmc_correlation_recCorrs")
old_cor[, `:=`(nTrials = as.integer(as.character(nTrials)),
               correlated_par = as.character(correlated_par),
               measure = as.character(measure), indicator = as.character(indicator))]
new_cor <- read_run(cor_dir, "correlations", c("nTrials", "correlated_par"))
new_cor <- relabel(new_cor)

agg_cor <- function(dt, run) {
  dt[correlated_par %in% c("A", "muc", "tau"),
     .(run = run,
       # realised parameter correlation of the one correlated parameter
       # (the emp_corr.* column of the other parameters is NA in these rows)
       gen = mean(as.numeric(get(paste0("emp_corr.", correlated_par[1]))), na.rm = TRUE),
       transfer = mean(correlation, na.rm = TRUE),
       transfer_corrected = mean(correlation_corrected, na.rm = TRUE),
       reps = .N),
     by = .(correlated_par, nTrials, measure, indicator)]
}
cor_all <- rbind(agg_cor(old_cor, "old"), agg_cor(new_cor, "new"))
cor_all[, ratio := transfer / gen]
fwrite(cor_all, file.path(out_dir, "transfer_old_vs_new_all_conditions.csv"))
cor_wide <- dcast(cor_all[nTrials == 100 & measure %in% c("RT", "PC") &
                            indicator %in% c("mean", "difference")],
                  correlated_par + measure + indicator ~ run,
                  value.var = c("gen", "transfer", "transfer_corrected", "ratio"))
setorder(cor_wide, correlated_par, measure, indicator)
fwrite(cor_wide, file.path(out_dir, "transfer_T100.csv"))

# ---- report ----------------------------------------------------------------

n_new_rec <- new_rec[, uniqueN(nRep), by = .(SampleSize, nTrials)]
md <- c(
  paste0("# Re-run `", run_tag, "` against the original run (A in [.015, .400])"),
  "",
  paste0("Generated ", format(Sys.time(), "%Y-%m-%d %H:%M"), " by scripts/Compare_DMC_narrowA.R with RUN_TAG=", run_tag, ". ",
         "Old = package datasets; new = output/*_", run_tag, "/. Replications per condition in the new recovery run: ",
         paste(unique(n_new_rec$V1), collapse = ", "), "."),
  "",
  "Reliability caveat: old accuracy reliabilities come from the row-order split that the re-run corrects, so the PC rows in the reliability tables mix the amplitude change with the fix; RT rows reflect the amplitude change only.",
  "",
  "## Recovery (validity) correlations, N = 100, 100 trials per condition",
  "",
  "Mean across replications; sd = between-replication SD. Classes: strong > .5, moderate > .3, weak > .1 (absolute values).",
  "",
  md_table(rec_wide[, .(genPar, measure, indicator, rec_old, rec_new, delta, sd_old, sd_new, class_change)]),
  "",
  "## Split-half reliability, N = 100",
  "",
  md_table(rel_wide),
  "",
  "## Reliability against validity for A, N = 100, 100 trials",
  "",
  md_table(paradox),
  "",
  "## Correlation transfer, N = 200, 100 trials (single-parameter conditions)",
  "",
  "gen = mean realised parameter correlation across tasks; transfer = mean observed indicator correlation; ratio = transfer / gen.",
  "",
  md_table(cor_wide),
  ""
)
writeLines(md, report_file)
cat(paste(md, collapse = "\n"))
cat("\nWritten:", report_file, "\n")
