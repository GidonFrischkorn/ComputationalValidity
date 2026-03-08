# ==============================================================================
# Generate Overestimation / Underestimation Appendix Figure
# ==============================================================================
# Uses existing correlation simulation data to illustrate two complementary
# ways behavioral cross-task correlations can mislead:
#
# Panel A — Underestimation: When the attention-control parameter (A / sd_0)
#   is correlated across tasks, RT_diff tracks the sharing but is attenuated;
#   RT_mean and both accuracy indicators correctly stay near zero.
#
# Panel B — Overestimation: When the controlled-drift parameter (muc / p) is
#   correlated while A / sd_0 are not, RT_mean and accuracy indicators show
#   spurious positive cross-task correlations; RT_diff correctly stays near zero.
# ==============================================================================

rm(list = ls())
graphics.off()

pacman::p_load(here, data.table, ggplot2, patchwork, ComputationalValidity)

# ── Color palette (consistent with existing manuscript figures) ───────────────
col_rt_diff <- "#1565C0"   # dark blue
col_rt_mean <- "#90CAF9"   # light blue
col_pc_diff <- "#B71C1C"   # dark red
col_pc_mean <- "#EF9A9A"   # light red

# ── Theme (consistent with existing manuscript figures) ───────────────────────
theme_manuscript <- theme_bw(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.key = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 12),
    axis.title = element_text(size = 13, face = "bold"),
    strip.text = element_text(size = 11, face = "bold")
  )

dir.create(here("figures", "manuscript"), showWarnings = FALSE, recursive = TRUE)

# ==============================================================================
# Load existing correlation simulation data
# ==============================================================================
data(dmc_correlation_recCorrs, package = "ComputationalValidity")
data(ssp_correlation_recCorrs, package = "ComputationalValidity")

# ==============================================================================
# Helper: extract RT and PC indicators for a given correlated_par
# ==============================================================================
extract_panel_data <- function(dmc_dat, ssp_dat,
                               par_dmc, par_ssp,
                               xcol_dmc, xcol_ssp,
                               sample_size = "N = 200",
                               n_trials = "100") {
  dmc <- data.table(dmc_dat)[
    correlated_par == par_dmc &
      SampleSize == sample_size &
      nTrials == n_trials &
      measure %in% c("RT", "PC") &
      indicator %in% c("mean", "difference")
  ][, .(
    x_cor = get(xcol_dmc),
    correlation_corrected,
    measure,
    indicator,
    model = "DMC"
  )]

  ssp <- data.table(ssp_dat)[
    correlated_par == par_ssp &
      SampleSize == sample_size &
      nTrials == n_trials &
      measure %in% c("RT", "PC") &
      indicator %in% c("mean", "difference")
  ][, .(
    x_cor = get(xcol_ssp),
    correlation_corrected,
    measure,
    indicator,
    model = "SSP"
  )]

  out <- rbind(dmc, ssp)
  out[, indicator_label := fcase(
    measure == "RT" & indicator == "difference", "RT Difference",
    measure == "RT" & indicator == "mean",       "RT Mean",
    measure == "PC" & indicator == "difference", "Accuracy Difference",
    measure == "PC" & indicator == "mean",       "Accuracy Mean"
  )]
  out[, indicator_label := factor(
    indicator_label,
    levels = c("RT Difference", "RT Mean", "Accuracy Difference", "Accuracy Mean")
  )]
  out[, model := factor(model, levels = c("DMC", "SSP"))]
  out
}

# Panel A: attention-control parameter correlated (A for DMC, sd_0 for SSP)
panel_a_dat <- extract_panel_data(
  dmc_correlation_recCorrs, ssp_correlation_recCorrs,
  par_dmc = "A", par_ssp = "sd_0",
  xcol_dmc = "emp_corr.A", xcol_ssp = "emp_corr.sd_0"
)

# Panel B: controlled-drift parameter correlated (muc for DMC, p for SSP)
panel_b_dat <- extract_panel_data(
  dmc_correlation_recCorrs, ssp_correlation_recCorrs,
  par_dmc = "muc", par_ssp = "p",
  xcol_dmc = "emp_corr.muc", xcol_ssp = "emp_corr.p"
)

cat(sprintf("Panel A — DMC: %d obs (%d indicators); SSP: %d obs\n",
            nrow(panel_a_dat[model == "DMC"]),
            uniqueN(panel_a_dat[model == "DMC"]$indicator_label),
            nrow(panel_a_dat[model == "SSP"])))
cat(sprintf("Panel B — DMC: %d obs (%d indicators); SSP: %d obs\n",
            nrow(panel_b_dat[model == "DMC"]),
            uniqueN(panel_b_dat[model == "DMC"]$indicator_label),
            nrow(panel_b_dat[model == "SSP"])))

# ==============================================================================
# Shared plot function
# ==============================================================================
make_panel <- function(dat, title) {
  ggplot(dat, aes(x = x_cor, y = correlation_corrected, color = indicator_label)) +
    geom_abline(
      intercept = 0, slope = 1,
      linetype = "dashed", color = "gray30", linewidth = 0.7
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dotted", color = "gray50", linewidth = 0.5
    ) +
    geom_point(alpha = 0.25, size = 1.2) +
    geom_smooth(method = "lm", se = TRUE, linewidth = 1.2, alpha = 0.2) +
    facet_wrap(~model, ncol = 2) +
    scale_color_manual(
      name = "Indicator",
      values = c(
        "RT Difference"       = col_rt_diff,
        "RT Mean"             = col_rt_mean,
        "Accuracy Difference" = col_pc_diff,
        "Accuracy Mean"       = col_pc_mean
      )
    ) +
    coord_cartesian(xlim = c(0, 1), ylim = c(-0.2, 1)) +
    labs(
      x = "Generating Parameter Correlation",
      y = "Observed Cross-Task Correlation",
      title = title
    ) +
    theme_manuscript +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      legend.position = "bottom"
    )
}

p_a <- make_panel(
  panel_a_dat,
  "A   Underestimation (A correlated / DMC; sd\u2080 correlated / SSP)"
)

p_b <- make_panel(
  panel_b_dat,
  "B   Overestimation (\u03bcc correlated / DMC; p correlated / SSP)"
)

# ==============================================================================
# Combine and save
# ==============================================================================
p_overestimation <- p_a / p_b + plot_layout(heights = c(1, 1))

ggsave(
  here("figures", "manuscript", "DMC_SSP_Overestimation.png"),
  p_overestimation, width = 10, height = 10, dpi = 300, device = ragg::agg_png
)
ggsave(
  here("figures", "manuscript", "DMC_SSP_Overestimation.tiff"),
  p_overestimation, width = 10, height = 10, dpi = 300, device = ragg::agg_tiff
)

message("Saved DMC_SSP_Overestimation.{png,tiff} to figures/manuscript/")
