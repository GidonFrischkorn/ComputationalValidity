#' @title Circularity Diagram Figure
#'
#' Generates a two-panel schematic contrasting correlation-based validity
#' (left, 5-step closed cycle) with the computational approach (right,
#' 5-step open chain). Both panels share the same 2×2 grid layout for
#' steps 1–4, plus a 5th box at bottom-centre.
#'
#' Left panel: The 5-step cycle highlights the double-duty problem.
#'   Steps 1-4 proceed clockwise (Assume → Predict → Observe → Infer
#'   indicator validity), then step 5 (Confirm general ability) completes
#'   the reasoning — but closes the circle using the same correlational
#'   evidence, feeding back into the original assumption. The closing arrow
#'   (5→1) is rendered as a dashed L-route along the left margin to visually
#'   distinguish the problematic link.
#'
#' Right panel: The same 5-step structure flows in one direction only.
#'   Steps 1-4 (Assume formal theory → Processes formalized as parameters
#'   → Generate data → Test indicator validity) lead to step 5
#'   (Cross-task correlations: testable prediction). No closing arrow —
#'   the cycle is broken.

library(ggplot2)
library(patchwork)
library(here)

# ── Shared geometry ────────────────────────────────────────────────────────────
box_w <- 3.2   # width of all boxes (uniform)
box_h <- 1.8   # height of all boxes

# 2×2 grid centres
cx1 <- 2.5; cy1 <- 7.5   # top-left
cx2 <- 7.5; cy2 <- 7.5   # top-right
cx3 <- 7.5; cy3 <- 2.5   # bottom-right
cx4 <- 2.5; cy4 <- 2.5   # bottom-left

# Box 5: directly below box 4, same column and size (both panels)
cx5 <- cx4; cy5 <- -2.0

# Box 6: directly below box 3, same column and size (right panel only)
cx6 <- cx3; cy6 <- -2.0

# Box edge helpers (w/h default to shared globals)
xl <- function(cx, w = box_w) cx - w / 2
xr <- function(cx, w = box_w) cx + w / 2
yb <- function(cy, h = box_h) cy - h / 2
yt <- function(cy, h = box_h) cy + h / 2

# Standard arrowhead
arr <- arrow(length = unit(0.10, "inches"), type = "closed")

# ── LEFT PANEL: Correlation-Based Validity ─────────────────────────────────────
fill_L    <- "#FDECEA"
border_L  <- "#C62828"
text_L    <- "#B71C1C"
closing_L <- "#7B0000"   # darker red marks the problematic closing link

p_left <- ggplot() +
  # ── Boxes 1-4 ──
  annotate("rect",
    xmin = xl(cx1), xmax = xr(cx1), ymin = yb(cy1), ymax = yt(cy1),
    fill = fill_L, color = border_L, linewidth = 0.8
  ) +
  annotate("rect",
    xmin = xl(cx2), xmax = xr(cx2), ymin = yb(cy2), ymax = yt(cy2),
    fill = fill_L, color = border_L, linewidth = 0.8
  ) +
  annotate("rect",
    xmin = xl(cx3), xmax = xr(cx3), ymin = yb(cy3), ymax = yt(cy3),
    fill = fill_L, color = border_L, linewidth = 0.8
  ) +
  annotate("rect",
    xmin = xl(cx4), xmax = xr(cx4), ymin = yb(cy4), ymax = yt(cy4),
    fill = fill_L, color = border_L, linewidth = 0.8
  ) +
  # ── Box 5 ──
  annotate("rect",
    xmin = xl(cx5), xmax = xr(cx5),
    ymin = yb(cy5), ymax = yt(cy5),
    fill = fill_L, color = border_L, linewidth = 0.8
  ) +
  # ── Labels ──
  annotate("text", x = cx1, y = cy1,
    label = "Assume\ngeneral ability",
    color = text_L, size = 3.6, hjust = 0.5, vjust = 0.5, family = "Arial"
  ) +
  annotate("text", x = cx2, y = cy2,
    label = "Predict\ncorrelations",
    color = text_L, size = 3.6, hjust = 0.5, vjust = 0.5, family = "Arial"
  ) +
  annotate("text", x = cx3, y = cy3,
    label = "Observe task\ncorrelations",
    color = text_L, size = 3.6, hjust = 0.5, vjust = 0.5, family = "Arial"
  ) +
  annotate("text", x = cx4, y = cy4,
    label = "Infer indicator\nvalidity",
    color = text_L, size = 3.6, hjust = 0.5, vjust = 0.5, family = "Arial"
  ) +
  annotate("text", x = cx5, y = cy5,
    label = "Confirm\ngeneral ability",
    color = text_L, size = 3.6, hjust = 0.5, vjust = 0.5, family = "Arial"
  ) +
  # ── Arrows 1→2, 2→3, 3→4 (solid red, clockwise main flow) ──
  annotate("segment",
    x = xr(cx1), xend = xl(cx2), y = cy1, yend = cy2,
    color = border_L, linewidth = 0.9, arrow = arr
  ) +
  annotate("segment",
    x = cx2, xend = cx3, y = yb(cy2), yend = yt(cy3),
    color = border_L, linewidth = 0.9, arrow = arr
  ) +
  annotate("segment",
    x = xl(cx3), xend = xr(cx4), y = cy3, yend = cy4,
    color = border_L, linewidth = 0.9, arrow = arr
  ) +
  # ── Arrow 4→5 (diagonal down-right, solid red) ──
  annotate("segment",
    x = cx4, xend = cx5,
    y = yb(cy4), yend = yt(cy5),
    color = border_L, linewidth = 0.9, arrow = arr
  ) +
  # ── Curved arrow 3→5: same correlational evidence feeds both step 4 & 5 ──
  annotate("curve",
    x = cx3, xend = xr(cx5),
    y = yb(cy3), yend = cy5,
    color = border_L, linewidth = 0.9, curvature = -0.35,
    arrow = arr
  ) +
  annotate("text",
    x = 8.1, y = -0.1,
    label = "double\nduty",
    color = border_L, size = 3.4, hjust = 1, vjust = 0.5,
    fontface = "italic", family = "Arial"
  ) +
  # ── Closing arrow 5→1: L-route along left margin (dashed, dark red) ──
  # Segment A: leftward from box 5 left edge to x = 0.3
  annotate("segment",
    x = xl(cx5), xend = 0.3, y = cy5, yend = cy5,
    color = closing_L, linewidth = 1.1, linetype = "dashed"
  ) +
  # Segment B: upward along left margin to the height of box 1
  annotate("segment",
    x = 0.3, xend = 0.3, y = cy5, yend = cy1,
    color = closing_L, linewidth = 1.1, linetype = "dashed"
  ) +
  # Segment C: rightward into box 1 left edge (arrowhead here)
  annotate("segment",
    x = 0.3, xend = xl(cx1), y = cy1, yend = cy1,
    color = closing_L, linewidth = 1.1, linetype = "dashed", arrow = arr
  ) +
  labs(title = "Correlation-Based Validity") +
  coord_cartesian(xlim = c(0, 10), ylim = c(-4, 10), expand = FALSE) +
  theme_void() +
  theme(
    plot.title  = element_text(
      face = "bold", size = 13, hjust = 0.5,
      margin = margin(b = 10), family = "Arial"
    ),
    plot.margin = margin(12, 16, 12, 12)
  )

# ── RIGHT PANEL: Computational Approach ───────────────────────────────────────
fill_R    <- "#E8EAF6"
border_R  <- "#3949AB"
text_R    <- "#1A237E"

# Box 6: model evaluation (lighter blue = supplementary validation step)
fill_R6   <- "#E3F2FD"
border_R6 <- "#5C9BD6"
text_R6   <- "#1A237E"

p_right <- ggplot() +
  # ── Boxes 1-4 ──
  annotate("rect",
    xmin = xl(cx1), xmax = xr(cx1), ymin = yb(cy1), ymax = yt(cy1),
    fill = fill_R, color = border_R, linewidth = 0.8
  ) +
  annotate("rect",
    xmin = xl(cx2), xmax = xr(cx2), ymin = yb(cy2), ymax = yt(cy2),
    fill = fill_R, color = border_R, linewidth = 0.8
  ) +
  annotate("rect",
    xmin = xl(cx3), xmax = xr(cx3), ymin = yb(cy3), ymax = yt(cy3),
    fill = fill_R, color = border_R, linewidth = 0.8
  ) +
  annotate("rect",
    xmin = xl(cx4), xmax = xr(cx4), ymin = yb(cy4), ymax = yt(cy4),
    fill = fill_R, color = border_R, linewidth = 0.8
  ) +
  # ── Box 5 ──
  annotate("rect",
    xmin = xl(cx5), xmax = xr(cx5),
    ymin = yb(cy5), ymax = yt(cy5),
    fill = fill_R, color = border_R, linewidth = 0.8
  ) +
  # ── Labels ──
  annotate("text", x = cx1, y = cy1,
    label = "Assume\nformal theory",
    color = text_R, size = 3.6, hjust = 0.5, vjust = 0.5, family = "Arial"
  ) +
  annotate("text", x = cx2, y = cy2,
    label = "Processes\nformalized as\nparameters",
    color = text_R, size = 3.6, hjust = 0.5, vjust = 0.5, family = "Arial"
  ) +
  annotate("text", x = cx3, y = cy3,
    label = "Generate data\nbased on model",
    color = text_R, size = 3.6, hjust = 0.5, vjust = 0.5, family = "Arial"
  ) +
  annotate("text", x = cx4, y = cy4,
    label = "Test\nindicator validity",
    color = text_R, size = 3.6, hjust = 0.5, vjust = 0.5, family = "Arial"
  ) +
  annotate("text", x = cx5, y = cy5,
    label = "Cross-task correlations\n(testable prediction)",
    color = text_R, size = 3.6, hjust = 0.5, vjust = 0.5, family = "Arial"
  ) +
  # ── Arrows 1→2, 2→3, 3→4 (solid blue) ──
  annotate("segment",
    x = xr(cx1), xend = xl(cx2), y = cy1, yend = cy2,
    color = border_R, linewidth = 0.9, arrow = arr
  ) +
  annotate("segment",
    x = cx2, xend = cx3, y = yb(cy2), yend = yt(cy3),
    color = border_R, linewidth = 0.9, arrow = arr
  ) +
  annotate("segment",
    x = xl(cx3), xend = xr(cx4), y = cy3, yend = cy4,
    color = border_R, linewidth = 0.9, arrow = arr
  ) +
  # ── Arrow 4→5 (same routing as left panel, solid blue) ──
  annotate("segment",
    x = cx4, xend = cx5,
    y = yb(cy4), yend = yt(cy5),
    color = border_R, linewidth = 0.9, arrow = arr
  ) +
  # ── Box 6: model evaluation (lighter blue fill/border, supplementary step) ──
  annotate("rect",
    xmin = xl(cx6), xmax = xr(cx6),
    ymin = yb(cy6), ymax = yt(cy6),
    fill = fill_R6, color = border_R6, linewidth = 0.8
  ) +
  annotate("text", x = cx6, y = cy6,
    label = "Reproduce\nempirical effects",
    color = text_R6, size = 3.6, hjust = 0.5, vjust = 0.5, family = "Arial"
  ) +
  # ── Solid arrow 3→6 (model generates data → check against known effects) ──
  annotate("segment",
    x = cx3, xend = cx6,
    y = yb(cy3), yend = yt(cy6),
    color = border_R6, linewidth = 0.9, arrow = arr
  ) +
  # ── Dashed arrow: box 4 → box 2 (parameter recovery: indicators → parameters) ──
  # Directed upward-diagonal: indicators (box 4) correlate with parameters (box 2).
  annotate("segment",
    x    = xr(cx4), xend = xl(cx2),
    y    = yt(cy4), yend = yb(cy2),
    color = "#5C6BC0", linewidth = 0.85, linetype = "dotted",
    arrow = arrow(ends = "last", type = "closed", length = unit(0.10, "inches"))
  ) +
  annotate("text",
    x = (xr(cx4) + xl(cx2)) / 2 + 0.1,
    y = (yt(cy4) + yb(cy2)) / 2 - 0.1,
    label = "parameter\nrecovery",
    color = "#5C6BC0", size = 3.2, hjust = 0, vjust = 1,
    fontface = "italic", family = "Arial"
  ) +
  # NO closing arrow — the cycle is deliberately absent
  labs(title = "Computational Approach") +
  coord_cartesian(xlim = c(0, 10), ylim = c(-4, 10), expand = FALSE) +
  theme_void() +
  theme(
    plot.title  = element_text(
      face = "bold", size = 13, hjust = 0.5,
      margin = margin(b = 10), family = "Arial"
    ),
    plot.margin = margin(12, 12, 12, 16)
  )

# ── Combine and save ───────────────────────────────────────────────────────────
p_out <- p_left + p_right +
  plot_layout(ncol = 2) &
  theme(plot.background = element_rect(fill = "white", color = NA))

ggsave(
  here("figures", "manuscript", "Circularity_Diagram.png"),
  p_out, width = 12, height = 6, dpi = 300,
  device = ragg::agg_png
)

message("Saved Circularity_Diagram.png to figures/manuscript/")
