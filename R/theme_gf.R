#' @title Custom plot theme
#'
#' @export
theme_gf <- function() {
  theme(
    # add border 1)
    panel.border = element_rect(colour = "black", fill = NA, linetype = "solid"),

    # color background 2)
    panel.background = element_rect(fill = "white"),

    # modify grid 3)
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "darkgrey", linetype = "solid",
                                       linewidth = 0.5),
    panel.grid.minor.y = element_line(colour = "lightgrey", linetype = "solid",
                                      linewidth = 0.5),

    # modify text, axis and colour 4) and 5)
    axis.text = element_text(colour = "black", family = "Arial", size = 10),
    axis.title = element_text(colour = "black", face = "bold",
                              family = "Arial", size = 12),
    axis.ticks = element_line(colour = "darkgrey", linewidth = 0.5),

    # legend at the bottom 6)
    legend.position = "right",
    legend.key = element_rect(fill = 'white'),
    legend.text = element_text(colour = "black", family = "Arial", size = 10),
    legend.title = element_text(colour = "black", family = "Arial",
                                face = "bold", size = 12),

    text = element_text(colour = "black", family = "Arial")
  )
}


# theme_bw() +
#   theme(panel.grid.major.x = element_blank(),
#         panel.grid.minor.x = element_blank(),
#         legend.key = element_rect(fill = 'white'),
#         text = element_text(size = 15),
#         line = element_line(linewidth = 1),
#         axis.ticks = element_line(linewidth = 1))
