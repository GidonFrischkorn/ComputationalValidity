get_correlations <- function (data, reliabilites) {
  df_correlation <- data %>%
    pivot_wider(names_from = "task",
                values_from = "value",
                names_glue = "task{task}") %>%
    summarize(correlation = cor(task1,task2),
              .by = c(measure,indicator))

  reliability_wide <- reliabilites %>%
    pivot_wider(names_from = "task",
                values_from = "reliability",
                names_glue = "rel_task{task}")

  df_correlation <- df_correlation %>%
    left_join(reliability_wide) %>%
    mutate(correlation_corrected = correlation / sqrt(rel_task1 * rel_task2)) %>%
    select(measure, indicator, correlation, correlation_corrected)

  return(df_correlation)
}
