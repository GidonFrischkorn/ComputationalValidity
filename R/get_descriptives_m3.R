#' @title Compute Behavioral Indicators from M3 Categorical Response Data
#'
#' @param data A data.table with columns ID, trialNum, response, task.
#'   The response column contains one of: "corr", "distc", "other", "disto", "npl".
#' @param format Character, either "long" (default) or "wide".
#'
#' @return A data.table in long format with columns ID, task, measure, indicator, value.
#'
#' @export
get_descriptives_m3 <- function(data, format = "long") {

  # Compute response proportions per participant and task
  agg_data <- data[, .(
    corr  = mean(response == "corr"),
    other = mean(response == "other"),
    distc = mean(response == "distc"),
    disto = mean(response == "disto"),
    npl   = mean(response == "npl")
  ), by = .(ID, task)]

  # Add composite indicators
  agg_data[, `:=`(
    accuracy       = corr,
    intrusion_mem  = other,
    intrusion_dist = distc + disto,
    intrusion_total = other + distc + disto,
    npl_rate       = npl
  )]

  if (format == "long") {
    # Pivot to long format matching get_descriptives() output structure
    proportion_cols <- c("corr", "other", "distc", "disto", "npl")
    composite_cols  <- c("accuracy", "intrusion_mem", "intrusion_dist",
                         "intrusion_total", "npl_rate")

    dt_prop <- data.table::melt(
      agg_data, id.vars = c("ID", "task"),
      measure.vars = proportion_cols,
      variable.name = "indicator", value.name = "value"
    )
    dt_prop[, measure := "proportion"]

    dt_comp <- data.table::melt(
      agg_data, id.vars = c("ID", "task"),
      measure.vars = composite_cols,
      variable.name = "indicator", value.name = "value"
    )
    dt_comp[, measure := "composite"]

    agg_data_long <- data.table::rbindlist(list(dt_prop, dt_comp))
    agg_data_long <- agg_data_long[, .(ID, task, measure, indicator, value)]

    return(agg_data_long)
  } else {
    return(agg_data)
  }
}
