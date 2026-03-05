#' @title Compute Split-Half Reliability for M3 Indicators
#'
#' @param data A data.table with columns ID, trialNum, response, task.
#'   The response column contains one of: "corr", "distc", "other", "disto", "npl".
#'
#' @return A data.frame with columns task, measure, indicator, reliability
#'   (Spearman-Brown corrected split-half reliability).
#'
#' @export
get_reliability_m3 <- function(data) {

  # Split into odd and even trials
  data[, OddEven := ifelse(trialNum %% 2 == 0, "even", "odd")]

  # Compute indicators for each half
  half_data <- data[, .(
    corr  = mean(response == "corr"),
    other = mean(response == "other"),
    distc = mean(response == "distc"),
    disto = mean(response == "disto"),
    npl   = mean(response == "npl")
  ), by = .(ID, task, OddEven)]

  # Add composites
  half_data[, `:=`(
    accuracy        = corr,
    intrusion_mem   = other,
    intrusion_dist  = distc + disto,
    intrusion_total = other + distc + disto,
    npl_rate        = npl
  )]

  # Compute reliability for each indicator
  all_indicators <- c("corr", "other", "distc", "disto", "npl",
                       "accuracy", "intrusion_mem", "intrusion_dist",
                       "intrusion_total", "npl_rate")
  measures <- c(rep("proportion", 5), rep("composite", 5))

  rel_list <- vector("list", length(all_indicators))

  for (j in seq_along(all_indicators)) {
    ind <- all_indicators[j]

    # Pivot to wide: odd vs even columns
    wide_dt <- data.table::dcast(
      half_data, ID + task ~ OddEven, value.var = ind
    )

    # Spearman-Brown corrected split-half
    rel_by_task <- wide_dt[, {
      r <- stats::cor(odd, even, use = "complete.obs")
      .(reliability = (2 * r) / (1 + r))
    }, by = task]

    rel_by_task[, `:=`(measure = measures[j], indicator = ind)]
    rel_list[[j]] <- rel_by_task
  }

  reliability <- data.table::rbindlist(rel_list)
  reliability <- reliability[, .(task, measure, indicator, reliability)]

  # Clean up temporary column
  data[, OddEven := NULL]

  return(as.data.frame(reliability))
}
