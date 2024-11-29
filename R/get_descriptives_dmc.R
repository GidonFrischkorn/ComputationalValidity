#' @title Analyse Data for
#'
#' @param data A single numeric value specifying for how many subjects data should be simulated.
#' @param format A single numeric value specifying how many trials per condition should be simulated.
#'
#'
#' @export
get_descriptives_dmc <- function(data, format = "long") {
  agg_data_pc <- data %>%
    summarize(PC = 1 - mean(Error), .by = c(ID,Cond,task))

  agg_data_RT <- data %>%
    filter(Error == 0) %>%
    summarize(RT = mean(RT), .by = c(ID,Cond,task))

  agg_data <- data.table::merge.data.table(agg_data_pc,agg_data_RT) %>%
    pivot_wider(names_from = "Cond",
                values_from = c("PC","RT")) %>%
    mutate(RT_diff = RT_incomp - RT_comp,
           PC_diff = PC_comp - PC_incomp,
           PC_mean = (PC_comp + PC_incomp) / 2,
           RT_mean = (RT_comp + RT_incomp) / 2) %>%
    select(ID,task,
           RT_incomp, RT_comp, RT_mean, RT_diff,
           PC_incomp, PC_comp, PC_mean, PC_diff)

  if (format == "long") {
    agg_data_long <- agg_data %>%
      pivot_longer(cols = contains("_"),
                   names_to = "indicator") %>%
      mutate(measure = stringr::str_split_i(indicator,"_",1),
             indicator = stringr::str_split_i(indicator,"_",2)) %>%
      select(ID, task, measure, indicator, value)
    return(agg_data_long)
  } else {
    return(agg_data)
  }
}
