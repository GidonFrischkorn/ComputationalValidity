#' @title Analyse Data for
#'
#' @param data A single numeric value specifying for how many subjects data should be simulated.
#' @param format A single numeric value specifying how many trials per condition should be simulated.
#'
#'
#' @export
get_ezDM <- function(data, format = "long") {
  # calculate ezDM parameters for each condition
  agg_data_ezDM <- data %>%
    summarize(v = ez_dm(RT = RT, ACC = 1 - Error, robust = TRUE)["v"],
              a = ez_dm(RT = RT, ACC =  1 - Error, robust = TRUE)["a"],
              t0 = ez_dm(RT = RT, ACC =  1 - Error, robust = TRUE)["t0"],
              .by = c(ID,Cond,task)) %>%
    pivot_longer(cols = c("v","a","t0"),
                 names_to = "measure") %>%
    pivot_wider(names_from = c("Cond"),
                values_from = c("value")) %>%
    mutate(mean = (comp + incomp)/2,
           diff = comp - incomp) %>%
    pivot_longer(cols = c("comp","incomp","mean","diff"),
                 names_to = "indicator")

  if (format == "long") {
    # return long format data
    return(agg_data_ezDM)
  } else {
    # reshape to wide and return wide format data
    agg_data_ezDM_wide <- agg_data_ezDM %>%
      pivot_wider(names_from = c("measure","indicator"),
                  values_from = "value")
    return(agg_data_ezDM_wide)
  }
}
