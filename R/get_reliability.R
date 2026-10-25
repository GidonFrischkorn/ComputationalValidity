#' @title Split-half reliability of behavioral indicators and EZ-DM parameters
#'
#' @description
#' Computes Spearman-Brown corrected split-half reliabilities for the mean,
#' condition-wise, and difference scores of accuracy (PC), correct-response RT,
#' and EZ-diffusion parameters (v, a, t0), separately per task.
#'
#' @details
#' Trials are assigned to the two halves by a random, balanced split within each
#' participant, task, and condition. Simulated trials have no temporal order, so
#' a random split is the odd-even split of a randomly ordered trial sequence.
#' An earlier version split on row order; because `dRiftDM::simulate_data()`
#' lists correct trials before error trials, that split placed the same number
#' of errors in both halves by construction and overestimated the accuracy
#' reliabilities (fixed 2026-10-20; see the response to Psych Review R1,
#' decision D10).
#'
#' @param data A data.frame of trial-level data with columns `ID`, `task`,
#'   `Cond` (comp/incomp), `RT`, and `Error` (0/1).
#'
#' @return A data.frame with columns `task`, `indicator`, `reliability`, `measure`.
#'
#' @export
get_reliability <- function(data) {
  # one random balanced split per participant x task x condition, shared by all
  # indicators so that PC, RT and EZ-DM reliabilities use the same halves
  data <- data %>%
    mutate(OddEven = sample(rep_len(c("odd", "even"), n())),
           .by = c(ID, task, Cond))

  reliability_pc <- data %>%
    summarize(PC = 1 - mean(Error), .by = c(ID,task,Cond,OddEven)) %>%
    pivot_wider(names_from = Cond,
                values_from = PC) %>%
    mutate(mean = (comp + incomp)/2,
           diff = comp- incomp) %>%
    pivot_longer(cols = c(comp, incomp, mean, diff),
                 names_to = "indicator") %>%
    pivot_wider(names_from = OddEven,
                values_from = value) %>%
    summarize(reliability = (2*cor(odd,even, use = "complete.obs"))/(1 + cor(odd,even, use = "complete.obs")), .by = c(task,indicator)) %>%
    mutate(measure = "PC")

  reliability_RT <- data %>%
    filter(Error == 0) %>%
    summarize(RT = mean(RT), .by = c(ID,task,Cond,OddEven)) %>%
    pivot_wider(names_from = Cond,
                values_from = RT) %>%
    mutate(mean = (comp + incomp)/2,
           diff = comp- incomp) %>%
    pivot_longer(cols = c(comp, incomp, mean, diff),
                 names_to = "indicator") %>%
    pivot_wider(names_from = OddEven,
                values_from = value) %>%
    summarize(reliability = (2*cor(odd,even, use = "complete.obs"))/(1 + cor(odd,even, use = "complete.obs")), .by = c(task,indicator)) %>%
    mutate(measure = "RT")

  reliability_ezDM <- data %>%
    summarize(
      v = ez_dm(RT = RT, ACC = 1 - Error)["v"],
      a = ez_dm(RT = RT, ACC = 1 - Error)["a"],
      t0 = ez_dm(RT = RT, ACC = 1 - Error)["t0"],
      .by = c(ID,Cond,OddEven,task)) %>%
    pivot_longer(cols = c("v","a","t0"),
                 names_to = "measure") %>%
    pivot_wider(names_from = c("Cond"),
                values_from = c("value")) %>%
    mutate(mean = (comp + incomp)/2,
           diff = comp - incomp) %>%
    pivot_longer(cols = c("comp","incomp","mean","diff"),
                 names_to = "indicator") %>%
    pivot_wider(names_from = OddEven,
                values_from = value) %>%
    summarize(reliability = (2*cor(odd,even, use = "complete.obs"))/(1 + cor(odd,even, use = "complete.obs")), .by = c(task,indicator, measure))

  reliability <- rbind(reliability_pc, reliability_RT, reliability_ezDM)
  return(reliability)
}
