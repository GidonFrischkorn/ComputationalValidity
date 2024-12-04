#' @export
get_reliability <- function(data) {
  reliability_pc <- data %>%
    mutate(trialNum = 1:n(), .by = c(ID, Cond,task)) %>%
    mutate(OddEven = ifelse(trialNum %% 2 == 0, "even","odd")) %>%
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
    mutate(trialNum = 1:n(), .by = c(ID, Cond,task)) %>%
    mutate(OddEven = ifelse(trialNum %% 2 == 0, "even","odd")) %>%
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
    mutate(trialNum = 1:n(), .by = c(ID,task,Cond)) %>%
    mutate(OddEven = ifelse(trialNum %% 2 == 0, "even","odd")) %>%
    summarize(v = ez_dm(RT = RT, ACC = 1 - Error, robust = TRUE)["v"],
              a = ez_dm(RT = RT, ACC =  1 - Error, robust = TRUE)["a"],
              t0 = ez_dm(RT = RT, ACC =  1 - Error, robust = TRUE)["t0"],
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
