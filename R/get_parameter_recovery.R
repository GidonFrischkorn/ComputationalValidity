#' @export
get_parameter_recovery <- function(rec_pars, gen_pars, df_reliability, id_var = "ID", format = "long") {
  par_names <- colnames(gen_pars)
  par_names <- par_names[-which(par_names == "ID")]

  df_recovery <- rec_pars %>%
    left_join(gen_pars, by = id_var) %>%
    group_by(measure, indicator) %>%
    summarize(N = n(),
              across(all_of(par_names), ~ cor(.,value), .names = "rec-{.col}"),
              .groups = "drop") %>%
    rowwise() %>%
    mutate(across(starts_with("rec-"), ~ psychometric::CIr(.,n = N)[1], .names = "LCL-{.col}")) %>%
    mutate(across(starts_with("rec-"), ~ psychometric::CIr(.,n = N)[2], .names = "UCL-{.col}"))
  colnames(df_recovery) <- stringr::str_replace_all(colnames(df_recovery),"-rec-","-")

  if(format == "long") {
    df_recovery_long <- df_recovery %>%
      pivot_longer(cols = starts_with(c("rec-","LCL-","UCL-")),
                   names_to = "recoveredPar",
                   values_to = "recovery") %>%
      mutate(
        recovPar = stringr::str_split_i(recoveredPar,pattern = "-",1),
        genPar = stringr::str_split_i(recoveredPar,pattern = "-",2)
      ) %>%
      select(-recoveredPar) %>%
      pivot_wider(id_cols = c(measure,indicator,N,genPar),
                  values_from = recovery,
                  names_from = recovPar) %>%
      select(genPar,measure,indicator,N,rec,UCL,LCL) %>%
      arrange(genPar,abs(rec)) %>%
      left_join(df_reliability, by = c("measure", "indicator")) %>%
      mutate(rec_corrected = rec / sqrt(1 * reliability),
             UCL_corrected = UCL / sqrt(1 * reliability),
             LCL_corrected = LCL / sqrt(1 * reliability)) %>%
      select(-reliability)

    return(df_recovery_long)
  } else {
    return(df_recovery)
  }
}

