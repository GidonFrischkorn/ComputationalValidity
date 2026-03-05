#' @title Simulate Correlated Data from Two M3 Complex Span Tasks
#'
#' @param n_sub A single numeric value specifying for how many subjects data should be simulated.
#' @param n_trials A single numeric value specifying how many trials per task should be simulated.
#' @param correlation A named numeric vector of target correlations for the correlated parameters.
#' @param correlated_par A character string specifying which parameters are correlated.
#'   Options: "c", "a", "f", "c-a", "c-f", "a-f", "c-a-f".
#' @param par_limits A data.frame containing min & max for each M3 parameter. See \code{simulate_data_m3()}.
#' @param n_options A named numeric vector specifying the number of response options per category.
#' @param verbose Numeric, controls printing progress.
#' @param seed Optional random seed for reproducibility.
#'
#' @return A list with elements:
#'   \describe{
#'     \item{sub_parms}{A data.table with generating parameters for both tasks (columns: ID, c, a, f, task).}
#'     \item{sim_data}{A data.table with trial-level responses for both tasks.}
#'     \item{empirical_correlation}{Named numeric vector of true and empirical correlations.}
#'   }
#'
#' @export
simulate_correlation_m3 <- function(n_sub, n_trials, correlation, correlated_par,
                                     par_limits = NULL,
                                     n_options = c(n_corr = 1, n_distc = 1, n_other = 4, n_disto = 4, n_npl = 5),
                                     verbose = 0, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  # Set parameter limits
  if (is.null(par_limits)) {
    lower <- c(c = 0.5, a = 0.5, f = 0.1)
    upper <- c(c = 3.5, a = 3.0, f = 0.9)
  } else {
    varying_pars <- c("c", "a", "f")
    if (!all(varying_pars %in% rownames(par_limits))) {
      stop("par_limits must have rows named 'c', 'a', and 'f'.")
    }
    lower <- c(c = par_limits["c", "min"], a = par_limits["a", "min"], f = par_limits["f", "min"])
    upper <- c(c = par_limits["c", "max"], a = par_limits["a", "max"], f = par_limits["f", "max"])
  }

  # Generate independent parameters for both tasks
  sub_parms_task1 <- data.table::data.table(
    ID = seq_len(n_sub),
    c = stats::runif(n_sub, lower["c"], upper["c"]),
    a = stats::runif(n_sub, lower["a"], upper["a"]),
    f = stats::runif(n_sub, lower["f"], upper["f"])
  )

  sub_parms_task2 <- data.table::data.table(
    ID = seq_len(n_sub),
    c = stats::runif(n_sub, lower["c"], upper["c"]),
    a = stats::runif(n_sub, lower["a"], upper["a"]),
    f = stats::runif(n_sub, lower["f"], upper["f"])
  )

  # Apply correlations to specified parameters
  empirical_correlation <- c(correlation)

  if (grepl("c", correlated_par)) {
    sub_parms_task2$c <- simulate_correlated_vars(
      sub_parms_task1$c, correlation = correlation["c"],
      mean = mean(sub_parms_task1$c), sd = sd(sub_parms_task1$c),
      lb = lower["c"], ub = upper["c"]
    )
    observed_correlation <- cor(sub_parms_task1$c, sub_parms_task2$c)
    empirical_correlation <- append(empirical_correlation, observed_correlation)
  }

  if (grepl("a", correlated_par)) {
    sub_parms_task2$a <- simulate_correlated_vars(
      sub_parms_task1$a, correlation = correlation["a"],
      mean = mean(sub_parms_task1$a), sd = sd(sub_parms_task1$a),
      lb = lower["a"], ub = upper["a"]
    )
    observed_correlation <- cor(sub_parms_task1$a, sub_parms_task2$a)
    empirical_correlation <- append(empirical_correlation, observed_correlation)
  }

  if (grepl("f", correlated_par)) {
    sub_parms_task2$f <- simulate_correlated_vars(
      sub_parms_task1$f, correlation = correlation["f"],
      mean = mean(sub_parms_task1$f), sd = sd(sub_parms_task1$f),
      lb = lower["f"], ub = upper["f"]
    )
    observed_correlation <- cor(sub_parms_task1$f, sub_parms_task2$f)
    empirical_correlation <- append(empirical_correlation, observed_correlation)
  }

  # Name the correlations
  corr_par_names <- strsplit(correlated_par, "-")[[1]]
  corr_par_names1 <- paste0("true_corr.", corr_par_names)
  corr_par_names2 <- paste0("emp_corr.", corr_par_names)
  names(empirical_correlation) <- c(corr_par_names1, corr_par_names2)

  # Define M3 model for data generation
  m3_model <- bmm::m3(
    resp_cats   = c("corr", "distc", "other", "disto", "npl"),
    num_options = n_options,
    version     = "cs",
    choice_rule = "softmax"
  )

  # Generate data for both tasks
  generate_m3_data <- function(sub_parms, task_id) {
    data_list <- vector("list", nrow(sub_parms))
    for (i in seq_len(nrow(sub_parms))) {
      pars_i <- c(a = sub_parms$a[i], c = sub_parms$c[i], f = sub_parms$f[i])
      sim_i <- bmm::rm3(n = 1, size = n_trials, pars = pars_i, m3_model = m3_model)

      counts <- as.numeric(sim_i[1, c("corr", "distc", "other", "disto", "npl")])
      categories <- c("corr", "distc", "other", "disto", "npl")
      responses <- sample(rep(categories, times = counts))

      data_list[[i]] <- data.table::data.table(
        ID = sub_parms$ID[i], trialNum = seq_len(n_trials), response = responses
      )
    }
    dt <- data.table::rbindlist(data_list)
    dt$task <- task_id
    return(dt)
  }

  sim_data_task1 <- generate_m3_data(sub_parms_task1, task_id = 1L)
  sim_data_task2 <- generate_m3_data(sub_parms_task2, task_id = 2L)

  # Combine tasks
  sub_parms_task1$task <- 1L
  sub_parms_task2$task <- 2L

  return(list(
    sub_parms = data.table::rbindlist(list(sub_parms_task1, sub_parms_task2)),
    sim_data  = data.table::rbindlist(list(sim_data_task1, sim_data_task2)),
    empirical_correlation = empirical_correlation
  ))
}
