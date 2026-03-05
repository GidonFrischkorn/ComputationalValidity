#' @title Simulate Data for the M3 Complex Span Model
#'
#' @param n_sub A single numeric value specifying for how many subjects data should be simulated.
#' @param n_trials A single numeric value specifying how many trials should be simulated per subject.
#' @param par_limits A data.frame containing two variables: min & max with 3 values
#'   for each parameter of the M3 (c, a, f). Row names must match parameter names.
#'   If NULL, default parameter ranges are used.
#' @param n_options A named numeric vector specifying the number of response options per category.
#'   Defaults to c(n_corr = 1, n_distc = 1, n_other = 4, n_disto = 4, n_npl = 5).
#' @param verbose Numeric, controls printing progress. Default 0 (no output).
#' @param seed Optional random seed for reproducibility. If NULL, no seed is set.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{sub_parms}{A data.table with columns ID, c, a, f containing the generating parameters.}
#'     \item{sim_data}{A data.table with trial-level categorical response data
#'       (columns: ID, trialNum, response, task).}
#'   }
#'
#' @export
simulate_data_m3 <- function(n_sub, n_trials,
                              par_limits = NULL,
                              n_options = c(n_corr = 1, n_distc = 1, n_other = 4, n_disto = 4, n_npl = 5),
                              verbose = 0, seed = NULL) {

  if (!is.null(seed)) set.seed(seed)

  # Define M3 complex span model
  m3_model <- bmm::m3(
    resp_cats   = c("corr", "distc", "other", "disto", "npl"),
    num_options = n_options,
    version     = "cs",
    choice_rule = "softmax"
  )

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

  # Draw parameters from uniform distributions
  sub_parms <- data.table::data.table(
    ID = seq_len(n_sub),
    c = stats::runif(n_sub, lower["c"], upper["c"]),
    a = stats::runif(n_sub, lower["a"], upper["a"]),
    f = stats::runif(n_sub, lower["f"], upper["f"])
  )

  # Generate trial-level data for each participant
  data_list <- vector("list", n_sub)

  for (i in seq_len(n_sub)) {
    pars_i <- c(
      a = sub_parms$a[i],
      c = sub_parms$c[i],
      f = sub_parms$f[i]
    )

    # Generate aggregate counts for this participant
    sim_i <- bmm::rm3(
      n        = 1,
      size     = n_trials,
      pars     = pars_i,
      m3_model = m3_model
    )

    # Convert aggregate counts to trial-level categorical responses
    counts <- as.numeric(sim_i[1, c("corr", "distc", "other", "disto", "npl")])
    categories <- c("corr", "distc", "other", "disto", "npl")
    responses <- rep(categories, times = counts)

    # Shuffle to remove ordering artifacts
    responses <- sample(responses)

    data_list[[i]] <- data.table::data.table(
      ID       = i,
      trialNum = seq_len(n_trials),
      response = responses
    )

    if (verbose > 0 && i %% 10 == 0) {
      cat("  Simulated participant", i, "of", n_sub, "\n")
    }
  }

  sim_data <- data.table::rbindlist(data_list)
  sim_data$task <- 1L

  return(list(
    sub_parms = sub_parms,
    sim_data  = sim_data
  ))
}
