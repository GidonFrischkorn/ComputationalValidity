#' @keywords internal
#'
#' @details
#' The data-manipulation verbs used inside the package are imported explicitly
#' from tidytable and data.table so that they resolve through the namespace and
#' not through the search path. Without these imports, a package attached in
#' front of ComputationalValidity on the search path could mask them; SimDesign
#' does this for `select()` and `group_by()`, which broke the analysis
#' functions on SimDesign worker processes (2026-10-20).
#'
#' @importFrom tidytable across all_of arrange contains filter group_by
#'   left_join mutate n pivot_longer pivot_wider rowwise select starts_with
#'   summarize
#' @importFrom data.table := dcast melt rbindlist
"_PACKAGE"
