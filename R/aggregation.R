#' Aggregate over age groups
#'
#' @param x output
#'
#' @return aggregated output
#' @export
aggregate_age <- function(x){
  x |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      pfpr_2_10 = ifelse(sum(.data$par_pf) > 0, stats::weighted.mean(.data$pfpr_2_10, .data$par_pf), 0),
      pvpr_1_99 = ifelse(sum(.data$par_pv) > 0, stats::weighted.mean(.data$pvpr_1_99, .data$par_pv), 0),
      dplyr::across(dplyr::contains("par"), sum),
      pop = sum(.data$pop),
      dplyr::across(dplyr::contains("cases"), sum),
      dplyr::across(dplyr::contains("severe"), sum),
      dplyr::across(dplyr::contains("deaths"), sum),
      dplyr::across(dplyr::contains("yll"), sum),
      dplyr::across(dplyr::contains("yld"), sum),
      dplyr::across(dplyr::contains("dalys"), sum),
      dplyr::across(dplyr::contains("n_smc"), mean),
      dplyr::across(dplyr::contains("n_rtss"), mean),
      dplyr::across(dplyr::contains("n_pmc"), mean),
      dplyr::across(dplyr::contains("commodity"), sum),
      dplyr::across(dplyr::contains("cost"), sum)
    ) |>
    dplyr::ungroup()
}
