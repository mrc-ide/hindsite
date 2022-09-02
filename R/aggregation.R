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
      dplyr::across(dplyr::contains("par"), sum),
      pop = sum(.data$pop),
      dplyr::across(dplyr::contains("cases"), sum),
      dplyr::across(dplyr::contains("severe"), sum),
      dplyr::across(dplyr::contains("deaths"), sum),
      yll = sum(.data$yll),
      yld = sum(.data$yld),
      dalys = sum(.data$dalys),
      dplyr::across(dplyr::contains("commodity"), sum),
      dplyr::across(dplyr::contains("cost"), sum)
    ) |>
    dplyr::ungroup()
}