#' Surveillance cost
#'
#' @param x output
#'
#' @return output with surveillance costs
#' @export
add_surveillance_cost <- function(x){
  x |>
    dplyr::mutate(cost_surveillance = treasure::cost_surveillance(pop_at_risk = .data$par))
}

#' Proactive case detection cost
#'
#' @param x output
#'
#' @return output with case detection costs
#' @export
add_case_detection_cost <- function(x){
  x |>
    dplyr::mutate(
      pf_api = ifelse(.data$par_pf > 0, .data$cases_pf / .data$par_pf * 1000, 0),
      pv_api = ifelse(.data$par_pv > 0, .data$cases_pv / .data$par_pv * 1000, 0),
      cost_proactive_case_detection =
                    ifelse(pf_api < 1 & pv_api < 1,
                           treasure::cost_pacd(n_tested = .data$cases_pf + .data$cases_pv),
                           0)) |>
    dplyr::select(-.data$pf_api, -.data$pv_api)
}
