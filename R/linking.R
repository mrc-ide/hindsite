#' Link PAR and adjust for age proportions
#'
#' @param x output
#' @param pop pop data frame (cols: year and par)
#'
#' @return output with par
#' @export
link_pop <- function(x, pop){
  x |>
    dplyr::left_join(pop, by = "year") |>
    dplyr::mutate(par = round(.data$par * .data$prop))
}

#' Link treatment coverage and add coverage uncertainty
#'
#' @param x output
#' @param tx treatment data.frame (cols: year, tx_cov)
#'
#' @return output with treatment coverage
link_tx <- function(x, tx){
  x |>
    dplyr::left_join(tx, by = "year") |>
    dplyr::mutate(tx_cov_lower = beta_coverage_ci(.data$tx_cov, q = 0.025, average_sample = 1473),
                  tx_cov_upper = beta_coverage_ci(.data$tx_cov, q = 0.975, average_sample = 1473))
}

#' Link intervention inputs
#'
#' @param x output
#' @param interventions interventions
#'
#' @return output linked to model intervention inputs
link_interventions <- function(x, interventions){
  interventions <- interventions |>
    dplyr::select(.data$iso3c,
                  dplyr::contains("name"),
                  .data$urban_rural,
                  .data$year,
                  .data$itn_use,
                  .data$net_type,
                  dplyr::contains("irs"),
                  .data$hh_size,
                  dplyr::contains("smc"),
                  .data$rtss_cov,
                  dplyr::contains("pmc"))
  x |>
    dplyr::left_join(interventions, by = "year")
}
