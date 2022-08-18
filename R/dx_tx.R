#' Treatment and diagnostic commodities
#'
#' @param x output
#'
#' @return output with treatment and diagnostic commodities
#' @export
add_tx <- function(x){
  x |>
    dplyr::mutate(commodity_act_public = round(.data$cases_pf * .data$tx_cov * .data$prop_public * .data$prop_act),
           commodity_act_private = round(.data$cases_pf *  .data$tx_cov * (1 - .data$prop_public) * .data$prop_act),
           commodity_non_act_public = round(.data$cases_pf * .data$tx_cov * .data$prop_public * (1 - .data$prop_act)),
           commodity_non_act_private = round(.data$cases_pf * .data$tx_cov * (1 - .data$prop_public) * (1 - .data$prop_act)),
           commodity_primaquine_public = round(.data$cases_pv * .data$tx_cov * .data$prop_public),
           commodity_primaquine_private = round(.data$cases_pv * .data$tx_cov * (1 - .data$prop_public)),
           commodity_rdt_public = round(.data$tx_cov * .data$prop_act * .data$cases_pf * .data$prop_public),
           commodity_rdt_private = round(.data$tx_cov * .data$prop_act * .data$cases_pf * (1 - .data$prop_public)),
           commodity_microscopy_public = round(.data$tx_cov * (1 - .data$prop_act) * .data$cases_pf * .data$prop_public) +
             round(.data$tx_cov * .data$cases_pv * .data$prop_public),
           commodity_microscopy_private = round(.data$tx_cov * (1 - .data$prop_act) * .data$cases_pf * (1 - .data$prop_public)) +
             round(.data$tx_cov * .data$cases_pv * (1 - .data$prop_public)),
           commodity_nmf_rdt_public = ifelse(.data$pfpr_2_10 > 0.05,
                                             round(.data$non_malarial_fevers * .data$tx_cov * .data$prop_public * .data$prop_act),
                                             0),
           commodity_nmf_rdt_private = ifelse(.data$pfpr_2_10 > 0.05,
                                              round(.data$non_malarial_fevers * .data$tx_cov * (1 - .data$prop_public) * .data$prop_act),
                                              0),
           commodity_nmf_act_public = round(.data$commodity_nmf_rdt_public * .data$pfpr_2_10),
           commodity_nmf_act_private = round(.data$commodity_nmf_rdt_private * .data$pfpr_2_10))
}

#' Treatment costs
#'
#' @param x output
#'
#' @return output with treatment costs
#' @export
add_tx_costs <- function(x){
  x |>
    dplyr::mutate(
      al_doses = dplyr::case_when(
        .data$age_l == 0 ~ 3 * 2 * 1,
        .data$age_l  == 1825 ~ 3 * 2 * 2.5,
        .data$age_l == 5475 ~ 3 * 2 * 4
      ),
      pq_doses = dplyr::case_when(
        .data$age_l == 0 ~ 14 * 0.25 * 7.5 / 7.5,
        .data$age_l == 1825 ~  14 * 0.25 * 25 / 7.5,
        .data$age_l == 5475 ~ 14 * 0.25 * 50 / 7.5
      ),
      cost_act_public = treasure::cost_al(
        n_doses = (.data$commodity_act_public + .data$commodity_nmf_act_public) * .data$al_doses),
      cost_act_private = treasure::cost_al(
        n_doses = (.data$commodity_act_private + .data$commodity_nmf_act_private) * .data$al_doses),
      cost_non_act_public = treasure::cost_al(
        n_doses = .data$commodity_non_act_public * .data$al_doses),
      cost_non_act_private = treasure::cost_al(
        n_doses = .data$commodity_non_act_private * .data$al_doses),
      cost_primaquine_public = treasure::cost_primaquine(
        n_doses = .data$commodity_primaquine_public * .data$pq_doses),
      cost_primaquine_private = treasure::cost_primaquine(
        n_doses = .data$commodity_primaquine_private * .data$pq_doses
      ),
      cost_rdt_public = treasure::cost_rdt(n_tests = .data$commodity_rdt_public + .data$commodity_nmf_rdt_public),
      cost_rdt_private  = treasure::cost_rdt(n_tests = .data$commodity_rdt_private + .data$commodity_nmf_rdt_private),
      cost_microscopy_public = treasure::cost_rdt(n_tests = .data$commodity_microscopy_public),
      cost_microscopy_private = treasure::cost_rdt(n_tests = .data$commodity_microscopy_private)
    ) |>
    dplyr::select(-c(.data$al_doses, .data$pq_doses))
}

#' Number of facility visits.
#'
#' Assumed some non-severe are in the private sector and all severe visits in public sector
#'
#' @param x output
#'
#' @return output with facility visits
#' @export
add_facility_visits <- function(x){
  x |>
    dplyr::mutate(commodity_outpatient_visits = round(.data$tx_cov * (.data$cases_pf + .data$cases_pv) * .data$prop_public),
           commodity_inpatient_visits = round(.data$tx_cov * (.data$severe_cases_pf + .data$severe_cases_pv)))
}

#' Facility costs
#'
#' @param x output
#' @param iso3c Country iso3c code
#'
#' @return output with facility costs
#' @export
add_facility_costs <- function(x, iso3c){
  if(iso3c %in% treasure::who_choice$ISO){
    facility <- dplyr::filter(treasure::who_choice, .data$ISO == iso3c)
    inpatient_cost <- facility$inpatient
    outpatient_cost <- facility$outpatient
  } else{
    inpatient_cost <- stats::median(treasure::who_choice$inpatient)
    outpatient_cost <- stats::median(treasure::who_choice$outpatient)
  }

  x |>
    dplyr::mutate(cost_outpatient = round(.data$commodity_outpatient_visits * outpatient_cost),
           cost_inpatient = round(.data$commodity_inpatient_visits * inpatient_cost))
}
