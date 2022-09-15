#' Add treatment commodities.
#'
#' Adds treatment commodities and uncertainty based of uncertainty in
#' treatment coverage, cases and severe cases.
#'
#' @param x output
#'
#' @return output with treatment commodities
add_tx <- function(x){
  x |>
    add_tx_internal() |>
    add_tx_internal(suffix = "_lower") |>
    add_tx_internal(suffix = "_upper")
}

#' Add treatment commodities
#'
#' @param x model output
#' @param suffix add "_lower" or "_upper" for uncertainty
#'
#' @return output with treatment commodities
add_tx_internal <- function(x, suffix = NULL){
  pf_cases_col <- paste0("cases", suffix, "_pf")
  pv_cases_col <- paste0("cases", suffix, "_pv")
  tx_cov_col <- paste0("tx_cov", suffix)

  tx_df <-
    data.frame(commodity_act_public = round(x[[pf_cases_col]] * x[[tx_cov_col]] * x[["prop_public"]] * x[["prop_act"]]),
               commodity_act_private = round(x[[pf_cases_col]] *  x[[tx_cov_col]] * (1 - x[["prop_public"]]) * x[["prop_act"]]),
               commodity_non_act_public = round(x[[pf_cases_col]] * x[[tx_cov_col]] * x[["prop_public"]] * (1 - x[["prop_act"]])),
               commodity_non_act_private = round(x[[pf_cases_col]] * x[[tx_cov_col]] * (1 - x[["prop_public"]]) * (1 - x[["prop_act"]])),
               commodity_primaquine_public = round(x[[pv_cases_col]] * x[[tx_cov_col]] * x[["prop_public"]]),
               commodity_primaquine_private = round(x[[pv_cases_col]] * x[[tx_cov_col]] * (1 - x[["prop_public"]])),
               commodity_rdt_public = round(x[[tx_cov_col]] * x[["prop_act"]] * x[[pf_cases_col]] * x[["prop_public"]]),
               commodity_rdt_private = round(x[[tx_cov_col]] * x[["prop_act"]] * x[[pf_cases_col]] * (1 - x[["prop_public"]])),
               commodity_microscopy_public = round(x[[tx_cov_col]] * (1 - x[["prop_act"]]) * x[[pf_cases_col]] * x[["prop_public"]]) +
                 round(x[[tx_cov_col]] * x[[pv_cases_col]] * x[["prop_public"]]),
               commodity_microscopy_private = round(x[[tx_cov_col]] * (1 - x[["prop_act"]]) * x[[pf_cases_col]] * (1 - x[["prop_public"]])) +
                 round(x[[tx_cov_col]] * x[[pv_cases_col]] * (1 - x[["prop_public"]])),
               commodity_nmf_rdt_public = ifelse(x[["pfpr_2_10"]] > 0.05,
                                                 round(x[["non_malarial_fevers"]] * x[[tx_cov_col]] * x[["prop_public"]] * x[["prop_act"]]),
                                                 0),
               commodity_nmf_rdt_private = ifelse(x[["pfpr_2_10"]] > 0.05,
                                                  round(x[["non_malarial_fevers"]] * x[[tx_cov_col]] * (1 - x[["prop_public"]]) * x[["prop_act"]]),
                                                  0)) |>
    dplyr::mutate(commodity_nmf_act_public = round(.data$commodity_nmf_rdt_public * x[["pfpr_2_10"]]),
           commodity_nmf_act_private = round(.data$commodity_nmf_rdt_private * x[["pfpr_2_10"]]))

  if(!is.null(suffix)){
    colnames(tx_df) <- paste0(colnames(tx_df), suffix)
  }

  x <- dplyr::bind_cols(x, tx_df)
  return(x)
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
      cost_act_public = round(treasure::cost_al(
        n_doses = (.data$commodity_act_public + .data$commodity_nmf_act_public) * .data$al_doses)),
      cost_act_private = round(treasure::cost_al(
        n_doses = (.data$commodity_act_private + .data$commodity_nmf_act_private) * .data$al_doses)),
      cost_non_act_public = round(treasure::cost_al(
        n_doses = .data$commodity_non_act_public * .data$al_doses)),
      cost_non_act_private = round(treasure::cost_al(
        n_doses = .data$commodity_non_act_private * .data$al_doses)),
      cost_primaquine_public = round(treasure::cost_primaquine(
        n_doses = .data$commodity_primaquine_public * .data$pq_doses)),
      cost_primaquine_private = round(treasure::cost_primaquine(
        n_doses = .data$commodity_primaquine_private * .data$pq_doses
      )),
      cost_rdt_public = round(treasure::cost_rdt(n_tests = .data$commodity_rdt_public + .data$commodity_nmf_rdt_public)),
      cost_rdt_private  = round(treasure::cost_rdt(n_tests = .data$commodity_rdt_private + .data$commodity_nmf_rdt_private)),
      cost_microscopy_public = round(treasure::cost_rdt(n_tests = .data$commodity_microscopy_public)),
      cost_microscopy_private = round(treasure::cost_rdt(n_tests = .data$commodity_microscopy_private))
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
                  commodity_inpatient_visits = round(.data$tx_cov * (.data$severe_cases_pf + .data$severe_cases_pv)),
                  commodity_outpatient_visits_lower = round(beta_coverage_ci(.data$tx_cov, q = 0.025, average_sample = 1473) *
                                                              (.data$cases_lower_pf + .data$cases_lower_pv) * .data$prop_public),
                  commodity_inpatient_visits_lower = round(beta_coverage_ci(.data$tx_cov, q = 0.025, average_sample = 1473) *
                                                             (.data$severe_cases_lower_pf + .data$severe_cases_lower_pv)),
                  commodity_outpatient_visits_upper = round(beta_coverage_ci(.data$tx_cov, q = 0.975, average_sample = 1473) *
                                                              (.data$cases_upper_pf + .data$cases_upper_pv) * .data$prop_public),
                  commodity_inpatient_visits_upper = round(beta_coverage_ci(.data$tx_cov, q = 0.975, average_sample = 1473) *
                                                             (.data$severe_cases_upper_pf + .data$severe_cases_upper_pv)))
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
