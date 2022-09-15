#' Age disaggregated
#'
#' @param pf pf output
#' @param pv pv output
#' @param iso3c iso3c code
#' @param population site population data
#'
#' @return Age disaggregated output
#' @export
get_age_disaggregated <- function(pf, pv, iso3c, population){
  dplyr::left_join(pf, pv) |>

    dplyr::mutate(prop = rowMeans(cbind(.data$prop_pf, .data$prop_pv), na.rm = TRUE)) |>
    dplyr::group_by(.data$year) |>
    dplyr::mutate(prop = .data$prop / sum(.data$prop)) |>
    dplyr::ungroup() |>

    dplyr::left_join(population) |>
    dplyr::mutate(pop = .data$pop * .data$prop,
           par = .data$par * .data$prop,
           par_pf = .data$par * .data$prop,
           par_pv = .data$par * .data$prop) |>

    non_malarial_fevers() |>
    add_tx() |>
    add_tx_costs() |>
    add_facility_visits() |>
    add_facility_costs(iso3c = iso3c)
}

#' Age aggregated output
#'
#' @param age_disaggregated age disaggregated output
#' @param iso3c COutnry iso3c code
#' @param interventions site interventions
#' @param scenario Scenario name
#' @param run Run name
#' @param model_population Modelled population size
#'
#' @return Age aggregated output
#' @export
get_age_aggregated <- function(age_disaggregated, iso3c, interventions, scenario, run, model_population){
  age_disaggregated |>
    aggregate_age() |>
    link_interventions(interventions = interventions) |>
    add_irs() |>
    add_irs_cost() |>
    add_pmc(modelled_population_size = model_population) |>
    add_pmc_cost() |>
    add_smc(modelled_population_size = model_population) |>
    add_smc_cost() |>
    add_rtss(modelled_population_size = model_population) |>
    add_rtss_cost() |>
    add_nets(iso3c = iso3c) |>
    add_net_cost() |>
    add_surveillance_cost() |>
    add_case_detection_cost() |>
    dplyr::mutate(scenario = scenario,
                  run = run)
}

#' Pf output
#'
#' @param model_output model output
#' @param interventions site interventions
#' @param population site population
#' @param model_population Modelled population size
#'
#' @return pf output
#' @export
get_pf <- function(model_output, interventions, population, model_population){
  tx_interventions <- interventions |>
    dplyr::select(.data$year, .data$tx_cov, .data$prop_public, .data$prop_act)

  par <- population |>
    dplyr::select(.data$year, .data$par_pf) |>
    dplyr::rename(par = .data$par_pf)

  pf <- model_output |>
    add_year() |>
    add_pf_columns() |>
    add_pfpr() |>
    add_rtss_total() |>
    wrangle_pf() |>
    rates(modelled_population = model_population) |>
    link_pop(par) |>
    link_tx(tx_interventions) |>
    age_mid_point() |>
    mortality_rate(scaler = 0.215) |>
    cases() |>
    severe_cases() |>
    deaths() |>
    dplyr::select(-.data$par) |>
    yll(life_expectancy = 60) |>
    yld() |>
    dalys() |>
    rename_cols(suffix = "pf")

  return(pf)
}

#' Pv output
#'
#' @param model_output model output
#' @param interventions site interventions
#' @param population site population
#' @param model_population Modelled population size
#'
#' @return pv output
#' @export
get_pv <- function(model_output, interventions, population, model_population){
  tx_interventions <- interventions |>
    dplyr::select(.data$year, .data$tx_cov, .data$prop_public, .data$prop_act)

  par <- population |>
    dplyr::select(.data$year, .data$par_pv) |>
    dplyr::rename(par = .data$par_pv)

  pv <- model_output |>
    add_year() |>
    add_pvpr() |>
    wrangle_pv() |>
    rates(modelled_population = model_population) |>
    link_pop(par) |>
    link_tx(tx_interventions) |>
    age_mid_point() |>
    mortality_rate(scaler = 0.003) |>
    cases() |>
    severe_cases() |>
    deaths() |>
    dplyr::select(-.data$par) |>
    yll(life_expectancy = 60) |>
    yld() |>
    dalys() |>
    rename_cols(suffix = "pv")

  return(pv)
}
