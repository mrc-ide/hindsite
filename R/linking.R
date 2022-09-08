link_interventions <- function(x, modelled_interventions, input_interventions){
  input_interventions <- input_interventions |>
    dplyr::select(-c(.data$itn_input_dist,
                     .data$pyrethroid_resistance, .data$bioassay_mortality,
                     .data$dn0, .data$rn0, .data$rnm,
                     .data$irs_insecticide, .data$irs_spray_rounds,
                     .data$ls_theta, .data$ls_gamma, .data$ks_theta,
                     .data$ks_gamma, .data$ms_theta, .data$ms_gamma))

  x  |>
    dplyr::left_join(input_interventions, by = "year") |>
    dplyr::left_join(modelled_interventions, by = "year")
}

link_tx_interventions <- function(x, input_interventions){
  input_interventions <- input_interventions |>
    dplyr::select(.data$year, .data$tx_cov, .data$prop_act, .data$prop_public)

  x  |>
    dplyr::left_join(input_interventions, by = "year")
}

link_prevalence <- function(x, prevalence){
  x |>
    dplyr::left_join(prevalence, by = "year")
}

link_ages <- function(x, ages){
  x |>
    dplyr::left_join(ages, by = c("year", "age_l", "age_u"))
}

link_pop <- function(x, population){
  population <- population |>
    dplyr::select(.data$year, .data$pop, .data$par, .data$par_pf, .data$par_pv)

  x |>
    dplyr::left_join(population, by = "year") |>
    dplyr::mutate(
      pop = round(.data$pop * .data$prop),
      par = round(.data$par * .data$prop),
      par_pf = round(.data$par_pf * .data$prop),
      par_pv = round(.data$par_pv * .data$prop)) |>
    dplyr::select(-.data$prop)
}
