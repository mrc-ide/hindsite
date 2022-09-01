#' Wrangle raw age-group output
#'
#' @param x model output
#' @param population_size Modelled population size
format_age <- function(x, population_size){
  x |>
    dplyr::select(.data$year, dplyr::contains("n_age")) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), mean)) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(-.data$year, names_to = "x", values_to = "y") |>
    dplyr::mutate(x = stringr::str_replace(x, "n_age", "prop")) |>
    tidyr::separate(x, into = c("x", "age_l", "age_u"), sep = "_", convert = TRUE) |>
    tidyr::pivot_wider(id_cols = c(.data$year, .data$age_l, .data$age_u),
                       names_from = .data$x, values_from = .data$y) |>
    dplyr::mutate(prop = .data$prop / population_size,
                  age_mid = exp_mid(.data$age_l, .data$age_u)) |>
    dplyr::select(.data$year, .data$age_l, .data$age_u, .data$age_mid, .data$prop)
}

#' Wrangle raw epi output
#'
#' @param x Model output
#' @param species Species "pf" or "pv"
format_epi <- function(x, species){
  x |>
    dplyr::select(.data$year, dplyr::contains("n_inc"), dplyr::contains("n_severe")) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), sum)) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(-.data$year, names_to = "x", values_to = "y") |>
    dplyr::mutate(x = stringr::str_replace(.data$x, "n_inc_", "")) |>
    tidyr::separate(.data$x, into = c("x", "age_l", "age_u"), sep = "_", convert = TRUE) |>
    dplyr::mutate(x = paste0(.data$x, "_", species)) |>
    tidyr::pivot_wider(id_cols = c(.data$year, .data$age_l, .data$age_u),
                       names_from = .data$x, values_from = .data$y)

}

#' Wrangle raw prevalence output
#'
#' @param x Model output
#' @param species Species "pf" or "pv"
format_prev <- function(x, species){
  if(species == "pf"){
    prev <- x |>
      dplyr::mutate(pfpr_2_10 = .data$n_detect_730_3649 / .data$n_730_3649) |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(pfpr_2_10 = mean(.data$pfpr_2_10)) |>
      dplyr::ungroup()
  }
  if(species == "pv"){
    prev <- x |>
      dplyr::mutate(pvpr_1_99 = .data$n_detect_365_36499 / .data$n_365_36499) |>
      dplyr::group_by(.data$year) |>
      dplyr::summarise(pvpr_1_99 = mean(.data$pvpr_1_99)) |>
      dplyr::ungroup()
  }
  return(prev)
}

#' Wrangle raw intervention output
#'
#' @param x Model output
format_interventions <- function(x){
  x |>
    dplyr::mutate(n_rtss_doses = .data$n_rtss_epi_dose_1 + .data$n_rtss_epi_dose_2 +
                    .data$n_rtss_epi_dose_3 + .data$n_rtss_epi_booster_1) |>
    dplyr::select(.data$year, .data$n_rtss_doses, .data$n_smc_treated, .data$n_pmc_treated) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(dplyr::across(dplyr::everything(), sum)) |>
    dplyr::ungroup()
}

#' Add missing columns
#'
#' Ensure that the minimum set of required columns is present, even if a simulation
#' run isn't performed (i.e. when there is no transmission of a species in a
#' location, or no implementation of an intervention in a location).
#'
#' @param x malariasimulation output
#' @param species species code. Can be falicparum "pf" or vivax "pf".
#'
#' @return data.frame with all required columns.
add_missing_columns <- function(x, species = "pf"){
  if(species == "pf"){
    required <- c("n_detect_730_3649", "n_730_3649",
                  "n_inc_clinical_0_1824", "n_inc_clinical_1825_5474",  "n_inc_clinical_5475_36499",
                  "n_inc_severe_0_1824",  "n_inc_severe_1825_5474", "n_inc_severe_5475_36499",
                  "n_age_0_1824", "n_age_1825_5474", "n_age_5475_36499",
                  "n_rtss_epi_dose_1", "n_rtss_epi_dose_2", "n_rtss_epi_dose_3", "n_rtss_epi_booster_1",
                  "n_pmc_treated", "n_smc_treated")
  }
  if(species == "pv"){
    required <- c("n_detect_365_36499", "n_365_36499",
                  "n_inc_clinical_0_1824", "n_inc_clinical_1825_5474",  "n_inc_clinical_5475_36499",
                  "n_inc_severe_0_1824",  "n_inc_severe_1825_5474", "n_inc_severe_5475_36499")
  }

  missing <- setdiff(required, colnames(x))
  x[,missing] <- 0
  return(x)
}

#' Add year column based on model timestep and baseline year
#'
#' @param x model output
#' @param baseline_year start year
#'
#' @return output with year column
add_year <- function(x, baseline_year = 2000){
  x |>
    dplyr::mutate(year = floor((.data$timestep - 1) / 365) + baseline_year)
}

#' Approximate mean age within an age bracket assuming exponential
#' age distribution
#'
#' @param l age bracket min
#' @param u age bracket max
#' @param average_age Average age in days
#'
#' @return output with average age column
exp_mid <- function(l, u, average_age =  19 * 365){
  rate <- 1 / average_age
  out <- purrr::map2_dbl(l, u, function(l, u, rate){
    stats::weighted.mean(l:u, c(stats::pexp(l:u, rate, lower.tail = FALSE)))
  }, rate = rate)
  return(round(out))
}
