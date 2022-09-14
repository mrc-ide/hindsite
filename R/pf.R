add_pf_columns <- function(x,
                           col = c("n_rtss_epi_dose_1", "n_rtss_epi_dose_2",
                                   "n_rtss_epi_dose_3", "n_rtss_epi_booster_1",
                                   "n_pmc_treated", "n_smc_treated")){

  add <- col[!col %in% names(x)]
  if(length(add) != 0){
    x[add] <- 0
  }

  return(x)
}

add_pfpr <- function(x){
  x |>
    dplyr::mutate(pfpr_2_10 = .data$n_detect_730_3649 / .data$n_730_3649)
}

add_rtss_total <- function(x){
  x |>
    dplyr::mutate(n_rtss_doses = .data$n_rtss_epi_dose_1 + .data$n_rtss_epi_dose_2 +
                    .data$n_rtss_epi_dose_3 + .data$n_rtss_epi_booster_1)
}

wrangle_pf <- function(x){
  x |>
    dplyr::select(year, pfpr_2_10, .data$n_rtss_doses, .data$n_smc_treated, .data$n_pmc_treated,
                  dplyr::contains("n_inc"), dplyr::contains("n_sev"), dplyr::contains("n_age")) |>
    dplyr::group_by(.data$year) |>
    summarise(pfpr_2_10 = round(mean(.data$pfpr_2_10), 5),
              across(contains("n_inc"), sum),
              across(contains("n_age"), mean),
              n_rtss_doses = sum(.data$n_rtss_doses),
              n_smc_treated = sum(.data$n_smc_treated),
              n_pmc_treated = sum(.data$n_pmc_treated)) |>
    ungroup() |>
    tidyr::pivot_longer(-c(.data$year, .data$pfpr_2_10, .data$n_rtss_doses,
                           .data$n_smc_treated, .data$n_pmc_treated),
                        names_to = "x", values_to = "y") |>
    dplyr::mutate(x = stringr::str_replace(.data$x, "n_age", "n"),
                  x = stringr::str_replace(.data$x, "n_inc_clinical", "inc"),
                  x = stringr::str_replace(.data$x, "n_inc_severe", "sev")) |>
    tidyr::separate(x, into = c("x", "age_l", "age_u"), sep = "_", convert = TRUE) |>
    tidyr::pivot_wider(names_from = .data$x, values_from = .data$y)
}

empty_pf <- function(timesteps, baseline_year = 2000){
  to_add <- c(
    "cases", "cases_lower", "cases_upper",
    "severe_cases", "severe_cases_lower", "severe_cases_upper",
    "deaths", "deaths_lower", "deaths_upper",
    "yll", "yll_lower", "yll_upper",
    "yld", "yld_lower", "yld_upper",
    "dalys", "dalys_lower", "dalys_upper")
  empty <-
    tidyr::tibble(
      year = baseline_year:(baseline_year -1 + (timesteps / 365))
    ) |>
    left_join(
      data.frame(age_l = c(0, 1825, 5475),
                 age_u = c(1824, 5474, 36499)),
      by = character())
  empty[paste0(to_add, "_pf")] <- 0
  empty$prop_pf <- NA
  empty$pfpr_2_10 <- 0
  empty$n_rtss_doses <- 0
  empty$n_smc_treated  <- 0
  empty$n_pmc_treated  <- 0

  return(empty)
}
