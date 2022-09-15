#' Add plasmodium vivax prevalence
#'
#' @param x output
#'
#' @return output with pvpr_1_99
add_pvpr <- function(x){
  x |>
    dplyr::mutate(pvpr_1_99 = .data$n_detect_365_36499 / .data$n_365_36499)
}

#' Wrangle pv model output
#'
#' Collapses output to annual and converts to long wrt age disaggregation
#'
#' @param x model output
#'
#' @return Long, age-disaggregated pv data
wrangle_pv <- function(x){
  x |>
    dplyr::select(.data$year, .data$pvpr_1_99,
                  dplyr::contains("n_inc"), dplyr::contains("n_sev"), dplyr::contains("n_age")) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(pvpr_1_99 = round(mean(.data$pvpr_1_99), 5),
              dplyr::across(dplyr::contains("n_inc"), sum),
              dplyr::across(dplyr::contains("n_age"), mean)) |>
    dplyr::ungroup() |>
    tidyr::pivot_longer(-c(.data$year, .data$pvpr_1_99),
                        names_to = "x", values_to = "y") |>
    dplyr::mutate(x = stringr::str_replace(.data$x, "n_age", "n"),
                  x = stringr::str_replace(.data$x, "n_inc_clinical", "inc"),
                  x = stringr::str_replace(.data$x, "n_inc_severe", "sev")) |>
    tidyr::separate(x, into = c("x", "age_l", "age_u"), sep = "_", convert = TRUE) |>
    tidyr::pivot_wider(names_from = .data$x, values_from = .data$y)
}

#' Empty pv output
#'
#' To be used when pv eir = 0
#'
#' @param timesteps Model timesteps
#' @param baseline_year Baseline year
#'
#' @return Empty output
empty_pv <- function(timesteps, baseline_year = 2000){
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
    dplyr::left_join(
      data.frame(age_l = c(0, 1825, 5475),
                 age_u = c(1824, 5474, 36499)),
      by = character())
  empty[paste0(to_add, "_pv")] <- 0
  empty$prop_pv <- NA
  empty$pvpr_1_99 <- 0

  return(empty)
}
