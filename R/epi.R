#' Add mortality rate
#'
#' @param x output
#' @param scaler Scaler for severe cases to deaths. Have used
#' original fitted scaler  of 0.215 for falciparum. For vivax can use a scaler
#' of 0.003 (Ahmed Rahimi, et al. (2014)).
#' @param treatment_scaler Impact of treatment coverage on scaling.
#'
#' @return output with mortality rates
#' @export
mortality_rate <- function(x,
                           scaler,
                           treatment_scaler = 0.5){
  x |>
    dplyr::mutate(mort = (1 - (treatment_scaler * .data$tx_cov)) * scaler * .data$sev)
}

#' Add malaria cases
#'
#' @param x output
#'
#' @return output with cases
#' @export
cases <- function(x){
  x |>
    dplyr::mutate(
      cases = round(.data$par * .data$inc),
      cases_lower = epi_uncertainty_approximation(.data$cases, 0.025, 0.227),
      cases_upper = epi_uncertainty_approximation(.data$cases, 0.975, 0.227)
    ) |>
    dplyr::select(-.data$inc)
}

#' Add malaria severe cases
#'
#' @param x output
#'
#' @return output with severe cases
#' @export
 severe_cases <- function(x){
  x |>
    dplyr::mutate(
      severe_cases = round(.data$par * .data$sev),
      severe_cases_lower = epi_uncertainty_approximation(.data$severe_cases, 0.025, 0.265),
      severe_cases_upper = epi_uncertainty_approximation(.data$severe_cases, 0.975, 0.265)
    ) |>
    dplyr::select(-.data$sev)
}

#' Add malaria deaths
#'
#' @param x output
#'
#' @return output with deaths
#' @export
deaths <- function(x){
  x |>
    dplyr::mutate(
      deaths = round(.data$par * .data$mort),
      deaths_lower = epi_uncertainty_approximation(.data$deaths, 0.025, 0.265),
      deaths_upper = epi_uncertainty_approximation(.data$deaths, 0.975, 0.265)
    ) |>
    dplyr::select(-.data$mort)
}

#' Years of life lost
#'
#' An approximation of years of life lost (within discretised age groups)
#'
#' @param x output
#' @param life_expectancy Life expectancy
#'
#' @return output with yll
#' @export
yll <- function(x, life_expectancy){
  x |>
    dplyr::mutate(
      yll = round((life_expectancy - .data$age_mid / 365) * .data$deaths),
      yll_lower = round((life_expectancy - .data$age_mid / 365) * .data$deaths),
      yll_upper = round((life_expectancy - .data$age_mid / 365) * .data$deaths)
    )
}

#' Years of life with disability
#'
#' Weights from \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4772264/}{Gunda _et al_, 2016}.
#' Currently using pf values.
#'
#' @param x output
#' @param episode_length Average length of clinical episode
#' @param severe_episode_length Average length of severe episode
#' @param weight1 Disability weight age group 1
#' @param weight2 Disability weight age group 2
#' @param weight3 Disability weight age group 3
#' @param severe_weight Disability weight severe malaria
#'
#' @return output with yld
#' @export
yld <- function(x, episode_length = 0.01375, severe_episode_length = 0.04795,
                weight1 = 0.211, weight2 = 0.195, weight3 = 0.172,
                severe_weight = 0.6){
  x |>
    dplyr::mutate(
      weight = dplyr::case_when(
        .data$age_l  == 0 ~ weight1,
        .data$age_l  == 1825 ~ weight2,
        .data$age_l == 5475 ~ weight3
      ),
      yld = round(.data$cases  * episode_length * .data$weight +
                    .data$severe_cases * severe_episode_length * severe_weight),
      yld_lower = round(.data$cases_lower  * episode_length * .data$weight +
                          .data$severe_cases_lower * severe_episode_length * severe_weight),
      yld_upper = round(.data$cases_upper  * episode_length * .data$weight +
                          .data$severe_cases_upper * severe_episode_length * severe_weight)) |>
    dplyr::select(-.data$weight)
}

#' DALYs
#'
#' @param x output
#'
#' @return output with dalys
#' @export
dalys <- function(x){
  x  |>
    dplyr::mutate(
      dalys = round(.data$yll + .data$yld),
      dalys_lower = round(.data$yll_lower + .data$yld_lower),
      dalys_upper = round(.data$yll_upper + .data$yld_upper)
    )
}

#' non_malarial_fevers
#'
#' Assume a constant rate of NMFs in under 5s and over 5s. This follows methodology
#' in \href{https://gh.bmj.com/content/2/2/e000176}{Patouillard _et al_, 2017}.
#'
#' @param x output
#' @param rate_under_5 Annual incidence of NMFs in children under 5
#' @param rate_over_5 Annual incidence of NMFs in individuals over 5
#'
#' @return output with nmfs
#' @export
non_malarial_fevers <- function(x, rate_under_5 = 3.4, rate_over_5 = 1){
  x |>
    dplyr::mutate(non_malarial_fevers = round(ifelse(.data$age_l == 0, rate_under_5 * .data$par, rate_over_5 * .data$par)))
}

#' Scale outputs for modelled population size
#'
#' @param x Model outputs
#' @param modelled_population modelled population size
#'
#' @return output with prop (age), inc and severe inc
#' @export
rates <- function(x, modelled_population){
  x |>
    dplyr::mutate(prop = .data$n / modelled_population,
                  inc = .data$inc / .data$n,
                  sev = .data$sev / .data$n) |>
    dplyr::select(-.data$n)
}

#' Add average age in range (assuming exponential demography)
#'
#' @param x output
#'
#' @return output with age average
#' @export
age_mid_point <- function(x){
  x |>
    dplyr::mutate(age_mid = exp_mid(.data$age_l, .data$age_u))
}
