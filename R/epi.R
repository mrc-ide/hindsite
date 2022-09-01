#' Add mortality rate
#'
#' @param x output
#' @param scaler_pf Scaler for falciparum severe cases to deaths
#' @param scaler_pv Scaler for vivax severe cases to deaths. The reference for
#' this is Ahmed Rahimi, et al. (2014).
#' @param treatment_scaler Impact of treatment coverage on scaling
#'
#' @return output with mortality rates
#' @export
mortality_rate <- function(x,
                           scaler_pf = 0.215, scaler_pv  = 0.003,
                           treatment_scaler = 0.5){
  x |>
    dplyr::mutate(mortality_pf = (1 - (treatment_scaler * .data$tx_cov)) * scaler_pf * .data$severe_pf,
                  mortality_pv = (1 - (treatment_scaler * .data$tx_cov)) * scaler_pv * .data$severe_pv)
}

#' Add malaria cases
#'
#' @param x output
#'
#' @return output with cases
#' @export
cases <- function(x){
  x$cases_pf <- round(x$par_pf * x$clinical_pf)
  x$cases_pv <- round(x$par_pv * x$clinical_pv)
  return(x)
}

#' Add malaria severe cases
#'
#' @param x output
#'
#' @return output with severe cases
severe_cases <- function(x){
  x$severe_cases_pf <- round(x$par_pf * x$severe_pf)
  x$severe_cases_pv <- round(x$par_pv * x$severe_pv)
  return(x)
}

#' Add malaria deaths
#'
#' @param x output
#'
#' @return output with deaths
#' @export
deaths <- function(x){
  x$deaths_pf <- round(x$par_pf * x$mortality_pf)
  x$deaths_pv <- round(x$par_pv * x$mortality_pv)
  return(x)
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
  x$yll <- (life_expectancy - x$age_mid / 365) * (x$deaths_pf + x$deaths_pv)
  return(x)
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
      yld = (.data$cases_pf + .data$cases_pv)  * episode_length * .data$weight +
        (.data$severe_cases_pf + .data$severe_cases_pv) * severe_episode_length * severe_weight)
}

#' DALYs
#'
#' @param x output
#'
#' @return output with dalys
#' @export
dalys <- function(x){
  x$dalys <- round(x$yll + x$yld)
  return(x)
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

#' Converts modelled quantities of age-disaggregated cases and severe cases
#' to rates
#'
#' @param x Model output
#' @param modelled_population_size Modelled population size
#'
#' @return Output with rates
add_rates <- function(x, modelled_population_size){
  x |>
  dplyr::mutate(
    clinical_pf = .data$clinical_pf / (modelled_population_size * .data$prop),
    severe_pf = .data$severe_pf / (modelled_population_size * .data$prop),
    clinical_pv = .data$clinical_pv / (modelled_population_size * .data$prop),
    severe_pv = .data$severe_pv / (modelled_population_size * .data$prop))
}
