#' Add nets
#'
#' @param x output
#' @param iso3c Country ISO3c code
#'
#' @return output with net numbers
#' @export
add_nets <- function(x, iso3c){
  # Usage rate
  urd <- netz::get_usage_rate_data()
  if(iso3c %in% urd$iso3){
    ur <- urd[urd$iso3 == iso3c, "usage_rate"]
  } else {
    ur <- stats::median(urd$usage_rate)
  }
  ur_min <- stats::quantile(urd$usage_rate, 0.025)
  ur_max <- stats::quantile(urd$usage_rate, 0.975)

  # Retention half life
  hld <- netz::get_halflife_data()
  if(iso3c %in% hld$iso3){
    hl <- hld[hld$iso3 == iso3c, "half_life"]
  } else {
    hl <- stats::median(hld$half_life)
  }
  hl_min <- stats::quantile(hld$half_life, 0.025)
  hl_max <- stats::quantile(hld$half_life, 0.975)

  x |>
    dplyr::mutate(
      access = netz::usage_to_access(usage = .data$itn_use, use_rate = ur),
      access = ifelse(is.na(.data$access), 1, .data$access),
      access_lower = netz::usage_to_access(usage = .data$itn_use, use_rate = ur_max),
      access_lower = ifelse(is.na(.data$access_lower), 1, .data$access_lower),
      access_upper = netz::usage_to_access(usage = .data$itn_use, use_rate = ur_min),
      access_upper = ifelse(is.na(.data$access_upper), 1, .data$access_upper),
      crop = netz::access_to_crop(.data$access, type = "loess_extrapolate"),
      crop_lower = netz::access_to_crop(.data$access_lower, type = "loess_extrapolate"),
      crop_upper = netz::access_to_crop(.data$access_upper, type = "loess_extrapolate"),
      commodity_nets_distributed = round(netz::crop_to_distribution_dynamic(crop = .data$crop, net_loss_function = net_loss_map, half_life = hl) * .data$par),
      commodity_nets_distributed_lower = round(netz::crop_to_distribution_dynamic(crop = .data$crop_lower, net_loss_function = net_loss_map, half_life = hl_max) * .data$par),
      commodity_nets_distributed_upper = round(netz::crop_to_distribution_dynamic(crop = .data$crop_upper, net_loss_function = net_loss_map, half_life = hl_min) * .data$par)) |>
    dplyr::select(-c(.data$access_lower, .data$access_upper, .data$crop, .data$crop_lower, .data$crop_upper))
}

#' Add net costs
#'
#' @param x output
#'
#' @return output with net costs
#' @export
add_net_cost <- function(x){
  x |>
    dplyr::mutate(cost_itn = dplyr::case_when(
      net_type == "pyrethroid_only" ~ round(treasure::cost_llin(.data$commodity_nets_distributed)),
      net_type == "pyrethroid_pbo" ~ round(treasure::cost_pbo_itn(.data$commodity_nets_distributed)),
      # Assume these nets are 10% more costly than pyrethroid PBO
      net_type == "pyrethroid_pyrrole" ~ round(treasure::cost_pbo_itn(.data$commodity_nets_distributed, pbo_itn_unit_cost = 3.51 * 1.1))
    ))
}

#' IRS commodities
#'
#' @param x output
#'
#' @return output with number of people protected and number of households sprayed
#' @export
add_irs <- function(x){
  x |>
    dplyr::mutate(commodity_irs_people_protected = round(.data$irs_cov * .data$par),
                  commodity_irs_people_protected_lower = round(beta_coverage_ci(.data$irs_cov, 0.025, 9720) * .data$par),
                  commodity_irs_people_protected_upper = round(beta_coverage_ci(.data$irs_cov, 0.975, 9720) * .data$par),
                  commodity_irs_households_sprayed = round(.data$commodity_irs_people_protected / .data$hh_size),
                  commodity_irs_households_sprayed_lower = round(.data$commodity_irs_people_protected_lower / .data$hh_size),
                  commodity_irs_households_sprayed_upper = round(.data$commodity_irs_people_protected_upper / .data$hh_size))
}

#' IRS costs
#'
#' @param x output
#'
#' @return output with irs costs
#' @export
add_irs_cost <- function(x){
  x |>
    dplyr::mutate(cost_irs = round(treasure::cost_ll_irs_person(.data$commodity_irs_people_protected)))
}

#' Add PMC
#'
#' @param x output
#' @param modelled_population_size modelled population size
#' @param doses_per_child Number of PMC doses per child per year
#'
#' @return output with pmc doses and children protected
add_pmc <- function(x, modelled_population_size, doses_per_child = 3){
  x |>
    dplyr::mutate(commodity_pmc_doses = round((.data$n_pmc_treated / modelled_population_size) * .data$par_pf),
                  commodity_pmc_children_protected = round(.data$commodity_pmc_doses / doses_per_child))
}

#' Add PMC cost
#'
#' @param x output
#'
#' @return output with PMC costs
#' @export
add_pmc_cost <- function(x){
  x |>
    dplyr::mutate(cost_pmc = round(treasure::cost_ipti(n_doses = .data$commodity_pmc_doses)))
}

#' Add SMC
#'
#' @param x output
#' @param modelled_population_size modelled population size
#'
#' @return output with pmc doses and children protected
add_smc <- function(x, modelled_population_size){
  x |>
    dplyr::mutate(
      smc_dose_cov = .data$n_smc_treated / modelled_population_size,
      commodity_smc_doses = round(.data$smc_dose_cov * .data$par_pf),
      commodity_smc_doses_lower = round(beta_coverage_ci(.data$smc_dose_cov, 0.025, 1473) * .data$par_pf),
      commodity_smc_doses_upper = round(beta_coverage_ci(.data$smc_dose_cov, 0.975, 1473) * .data$par_pf),
      commodity_smc_children_protected = round(.data$commodity_smc_doses / .data$smc_n_rounds),
      commodity_smc_children_protected_lower = round(.data$commodity_smc_doses_lower / .data$smc_n_rounds),
      commodity_smc_children_protected_upper = round(.data$commodity_smc_doses_upper / .data$smc_n_rounds)
    ) |>
    dplyr::select(-.data$smc_dose_cov)
}

#' Add SMC cost
#'
#' @param x output
#'
#' @return output with SMC costs
#' @export
add_smc_cost <- function(x){
  x |>
    dplyr::mutate(cost_smc = round(treasure::cost_smc(n_doses = .data$commodity_smc_doses)))
}

#' Add RTS,S
#'
#' @param x output
#' @param modelled_population_size modelled population size
#' @param rtss_doses_per_child Number of doses per child
#'
#' @return output with rtss doses and rtss protected
add_rtss <- function(x, modelled_population_size, rtss_doses_per_child = 4){
  x |>
    dplyr::mutate(commodity_rtss_doses = round((.data$n_rtss_doses / modelled_population_size) * .data$par_pf),
                  commodity_rtss_children_protected = round(.data$commodity_rtss_doses / rtss_doses_per_child))
}

#' Add rtss cost
#'
#' @param x output
#'
#' @return output with rtss costs
#' @export
add_rtss_cost <- function(x){
  x |>
    dplyr::mutate(cost_rtss = round(treasure::cost_rtss(n_doses = .data$commodity_rtss_doses)))
}

